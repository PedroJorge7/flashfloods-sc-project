# Shared establishment-level helpers: panel construction, model fitting, and
# LaTeX table/figure output. Treatment: 0-5 km vs. 50-80 km control, SEs
# clustered by census tract. treat_B is fixed once per establishment from its
# 2007 baseline (see build_establishment_panel() below).

library(dplyr)
library(tidyr)
library(tibble)
library(haven)
library(fixest)
library(broom)
library(ggplot2)
library(ggpubr)
library(survival)

# -----------------------------------------------------------------------------
# Logging
# -----------------------------------------------------------------------------
log_lines <- character(0)

log_msg <- function(...) {
  msg <- sprintf(...)
  stamped <- sprintf("[%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg)
  message(stamped)
  log_lines <<- c(log_lines, stamped)
}

log_warning <- function(context, w) {
  log_msg("WARNING in %s: %s", context, conditionMessage(w))
}

# -----------------------------------------------------------------------------
# make_numeric_id() — coerces a labelled/character ID column to numeric,
# falling back to a factor-code mapping if direct coercion loses too many
# values (e.g. alphanumeric tract codes).
# -----------------------------------------------------------------------------
make_numeric_id <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x_chr <- as.character(haven::zap_labels(x))
  x_chr[x_chr %in% c("", "NA")] <- NA_character_
  x_num <- suppressWarnings(as.numeric(x_chr))
  if (sum(!is.na(x_chr)) > 0 && mean(!is.na(x_chr) & is.na(x_num)) > 0.2)
    x_num <- as.numeric(factor(x_chr))
  x_num
}

# -----------------------------------------------------------------------------
# build_establishment_panel() — general-purpose panel builder. Treatment is
# fixed once per establishment from its 2007 (or earliest available)
# distance band; outcomes (morte, new_firm, mover_ano_mun,
# reloc_tract_tminus1) are never nulled based on a later year's distance, so
# relocated establishments stay in the panel.
# -----------------------------------------------------------------------------
build_establishment_panel <- function(raw_data, treated_rule, label,
                                       control_rule = function(d) dplyr::between(d, 50, 80),
                                       year_min = 2003, year_max = 2012,
                                       dummy_years = NULL) {

  log_msg("Building establishment panel [CORRECTED, full life-of-firm]: %s (years %d-%d)", label, year_min, year_max)

  if (is.null(dummy_years)) dummy_years <- year_min:year_max

  has_new_firm <- "new_firm" %in% names(raw_data)

  data <- raw_data %>%
    mutate(
      morte         = as.numeric(haven::zap_labels(morte)),
      mover_ano_mun = as.numeric(haven::zap_labels(mover_ano_mun)),
      new_firm      = if (has_new_firm) as.numeric(haven::zap_labels(new_firm)) else NA_real_,
      dist_flood_band = case_when(
        treated_rule(dist_flood) ~ 1,
        control_rule(dist_flood) ~ 0,
        TRUE                     ~ NA_real_
      )
    ) %>%
    arrange(id_estab, year)

  # Fixed baseline classification: prefer each establishment's 2007 record;
  # fall back to its earliest available year with a classifiable dist_flood.
  baseline <- data %>%
    filter(!is.na(dist_flood_band)) %>%
    arrange(id_estab, dplyr::desc(year == 2007), year) %>%
    group_by(id_estab) %>%
    slice(1) %>%
    ungroup() %>%
    transmute(id_estab, treat_B_baseline = dist_flood_band)

  data <- data %>%
    left_join(baseline, by = "id_estab") %>%
    mutate(treat_B = treat_B_baseline) %>%
    filter(year >= year_min & year <= year_max) %>%
    mutate(
      treat_trend    = if_else(year >= 2008, 1, 0),
      treat_trend_f  = factor(treat_trend),
      code_tract_num = make_numeric_id(code_tract)
    ) %>%
    arrange(id_estab, year) %>%
    group_by(id_estab) %>%
    mutate(
      reloc_tract_tminus1 = as.numeric(
        !is.na(dplyr::lead(code_tract_num)) & dplyr::lead(code_tract_num) != code_tract_num
      ),
      reloc_tract_tminus1 = if_else(year == min(year), 0, reloc_tract_tminus1),
      reloc_tract_tminus1 = if_else(is.na(morte), NA_real_, reloc_tract_tminus1)
    ) %>%
    ungroup()

  for (y in dummy_years) {
    v <- paste0("treat_B_", y)
    data[[v]] <- dplyr::case_when(
      data$year == y & !is.na(data$treat_B) ~ data$treat_B,
      !is.na(data$treat_B) & data$year != y ~ 0,
      TRUE                                  ~ NA_real_
    )
  }

  data <- data %>%
    mutate(
      treat_B_agg = case_when(
        year >= 2008 & treat_B == 1 ~ 1,
        !is.na(treat_B)             ~ 0,
        TRUE                        ~ NA_real_
      )
    )

  n_treated <- data %>% filter(treat_B == 1) %>% distinct(id_estab) %>% nrow()
  n_control <- data %>% filter(treat_B == 0) %>% distinct(id_estab) %>% nrow()
  log_msg("  %s: %d treated establishments, %d control establishments", label, n_treated, n_control)

  data
}

# -----------------------------------------------------------------------------
# Panel caching: avoid rebuilding a multi-minute panel after a downstream
# script error. cache_path is typically file.path(cache_dir, "<name>.rds").
# -----------------------------------------------------------------------------
build_or_load_panel <- function(cache_path, build_fn, force_rebuild = FALSE) {
  if (!force_rebuild && file.exists(cache_path)) {
    log_msg("Loading cached panel: %s", cache_path)
    return(readRDS(cache_path))
  }
  panel <- build_fn()
  saveRDS(panel, cache_path)
  log_msg("Cached panel saved: %s", cache_path)
  panel
}

# -----------------------------------------------------------------------------
# fit_establishment_models() — fits the three standard specs (aggregated,
# time-varying, event-study), tract-clustered by default. Tolerates a
# NULL/degenerate outcome without halting the whole run.
# -----------------------------------------------------------------------------
fit_establishment_models <- function(data, outcome, cluster_var = ~code_tract_num,
                                      fe_extra = "treat_trend[code_tract_num]",
                                      fe_extra_event = "treat_trend_f[code_tract_num]",
                                      event_lead_years = 2003:2006,
                                      event_lag_years = 2008:2012,
                                      tv_years = 2008:2012,
                                      context = "") {

  est_sample <- data %>%
    filter(!is.na(treat_B), !is.na(code_tract_num), !is.na(.data[[outcome]]))
  expected_nobs   <- nrow(est_sample)
  expected_nclust <- length(unique(est_sample$code_tract_num))
  outcome_vals <- unique(est_sample[[outcome]]); outcome_vals <- outcome_vals[!is.na(outcome_vals)]

  safe_feols <- function(fml, dat, tag) {
    if (length(outcome_vals) < 2) {
      log_msg("  SKIPPED (%s / %s): outcome '%s' has no variation (n_distinct=%d)",
              context, tag, outcome, length(outcome_vals))
      return(NULL)
    }
    tryCatch(
      withCallingHandlers(
        feols(fml, data = dat, cluster = cluster_var, fixef.rm = "none", lean = TRUE),
        warning = function(w) { log_warning(sprintf("%s / %s", context, tag), w); invokeRestart("muffleWarning") }
      ),
      error = function(e) { log_msg("  ERROR (%s / %s): %s", context, tag, conditionMessage(e)); NULL }
    )
  }
  check_nobs <- function(m, tag) {
    if (is.null(m)) return(NA_integer_)
    got <- nobs(m)
    if (!is.na(got) && got != expected_nobs)
      log_msg("  NOTE (%s / %s): fixest nobs=%d differs from pre-filtered sample=%d", context, tag, got, expected_nobs)
    got
  }

  fe_agg_str   <- paste0("id_estab + year", if (nzchar(fe_extra)) paste0(" + ", fe_extra) else "")
  fe_event_str <- paste0("id_estab + year", if (nzchar(fe_extra_event)) paste0(" + ", fe_extra_event) else "")

  agg <- safe_feols(as.formula(paste0(outcome, " ~ treat_B_agg | ", fe_agg_str)), data, "aggregated")
  nobs_agg <- check_nobs(agg, "aggregated")

  tv_vars <- paste0("treat_B_", tv_years, collapse = " + ")
  timevar <- safe_feols(as.formula(paste0(outcome, " ~ ", tv_vars, " | ", fe_agg_str)), data, "time-varying")
  nobs_timevar <- check_nobs(timevar, "time-varying")

  es_years <- c(event_lead_years, event_lag_years)
  es_vars  <- paste0("treat_B_", es_years, collapse = " + ")
  event <- safe_feols(as.formula(paste0(outcome, " ~ ", es_vars, " | ", fe_event_str)), data, "event-study")
  nobs_event <- check_nobs(event, "event-study")

  list(
    agg = agg, timevar = timevar, event = event,
    nobs = c(agg = nobs_agg, timevar = nobs_timevar, event = nobs_event),
    nclust = c(agg = expected_nclust, timevar = expected_nclust, event = expected_nclust),
    tv_years = tv_years
  )
}

# -----------------------------------------------------------------------------
# Formatting helpers
# -----------------------------------------------------------------------------
star_fun <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***"); if (p < 0.05) return("**"); if (p < 0.10) return("*"); ""
}
fmt_coef <- function(est, p) if (is.na(est)) "n/a" else sprintf("%.5f%s", est, star_fun(p))
fmt_se   <- function(se) if (is.na(se)) "" else sprintf("(%.5f)", se)
fmt_obs  <- function(n) if (is.na(n)) "n/a" else gsub(",", "{,}", format(n, big.mark = ",", scientific = FALSE), fixed = TRUE)

get_term <- function(model, term) {
  if (is.null(model)) return(data.frame(estimate = NA_real_, std.error = NA_real_, p.value = NA_real_))
  tt  <- broom::tidy(model)
  out <- tt[tt$term == term, c("estimate", "std.error", "p.value")]
  if (nrow(out) == 0) return(data.frame(estimate = NA_real_, std.error = NA_real_, p.value = NA_real_))
  out
}

event_study_frame <- function(model, baseline_year = 2007) {
  if (is.null(model)) return(NULL)
  tt <- broom::tidy(model, conf.int = TRUE)
  tt <- tt[grepl("^treat_B_", tt$term), ]
  out <- data.frame(
    year = as.numeric(gsub("treat_B_", "", tt$term)),
    estimate = tt$estimate, ci_low = tt$conf.low, ci_high = tt$conf.high
  )
  out <- rbind(out, data.frame(year = baseline_year, estimate = 0, ci_low = 0, ci_high = 0))
  out[order(out$year), ]
}

make_event_study_plot <- function(df, title_label, baseline_year = 2007) {
  ggplot(df, aes(x = year, y = estimate)) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "grey90", alpha = 0.5) +
    geom_line(aes(y = ci_high), color = "grey30", linewidth = 0.1) +
    geom_line(aes(y = ci_low),  color = "grey30", linewidth = 0.1) +
    geom_point(size = 2, color = "firebrick") +
    geom_line(color = "firebrick", linewidth = 1) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = baseline_year + 0.5, linetype = "dashed") +
    scale_x_continuous(breaks = sort(unique(df$year))) +
    labs(x = "Year", y = "Coefficient", title = title_label) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# -----------------------------------------------------------------------------
# Sector / size subgroup helpers (Table 4, Table 5)
# -----------------------------------------------------------------------------
add_sector_dummies <- function(data) {
  data %>% mutate(
    Construcao = as.integer(subs_ibge == 15),
    Transporte = as.integer(subs_ibge == 20),
    Industria  = as.integer(dplyr::between(subs_ibge, 3, 13)),
    Comercio   = as.integer(subs_ibge %in% c(16, 17)),
    Servicos   = as.integer(subs_ibge == 21)
  )
}

add_size_bins <- function(data) {
  data %>% mutate(
    size_estab1 = as.integer(empregados < 10),
    size_estab2 = as.integer(dplyr::between(empregados, 10, 49)),
    size_estab3 = as.integer(empregados >= 50)
  )
}

# -----------------------------------------------------------------------------
# Baseline-control construction (Table C.3): take each establishment's value
# at 2007, or the latest available year <= 2007, else 0.
# -----------------------------------------------------------------------------
make_baseline_value <- function(df, varname) {
  df %>%
    group_by(id_estab) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
      .val_2007 = .data[[varname]][year == 2007][1],
      .val_pre2007 = { v <- .data[[varname]][year <= 2007]; if (length(v) == 0) NA else dplyr::last(na.omit(v)) }
    ) %>%
    mutate(baseline = dplyr::coalesce(.val_2007, .val_pre2007, 0)) %>%
    ungroup() %>%
    pull(baseline)
}

# -----------------------------------------------------------------------------
# Like make_baseline_value(), but falls back to the earliest available year
# instead of 0 when there's no record at or before 2007.
# -----------------------------------------------------------------------------
make_baseline_or_earliest_value <- function(df, varname) {
  df %>%
    group_by(id_estab) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
      .val_2007     = .data[[varname]][year == 2007][1],
      .val_pre2007  = { v <- .data[[varname]][year <= 2007]; if (length(v) == 0) NA else dplyr::last(na.omit(v)) },
      .val_earliest = dplyr::first(na.omit(.data[[varname]]))
    ) %>%
    mutate(baseline = dplyr::coalesce(.val_2007, .val_pre2007, .val_earliest)) %>%
    ungroup() %>%
    pull(baseline)
}

# -----------------------------------------------------------------------------
# ECP (public-calamity) municipality lists (not used by Main Estimates, kept
# for parity with the main utils file).
# -----------------------------------------------------------------------------
ecp_municipalities_generic <- c(420220, 420240, 420290, 420320, 420590, 420710, 420820,
                                 420845, 421150, 421320, 421470, 421510, 421820)
ecp_municipalities_2011    <- c(420030, 420190, 420290, 420850, 420950, 420990,
                                421400, 421460, 421480, 421780, 421860)

# -----------------------------------------------------------------------------
# make_first_year_value(): each establishment's value of `varname` at its
# first observed year in the data (used for Table 5's baseline controls).
# -----------------------------------------------------------------------------
make_first_year_value <- function(df, varname) {
  df %>%
    group_by(id_estab) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(.first_val = dplyr::first(.data[[varname]])) %>%
    ungroup() %>%
    pull(.first_val)
}

# -----------------------------------------------------------------------------
# emit_closure_relocation_table() / emit_closure_relocation_event_study()
# Shared 2-outcome (Closure, Relocation) table/figure builder, used by the
# Appendix C robustness scripts.
# -----------------------------------------------------------------------------
emit_closure_relocation_table <- function(res_closure, res_reloc, out_path, caption, label, notes_extra = "", notes_full = NULL) {

  agg_models <- list(res_closure$agg, res_reloc$agg)
  tv_models  <- list(res_closure$timevar, res_reloc$timevar)
  col_names  <- c("Closure", "Relocation")
  n_col <- 2

  row_gap <- function(label_txt, vals) {
    inter <- as.vector(rbind(vals, rep("", length(vals))))
    paste0(label_txt, " & ", paste(inter, collapse = " & "), " \\\\")
  }

  post_est <- lapply(agg_models, get_term, term = "treat_B_agg")
  coefA <- sapply(post_est, function(x) fmt_coef(x$estimate, x$p.value))
  seA   <- sapply(post_est, function(x) fmt_se(x$std.error))

  nobsB <- c(res_closure$nobs["timevar"], res_reloc$nobs["timevar"])

  lines <- c(
    "\\begin{table}[htb]", "  \\centering",
    sprintf("  \\tabcaption{%s}", caption), sprintf("  \\label{%s}", label),
    "  \\scalebox{0.85}{", "  \\begin{threeparttable}",
    paste0("    \\begin{tabular}{l", strrep("c", 2 * n_col), "}"),
    "    \\toprule",
    paste0("          & (1) &  & (2) &  \\\\"),
    "\\cmidrule(lr){2-2}\\cmidrule(lr){4-4}",
    row_gap("    Dep. Var:", col_names),
    "    \\midrule",
    "    \\multicolumn{5}{l}{\\textbf{Panel A: Time-Aggregated DiD}}\\\\",
    row_gap("    Flash Flood Post", coefA),
    row_gap("                     ", seA),
    "    \\midrule",
    "    \\multicolumn{5}{l}{\\textbf{Panel B: Time-Varying DiD}}\\\\"
  )

  tv_years <- res_closure$tv_years
  if (is.null(tv_years)) tv_years <- 2008:2012
  for (yr in tv_years) {
    term <- paste0("treat_B_", yr)
    est  <- lapply(tv_models, get_term, term = term)
    coefB <- sapply(est, function(x) fmt_coef(x$estimate, x$p.value))
    seB   <- sapply(est, function(x) fmt_se(x$std.error))
    lines <- c(lines, row_gap(sprintf("    Flash Flood %d", yr), coefB), row_gap("                     ", seB))
  }

  note_text <- if (!is.null(notes_full)) {
    notes_full
  } else {
    paste0("0-5 km treatment vs. 50-80 km control, standard errors clustered at the census-tract level. ",
           "Panel A: aggregated post-treatment effect. Panel B: year-specific effects 2008-2012. ",
           "Establishment and year FE and a census-tract linear trend in all models. ", notes_extra,
           " *** p $<$ 0.01, ** p $<$ 0.05, * p $<$ 0.1.")
  }

  lines <- c(
    lines, "    \\midrule",
    row_gap("    Observations", sapply(nobsB, fmt_obs)),
    "    \\bottomrule", "    \\bottomrule", "    \\end{tabular}%",
    "    \\begin{tablenotes}[flushleft]",
    paste0("    \\item \\small \\textit{Notes:} ", note_text),
    "    \\end{tablenotes}", "  \\end{threeparttable}", "  }", "\\end{table}%"
  )

  writeLines(lines, out_path)
  log_msg("Saved table: %s", out_path)
}

emit_closure_relocation_event_study <- function(res_closure, res_reloc, out_path, subtitle = "") {
  df_c <- event_study_frame(res_closure$event)
  df_r <- event_study_frame(res_reloc$event)
  if (is.null(df_c) || is.null(df_r)) {
    log_msg("SKIPPED event-study figure (missing model): %s", out_path)
    return(invisible(NULL))
  }
  pA <- make_event_study_plot(df_c, paste0("A - Establishment Closure (0/1)", if (nzchar(subtitle)) paste0("\n", subtitle) else ""))
  pB <- make_event_study_plot(df_r, paste0("B - Establishment Relocation (0/1)", if (nzchar(subtitle)) paste0("\n", subtitle) else ""))
  fig <- ggpubr::ggarrange(pA, pB, ncol = 2)
  ggsave(filename = out_path, plot = fig, dpi = 300, width = 12, height = 6, units = "in")
  log_msg("Saved figure: %s", out_path)
}
