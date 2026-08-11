# ============================================================================
# Appendix C, Table C.5 — Establishment-Level
# Estimates Using Alternative Inference Procedures
# Treatment: 0-5 km vs. 50-80 km control. Full specification (establishment
# FE, year FE, census-tract trend) throughout. Compares Conley spatial SEs
# (10/20 km) and a wild cluster bootstrap (Rademacher weights, clustered at
# census tract) against each other.# ============================================================================

log_msg("=== 05_table_C5_alternative_inference.R: start ===")

coord_path <- data_path("firm_coordinates.dta")
if (!file.exists(coord_path)) {
  log_msg("SKIPPED Table C.5: firm_coordinates.dta not found at %s", coord_path)
} else {

  coord_data <- haven::read_dta(coord_path) %>%
    mutate(lat = Lat, lon = Lon) %>%
    filter(year >= 2003 & year <= 2012) %>%
    filter(!is.na(lat), !is.na(lon)) %>%
    arrange(id_estab, year)

  treated_rule_5km <- function(d) d <= 5
  panel <- build_or_load_panel(
    file.path(cache_dir, "C5_panel_alt_inference.rds"),
    function() build_establishment_panel(coord_data, treated_rule_5km, "C.5 (0-5 km, with coordinates)")
  )

  outcomes <- c(morte = "Closure", reloc_tract_tminus1 = "Relocation")
  tv_years <- 2008:2012

  fml_agg <- function(outcome) as.formula(paste0(outcome, " ~ treat_B_agg | id_estab + year + treat_trend[code_tract_num]"))
  fml_evt <- function(outcome) as.formula(paste0(outcome, " ~ ", paste0("treat_B_", tv_years, collapse = " + "),
                                                  " | id_estab + year + treat_trend_f[code_tract_num]"))

  wild_bootstrap_se <- function(fml, data, resp_var, terms, B = 999, ncores = 6, seed = 20260727) {
    m0 <- feols(fml, data = data, lean = FALSE, notes = FALSE)
    used_idx <- if (is.null(m0$obs_selection)) seq_len(nrow(data)) else setdiff(seq_len(nrow(data)), -m0$obs_selection$obsRemoved)
    d_est <- data[used_idx, , drop = FALSE]
    y_hat <- fitted(m0)
    e_hat <- resid(m0)

    clust <- d_est$code_tract_num
    clust_levels <- unique(clust)
    G <- length(clust_levels)
    clust_idx <- match(clust, clust_levels)

    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterEvalQ(cl, { library(fixest); fixest::setFixest_nthreads(1); invisible(NULL) })
    parallel::clusterExport(cl, varlist = c("fml", "d_est", "y_hat", "e_hat", "clust_idx", "G", "resp_var", "terms"),
                             envir = environment())
    parallel::clusterSetRNGStream(cl, iseed = seed)

    boot_mat <- parallel::parSapply(cl, seq_len(B), function(b) {
      w <- sample(c(-1, 1), size = G, replace = TRUE)[clust_idx]
      d_b <- d_est
      d_b[[resp_var]] <- y_hat + e_hat * w
      m_b <- tryCatch(fixest::feols(fml, data = d_b, lean = TRUE, notes = FALSE),
                       error = function(e) NULL)
      out <- stats::setNames(rep(NA_real_, length(terms)), terms)
      if (!is.null(m_b)) {
        cf <- stats::coef(m_b)
        common <- intersect(terms, names(cf))
        out[common] <- cf[common]
      }
      out
    })
    if (!is.matrix(boot_mat)) boot_mat <- matrix(boot_mat, nrow = 1, dimnames = list(terms, NULL))

    apply(boot_mat, 1, function(x) sd(x, na.rm = TRUE))
  }

  get_main_point_estimates <- function(outcome) {
    mp <- get("main_panel", envir = .GlobalEnv)
    fmlA <- fml_agg(outcome); fmlE <- fml_evt(outcome)
    mA <- feols(fmlA, data = mp, lean = TRUE, notes = FALSE)
    mE <- feols(fmlE, data = mp, lean = TRUE, notes = FALSE)
    list(agg = get_term(mA, "treat_B_agg")$estimate,
         evt = sapply(paste0("treat_B_", tv_years), function(tm) get_term(mE, tm)$estimate))
  }
  main_panel_ok <- exists("main_panel", envir = .GlobalEnv) && is.data.frame(get("main_panel", envir = .GlobalEnv))
  if (!main_panel_ok) log_msg("WARNING: main_panel not in scope; Relocation point estimate will come from the coordinates panel instead (may not match the main text).")

  method_names <- c("Conley 10km", "Conley 20km", "Wild Bootstrap")

  fit_outcome <- function(outcome, point_override = NULL) {
    fmlA <- fml_agg(outcome); fmlE <- fml_evt(outcome)

    mA_conley10 <- feols(fmlA, data = panel, vcov = conley(10), lean = TRUE)
    mA_conley20 <- feols(fmlA, data = panel, vcov = conley(20), lean = TRUE)
    mE_conley10 <- feols(fmlE, data = panel, vcov = conley(10), lean = TRUE)
    mE_conley20 <- feols(fmlE, data = panel, vcov = conley(20), lean = TRUE)

    log_msg("Running wild cluster bootstrap for %s (aggregated, B=999)...", outcomes[[outcome]])
    se_wb_agg <- wild_bootstrap_se(fmlA, panel, outcome, terms = "treat_B_agg")
    log_msg("Running wild cluster bootstrap for %s (time-varying, B=999)...", outcomes[[outcome]])
    se_wb_evt <- wild_bootstrap_se(fmlE, panel, outcome, terms = paste0("treat_B_", tv_years))

    if (is.null(point_override)) {
      agg_point <- get_term(mA_conley10, "treat_B_agg")$estimate
      evt_point <- sapply(paste0("treat_B_", tv_years), function(tm) get_term(mE_conley10, tm)$estimate)
    } else {
      agg_point <- point_override$agg
      evt_point <- point_override$evt
    }

    se_conley10_agg <- get_term(mA_conley10, "treat_B_agg")$std.error
    se_conley20_agg <- get_term(mA_conley20, "treat_B_agg")$std.error
    se_conley10_evt <- sapply(paste0("treat_B_", tv_years), function(tm) get_term(mE_conley10, tm)$std.error)
    se_conley20_evt <- sapply(paste0("treat_B_", tv_years), function(tm) get_term(mE_conley20, tm)$std.error)

    p_from <- function(pt, se) 2 * pnorm(-abs(pt / se))

    agg_by_method <- list(
      "Conley 10km"    = data.frame(estimate = agg_point, std.error = se_conley10_agg, p.value = p_from(agg_point, se_conley10_agg)),
      "Conley 20km"    = data.frame(estimate = agg_point, std.error = se_conley20_agg, p.value = p_from(agg_point, se_conley20_agg)),
      "Wild Bootstrap" = data.frame(estimate = agg_point, std.error = se_wb_agg[["treat_B_agg"]], p.value = p_from(agg_point, se_wb_agg[["treat_B_agg"]]))
    )
    evt_by_method <- lapply(method_names, function(mth) {
      out <- lapply(paste0("treat_B_", tv_years), function(tm) {
        pt <- evt_point[[tm]]
        se <- if (mth == "Conley 10km") se_conley10_evt[[tm]]
              else if (mth == "Conley 20km") se_conley20_evt[[tm]]
              else se_wb_evt[[tm]]
        data.frame(estimate = pt, std.error = se, p.value = p_from(pt, se))
      })
      names(out) <- paste0("treat_B_", tv_years)
      out
    })
    names(evt_by_method) <- method_names

    list(agg = agg_by_method, evt = evt_by_method,
         nobs = nobs(mE_conley10),
         nclust = dplyr::n_distinct(panel$code_tract_num[!is.na(panel$treat_B)]))
  }

  closure_override <- if (main_panel_ok) get_main_point_estimates("morte") else NULL
  res_closure <- fit_outcome("morte", point_override = closure_override)
  reloc_override <- if (main_panel_ok) get_main_point_estimates("reloc_tract_tminus1") else NULL
  res_reloc   <- fit_outcome("reloc_tract_tminus1", point_override = reloc_override)

  col_outcome <- rep(c("Closure", "Relocation"), each = length(method_names))
  col_method  <- rep(method_names, times = 2)
  res_by_col  <- rep(list(res_closure, res_reloc), each = length(method_names))
  n_col <- length(col_outcome)

  row_gap <- function(label_txt, vals) paste0(label_txt, " & ", paste(as.vector(rbind(vals, rep("", length(vals)))), collapse = " & "), " \\\\")

  coefA <- mapply(function(res, m) fmt_coef(res$agg[[m]]$estimate, res$agg[[m]]$p.value), res_by_col, col_method)
  seA   <- mapply(function(res, m) fmt_se(res$agg[[m]]$std.error), res_by_col, col_method)

  lines <- c(
    "\\begin{table}[htb]", "  \\centering",
    "  \\tabcaption{Establishment-Level Estimates Using Alternative Inference Procedures}",
    "    \\label{tab:reest_altinf}",
    "    \\scalebox{0.65}{",
    " \\begin{threeparttable}",
    paste0("    \\begin{tabular}{l", strrep("c", 2 * n_col), "}"),
    "\\toprule",
    paste0("          & ", paste(as.vector(rbind(paste0("(", seq_len(n_col), ")"), rep("", n_col))), collapse = " & "), " \\\\"),
    paste(sapply(seq_len(n_col), function(i) sprintf("\\cmidrule(lr){%d-%d}", 2 * i, 2 * i)), collapse = ""),
    row_gap("Dep. Var:", col_outcome),
    row_gap("Inference:", col_method),
    "\\midrule",
    paste0("    \\multicolumn{", 2 * n_col + 1, "}{l}{\\textbf{Panel A: Time-Aggregated DiD}}\\\\"),
    row_gap("Flash Flood Post", coefA),
    row_gap("                 ", seA)
  )

  lines <- c(lines, "\\midrule",
             paste0("    \\multicolumn{", 2 * n_col + 1, "}{l}{\\textbf{Panel B: Time-Varying DiD}}\\\\"))
  for (yr in tv_years) {
    term <- paste0("treat_B_", yr)
    coefY <- mapply(function(res, m) fmt_coef(res$evt[[m]][[term]]$estimate, res$evt[[m]][[term]]$p.value), res_by_col, col_method)
    seY   <- mapply(function(res, m) fmt_se(res$evt[[m]][[term]]$std.error), res_by_col, col_method)
    lines <- c(lines, row_gap(sprintf("Flash Flood %d", yr), coefY), row_gap("                 ", seY))
  }

  nobs_col <- sapply(res_by_col, function(res) fmt_obs(res$nobs))

  lines <- c(
    lines,
    "\\midrule",
    row_gap("Observations", nobs_col),
    "    \\bottomrule",
    "    \\bottomrule",
    "    \\end{tabular}%",
    "       \t\\begin{tablenotes}[flushleft] \\item \\small \\textit{Notes:} This table reports estimates of equation (1) for establishment closure (columns 1--3) and spatial relocation (columns 4--6) using alternative inference procedures. Columns (1) and (4) use Conley spatial standard errors with a 10 km cutoff; columns (2) and (5) use a 20 km cutoff; and columns (3) and (6) use a wild cluster bootstrap procedure at the census-tract level with 999 replications. Panel A presents estimates using a single post-treatment dummy, whereas Panel B presents estimates using time-varying treatment dummies. All specifications include establishment and year fixed effects and a census-tract linear trend. The treatment radius ranges from 0--5 km from the flood spots, and the control ring is between 50--80 km.  *** represents p $<$ 0.01, ** represents p $<$ 0.05, * represents p $<$ 0.1.",
    "   \t\t\\end{tablenotes}",
    "   \t\t\\end{threeparttable}",
    "   \t\t}",
    "\\end{table}%"
  )

  out_path <- file.path(tables_dir, "Tab_C05_Alternative_Inference_5km_tract.tex")
  writeLines(lines, out_path)
  log_msg("Saved table: %s", out_path)
}

log_msg("=== 05_table_C5_alternative_inference.R: done ===")
