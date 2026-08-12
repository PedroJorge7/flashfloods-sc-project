# Shared worker-level helpers: PSM matching, DiD regression, and LaTeX
# table/figure output. treat_B is fixed once per worker from their 2007
# baseline job, and outcomes track each worker's full RAIS history
# (including years outside Santa Catarina) instead of being nulled when
# they leave the treated/control bands. gen_table() and event_study_plot()
# are unchanged from the original.

source(file.path(project_root, "Code", "utils", "path_utils.R"))

library(fixest)
library(tidyr)
library(dplyr)
library(MatchIt)

# -----------------------------------------------------------------------------
# output_empregados() — full PSM + DiD pipeline for dismissed workers.
# exposed_workers = TRUE: baseline treat = any worker in the treated band at
#   their 2007 job (not just those dismissed in 2008).
# remove_treat_control_mob = TRUE: drops workers ever observed at a job whose
#   distance band is opposite their baseline classification.
# -----------------------------------------------------------------------------
output_empregados <- function(dados, min_treat, max_treat,
                               min_control, max_control,
                               remove_treat_control_mob = FALSE,
                               expand_outcomes = FALSE,
                               exposed_workers = FALSE,
                               trend = FALSE,
                               PSM = TRUE,
                               se_type = "twoway",
                               cluster_formula = ~ year + cpf) {

  if (expand_outcomes) {
    stop("output_empregados() [New_Results, corrected]: expand_outcomes is not implemented ",
         "in this corrected version (no Appendix script sets it).")
  }

  se_type <- match.arg(se_type, c("twoway", "cluster"))

  run_worker_model <- function(formula, data, fixef_rm = NULL) {
    model_args <- list(fml = formula, data = data)
    if (!is.null(fixef_rm)) model_args$fixef.rm <- fixef_rm
    if (identical(se_type, "cluster")) model_args$cluster <- cluster_formula
    model <- do.call(feols, model_args)
    if (identical(se_type, "cluster")) summary(model, cluster = cluster_formula) else summary(model, se = "twoway")
  }

  # ---------------------------------------------------------------------------
  # Step 1: PSM on the 2007 cross-section. `treat` (1 = treated, 0 = control)
  # becomes the worker's FIXED baseline classification, carried through to
  # every year of that worker's panel in Step 3.
  # ---------------------------------------------------------------------------
  dados2 <- dados %>% filter(year == 2007)
  dados3 <- dados2 %>%
    filter(between(dist_flood, min_treat, max_treat) | between(dist_flood, min_control, max_control))
  if (exposed_workers) {
    dados3 <- dados3 %>%
      mutate(treat   = ifelse(between(dist_flood, min_treat, max_treat), 1, 0),
             control = ifelse(between(dist_flood, min_control, max_control), 1, 0)) %>%
      filter(treat == 1 | control == 1)
  } else {
    dados3 <- dados3 %>%
      mutate(treat   = ifelse(ano_morte == 2008 & between(dist_flood, min_treat, max_treat), 1, 0),
             control = ifelse(between(dist_flood, min_control, max_control), 1, 0)) %>%
      filter(treat == 1 | control == 1)
  }

  dados4 <- dados3 %>%
    mutate(
      educ_d1 = ifelse(grau_instr %in% c("1", "2", "3"), 1, 0),
      educ_d2 = ifelse(grau_instr %in% c("4", "5"), 1, 0),
      educ_d3 = ifelse(grau_instr %in% c("6", "7"), 1, 0),
      educ_d4 = ifelse(grau_instr %in% c("8", "9", "10", "11"), 1, 0),
      male    = ifelse(genero == "1", 1, 0),
      idade = as.numeric(idade), grau_instr = as.numeric(grau_instr),
      rem_med_r = as.numeric(rem_med_r), rem_dez_r = as.numeric(rem_dez_r),
      codemun = as.numeric(codemun), temp_empr = as.numeric(temp_empr),
      tamestab = as.numeric(tamestab), subs_ibge = as.numeric(subs_ibge),
      Construcao  = ifelse(subs_ibge == 15, 1, 0),
      Industria   = ifelse(subs_ibge %in% 3:13, 1, 0),
      Comercio    = ifelse(subs_ibge %in% c(16, 17), 1, 0),
      servicos    = ifelse(subs_ibge == 21, 1, 0),
      transporte  = ifelse(subs_ibge == 20, 1, 0),
      trabalhador_fixo       = ifelse(tipo_sintetizado %in% c("CLT", "Estatutário"), 1, 0),
      trabalhador_temporario = ifelse(tipo_sintetizado == "Trabalho Temporário", 1, 0),
      trabalhador_outros     = ifelse(tipo_sintetizado %in% c("Outros", "Trabalho Avulso"), 1, 0)
    )

  if (PSM) {
    psm <- matchit(
      treat ~ idade + rem_med_r + temp_empr + tamestab,
      method = "nearest",
      exact  = c("educ_d1", "educ_d2", "educ_d3", "educ_d4", "male",
                 "Construcao", "Industria", "Comercio", "servicos", "transporte",
                 "trabalhador_fixo", "trabalhador_temporario", "trabalhador_outros"),
      replace = FALSE,
      data    = dados4
    )
    m.data <- match.data(psm)
    baseline <- m.data[, c("cpf", "weights", "treat")]
  } else {
    baseline <- dados4[, c("cpf", "treat")]
    baseline$weights <- 1
  }
  names(baseline)[names(baseline) == "treat"] <- "treat_base"

  # Merge the fixed baseline classification onto EVERY year of that worker's
  # raw records (merge is by cpf only, not cpf+year).
  dados2m <- merge(dados, baseline, by = "cpf")

  # ---------------------------------------------------------------------------
  # Step 2: expand to a balanced panel and compute variables (same fill()
  # logic as the original; treat_base/weights added to the cpf-grouped fill
  # so every expanded row -- including pure gap years with no raw record at
  # all -- carries the worker's constant baseline classification).
  # ---------------------------------------------------------------------------
  dados3e <- expand.grid(distinct(dados2m, cpf)$cpf, 2002:max(dados$year))
  names(dados3e) <- c('cpf', 'year')

  dados4e <- left_join(dados3e, dados2m, by = c("cpf", "year")) %>%
    arrange(cpf, year) %>%
    mutate(ano_nascimento = year - idade, code_tract = as.numeric(code_tract)) %>%
    group_by(cpf) %>%
    tidyr::fill(codemun, id_estab, code_tract, pis, min_ano, genero,
                ano_nascimento, grau_instr, treat_base, weights, .direction = 'downup') %>%
    group_by(id_estab) %>%
    tidyr::fill(ano_morte, dist_flood, code_tract, .direction = 'downup') %>%
    group_by(cpf) %>%
    mutate(
      idade = ifelse(is.na(idade), year - ano_nascimento, idade),
      # Pure RAIS-presence indicator: 1 if this worker has a valid end-of-year
      # employment record THIS YEAR, regardless of where that job is located
      # (not nulled based on distance to the flood spots).
      empregado = ifelse(is.na(as.numeric(emp_31dez)), 0, 1)
    ) %>%
    filter(year >= 2003 & year <= max(dados$year), year >= min_ano) %>%
    ungroup()

  # ---------------------------------------------------------------------------
  # Step 3: treatment dummies, built from the FIXED baseline classification
  # (treat_base), not from that year's dist_flood.
  # ---------------------------------------------------------------------------
  dados4e$treat_B <- dados4e$treat_base

  if (remove_treat_control_mob) {
    # REDEFINED (see the header comment above): drop workers ever observed
    # with a current-job distance band opposite their baseline treat_B.
    dados4e <- dados4e %>%
      mutate(
        .this_year_band = case_when(
          between(dist_flood, min_treat, max_treat) ~ 1,
          between(dist_flood, min_control, max_control) ~ 0,
          TRUE ~ NA_real_
        ),
        .crossed = !is.na(.this_year_band) & .this_year_band != treat_B
      ) %>%
      group_by(cpf) %>%
      filter(!any(.crossed)) %>%
      ungroup() %>%
      select(-.this_year_band, -.crossed)
    log_msg("remove_treat_control_mob: %d worker-year rows retained after dropping ring-crossers", nrow(dados4e))
  }

  dados4e$treat_B_agg <- ifelse(dados4e$year >= 2009 & dados4e$treat_B == 1, 1, 0)

  for (y in 2002:2016) {
    dados4e[[paste0("treat_B_", y)]] <- ifelse(dados4e$year == y, dados4e$treat_B, 0)
  }

  dados4 <- dados4e

  # code_tract required non-missing regardless of `trend`, so no-trend and
  # trend calls on the same data use identical N.
  dados4$code_tract <- ifelse(is.na(dados4$code_tract), dados4$codemun, dados4$code_tract)
  dados4$code_tract <- as.numeric(dados4$code_tract)
  dados4$codemun    <- as.numeric(dados4$codemun)
  dados4$year       <- as.numeric(dados4$year)
  dados4$pre_pos    <- ifelse(dados4$year > 2008, 1, 0)
  n_before_tract_filter <- nrow(dados4)
  dados4 <- dados4[!is.na(dados4$code_tract), ]
  if (nrow(dados4) < n_before_tract_filter) {
    log_msg("Dropped %d rows with no usable code_tract/codemun (kept %d)",
            n_before_tract_filter - nrow(dados4), nrow(dados4))
  }

  # ---------------------------------------------------------------------------
  # Step 4: estimate employment models.
  # ---------------------------------------------------------------------------
  if (trend) {
    reg1 <- run_worker_model(empregado ~ i(year, treat_B, 2007) | year + cpf + pre_pos[code_tract], data = dados4)
    output1 <- data.frame(Regression = "A - Employment (1/0)", parmseq = as.numeric(gsub(".*::(\\d+):.*", "\\1", names(coef(reg1)))), estimate = coef(reg1), se = reg1$se, min = coef(reg1) - 1.96 * reg1$se, max = coef(reg1) + 1.96 * reg1$se, p.value = broom::tidy(reg1)$p.value, nobs = reg1$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control))
    output1 <- rbind(output1,
                     c("A - Employment (1/0)", 2007, 0, 0, 0, 0, 0, 0, paste0(min_treat, "-", max_treat), paste0(min_control, "-", max_control)))
    output1 <- output1 %>% arrange(Regression, parmseq) %>%
      mutate(estimate = as.numeric(estimate), min = as.numeric(min), max = as.numeric(max),
             parmseq = as.numeric(parmseq))
    output1$type <- 'event_study'

    reg1 <- run_worker_model(empregado ~ treat_B_agg | year + cpf + pre_pos[code_tract], data = dados4)
    output2 <- data.frame(Regression = "A - Employment (1/0)", parmseq = "Flash Flood Post", estimate = coef(reg1), se = reg1$se, min = coef(reg1) - 1.96 * reg1$se, max = coef(reg1) + 1.96 * reg1$se, p.value = broom::tidy(reg1)$p.value, nobs = reg1$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control))
    output2$type <- 'type_treatment'

    reg1 <- run_worker_model(empregado ~ treat_B_2008 + treat_B_2009 + treat_B_2010 + treat_B_2011 + treat_B_2012 + treat_B_2013 + treat_B_2014 + treat_B_2015 + treat_B_2016 | year + cpf + pre_pos[code_tract], data = dados4)
    output3 <- data.frame(Regression = "A - Employment (1/0)", parmseq = gsub("treat_B_", "Flash Flood ", names(coef(reg1))), estimate = coef(reg1), se = reg1$se, min = coef(reg1) - 1.96 * reg1$se, max = coef(reg1) + 1.96 * reg1$se, p.value = broom::tidy(reg1)$p.value, nobs = reg1$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control))
    output3$type <- 'type_treatment'

  } else {
    reg1 <- run_worker_model(empregado ~ i(year, treat_B, 2007) | year + cpf, data = dados4)
    output1 <- data.frame(Regression = "A - Employment (1/0)", parmseq = as.numeric(gsub(".*::(\\d+):.*", "\\1", names(coef(reg1)))), estimate = coef(reg1), se = reg1$se, min = coef(reg1) - 1.96 * reg1$se, max = coef(reg1) + 1.96 * reg1$se, p.value = broom::tidy(reg1)$p.value, nobs = reg1$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control))
    output1 <- rbind(output1,
                     c("A - Employment (1/0)", 2007, 0, 0, 0, 0, 0, 0, paste0(min_treat, "-", max_treat), paste0(min_control, "-", max_control)))
    output1 <- output1 %>% arrange(Regression, parmseq) %>%
      mutate(estimate = as.numeric(estimate), min = as.numeric(min), max = as.numeric(max),
             parmseq = as.numeric(parmseq))
    output1$type <- 'event_study'

    reg1 <- run_worker_model(empregado ~ treat_B_agg | year + cpf, data = dados4)
    output2 <- data.frame(Regression = "A - Employment (1/0)", parmseq = "Flash Flood Post", estimate = coef(reg1), se = reg1$se, min = coef(reg1) - 1.96 * reg1$se, max = coef(reg1) + 1.96 * reg1$se, p.value = broom::tidy(reg1)$p.value, nobs = reg1$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control))
    output2$type <- 'type_treatment'

    reg1 <- run_worker_model(empregado ~ treat_B_2008 + treat_B_2009 + treat_B_2010 + treat_B_2011 + treat_B_2012 + treat_B_2013 + treat_B_2014 + treat_B_2015 + treat_B_2016 | year + cpf, data = dados4)
    output3 <- data.frame(Regression = "A - Employment (1/0)", parmseq = gsub("treat_B_", "Flash Flood ", names(coef(reg1))), estimate = coef(reg1), se = reg1$se, min = coef(reg1) - 1.96 * reg1$se, max = coef(reg1) + 1.96 * reg1$se, p.value = broom::tidy(reg1)$p.value, nobs = reg1$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control))
    output3$type <- 'type_treatment'
  }

  output <- plyr::rbind.fill(output1, output2, output3)
  row.names(output) <- 1:nrow(output)
  return(output)
}

# -----------------------------------------------------------------------------
# gen_table()  [unchanged]
# Formats the output of output_empregados() into a printable coefficient table.
# -----------------------------------------------------------------------------
gen_table <- function(df) {
  table <- df %>%
    filter(type == 'type_treatment') %>%
    rename(std.error = se, term = parmseq, output = Regression)

  nobs <- table %>% select(output, nobs) %>% distinct(output, .keep_all = TRUE)
  nobs <- nobs$nobs

  table <- table %>%
    mutate(
      std.error = as.numeric(std.error),
      estimate  = as.numeric(estimate),
      estimate  = sprintf("%.5f", round(estimate, 5)),
      std.error = sprintf("%.5f", round(std.error, 5)),
      p.value   = as.numeric(p.value),
      estimate  = ifelse(p.value <= 0.01, paste0(estimate, "***"),
                  ifelse(p.value <= 0.05, paste0(estimate, "**"),
                  ifelse(p.value <= 0.10, paste0(estimate, "*"), estimate))),
      std.error = paste0("(", std.error, ")")
    ) %>%
    select(output, term, Coef = estimate, std.error)

  table <- table %>% pivot_wider(names_from = 'output', values_from = c('Coef', 'std.error'))

  table <- do.call(rbind, lapply(1:nrow(table), function(i) {
    rbind(
      data.frame(term = table$term[i], Employment = table$`Coef_A - Employment (1/0)`[i], stringsAsFactors = FALSE),
      data.frame(term = "",            Employment = table$`std.error_A - Employment (1/0)`[i], stringsAsFactors = FALSE)
    )
  }))

  rbind(table, c("", nobs))
}

# -----------------------------------------------------------------------------
# event_study_plot()  [unchanged]
# Builds a ggplot event-study panel from the output of output_empregados().
# -----------------------------------------------------------------------------
event_study_plot <- function(reg_name, data) {
  dd <- data %>%
    filter(Regression == reg_name, type == "event_study") %>%
    mutate(
      parmseq  = as.numeric(parmseq),
      estimate = as.numeric(estimate),
      min      = as.numeric(min),
      max      = as.numeric(max)
    ) %>%
    filter(is.finite(estimate)) %>%
    arrange(parmseq)

  ggplot2::ggplot(dd, ggplot2::aes(x = parmseq, y = estimate)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = min, ymax = max), fill = "grey90", alpha = 0.5) +
    ggplot2::geom_line(ggplot2::aes(y = max), color = "grey30", linewidth = 0.1) +
    ggplot2::geom_line(ggplot2::aes(y = min), color = "grey30", linewidth = 0.1) +
    ggplot2::geom_line(color = "firebrick", linewidth = 1) +
    ggplot2::geom_point(size = 2, color = "firebrick") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 2007.5, linetype = "dashed") +
    ggplot2::scale_x_continuous(breaks = sort(unique(dd$parmseq))) +
    ggplot2::labs(x = "Year", y = "Coefficient", title = reg_name) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}
