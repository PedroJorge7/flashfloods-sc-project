# ============================================================================
# Table 5 — Effect of Flash Floods on Hazard of Establishment Closure
# Cox proportional hazards model (survival::coxph), not fixest::feols.
# Treatment: 0-5 km vs. 50-80 km control. Clustering: census tract.
# Strata: sector x year. Depends on `main_panel`.
# ============================================================================

log_msg("=== 07_table5_hazard_establishment_closure.R: start ===")

cox_data <- main_panel %>%
  filter(!is.na(treat_B)) %>%
  mutate(start = year - 1) %>%
  arrange(id_estab, year)

model1_data <- cox_data %>% filter(!is.na(treat_B_agg), !is.na(subs_ibge), !is.na(code_tract_num))

cox1 <- tryCatch(
  coxph(Surv(start, year, morte) ~ treat_B_agg + strata(subs_ibge, year) + cluster(code_tract_num),
        data = model1_data, ties = "breslow", na.action = na.omit),
  error = function(e) { log_msg("ERROR fitting cox1: %s", conditionMessage(e)); NULL }
)

baseline_vars <- c("empregados", "afil", "yr_abert", "educ_d1", "educ_d2", "educ_d3", "educ_d4")
have_baseline <- all(baseline_vars %in% names(cox_data))

cox2 <- NULL
if (have_baseline) {
  model2_data <- cox_data
  for (v in baseline_vars) model2_data[[paste0(v, "2")]] <- make_first_year_value(model2_data, v)
  model2_data <- model2_data %>%
    filter(!is.na(treat_B_agg), !is.na(subs_ibge), !is.na(code_tract_num),
           !is.na(empregados2), !is.na(afil2), !is.na(yr_abert2),
           !is.na(educ_d12), !is.na(educ_d22), !is.na(educ_d32), !is.na(educ_d42))
  cox2 <- tryCatch(
    coxph(Surv(start, year, morte) ~ treat_B_agg + empregados2 + afil2 + yr_abert2 +
            educ_d22 + educ_d32 + educ_d42 + strata(subs_ibge, year) + cluster(code_tract_num),
          data = model2_data, ties = "breslow", na.action = na.omit),
    error = function(e) { log_msg("ERROR fitting cox2: %s", conditionMessage(e)); NULL }
  )
} else {
  log_msg("NOTE: baseline control variables not all available (%s); model 2 skipped",
          paste(setdiff(baseline_vars, names(cox_data)), collapse = ", "))
}

extract_cox <- function(model, data, term = "treat_B_agg") {
  if (is.null(model)) return(list(coef = NA_real_, se = NA_real_, p = NA_real_, n = NA_integer_,
                                   n_units = NA_integer_, n_failures = NA_integer_))
  sm <- summary(model)
  co <- sm$coefficients
  list(
    coef = co[term, "coef"], se = co[term, "robust se"], p = co[term, "Pr(>|z|)"],
    n = sm$n, n_units = dplyr::n_distinct(data$id_estab), n_failures = sum(data$morte == 1, na.rm = TRUE)
  )
}

r1 <- extract_cox(cox1, model1_data)
r2 <- if (have_baseline) extract_cox(cox2, model2_data) else extract_cox(NULL, model1_data)

fmt_hazard <- function(coef) if (is.na(coef)) "n/a" else sprintf("%.5f", exp(coef))

lines <- c(
  "\\begin{table}[htb]", "  \\centering",
  "  \\tabcaption{Effect of Flash Floods on Hazard of Establishment Closure}",
  "  \\label{tab:cox_flash}",
  "  \\scalebox{0.85}{",
  "  \\begin{threeparttable}",
  "  \\begin{tabular}{l cc}", "    \\toprule",
  "          & (1) & (2) \\\\",
  "    \\midrule",
  sprintf("    Flash Flood Post\n          & %s & %s \\\\", fmt_coef(r1$coef, r1$p), fmt_coef(r2$coef, r2$p)),
  sprintf("          & %s  & %s  \\\\", fmt_se(r1$se), fmt_se(r2$se)),
  sprintf("    Hazard Ratio    & %s  & %s \\\\", fmt_hazard(r1$coef), fmt_hazard(r2$coef)),
  "    \\midrule",
  sprintf("    Observations                & %s & %s \\\\", fmt_obs(r1$n), fmt_obs(r2$n)),
  sprintf("    Number of Units             & %s  & %s  \\\\", fmt_obs(r1$n_units), fmt_obs(r2$n_units)),
  sprintf("    Number of Failures          & %s  & %s  \\\\", fmt_obs(r1$n_failures), fmt_obs(r2$n_failures)),
  "    Establishment Controls      & No      & Yes     \\\\",
  "    \\bottomrule", "    \\bottomrule",
  "  \\end{tabular}",
  "  \\begin{tablenotes}[flushleft]",
  "  \\scriptsize",
  "  \\item Notes: Estimates of equation (\\ref{eq:cox}) using the hazard of establishment closure as the dependent variable. The census-tract clustered standard errors are in parentheses. Observations denote establishment-year observations, the number of units corresponds to unique establishments at risk, and the number of failures indicates establishments that experienced the failure event (closure). All models include sector and year fixed effects. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.",
  "  \\end{tablenotes}",
  "  \\end{threeparttable}",
  "  }",
  "\\end{table}%"
)

out_path <- file.path(tables_dir, "Tab_05_Hazard_Establishment_Closure_5km_tract.tex")
writeLines(lines, out_path)
log_msg("Saved table: %s", out_path)

log_msg("=== 07_table5_hazard_establishment_closure.R: done ===")
