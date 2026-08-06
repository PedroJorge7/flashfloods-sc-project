# ============================================================================
# Appendix F, Table F.3 — Worker-Level Estimates Dropping Units that Moved
# Across Areas
# Treatment: 0-5 km vs. 50-80 km control. The sample excludes workers whose
# treated/control-ring status switches across the panel. The table reports
# employment and wage outcomes from specifications with a census-tract trend.
# Uses `dados_main` (2002-2012) from 00b_load_worker_data.R.
# ============================================================================

log_msg("=== 03_table_F3_dropping_movers_workers.R: start ===")

output_no_movers <- output_empregados(
  dados_main,
  WORKER_MIN_TREAT,
  WORKER_MAX_TREAT,
  WORKER_MIN_CTRL,
  WORKER_MAX_CTRL,
  remove_treat_control_mob = TRUE,
  trend = TRUE,
  se_type = "cluster",
  cluster_formula = WORKER_CLUSTER_FORMULA
)

get_worker_term_f3 <- function(regression_label, term_label) {
  row <- output_no_movers %>%
    dplyr::filter(
      type == "type_treatment",
      Regression == regression_label,
      parmseq == term_label
    )
  if (nrow(row) == 0L) {
    return(list(estimate = NA_real_, se = NA_real_, p = NA_real_, n = NA_real_))
  }
  list(
    estimate = as.numeric(row$estimate[1]),
    se = as.numeric(row$se[1]),
    p = as.numeric(row$p.value[1]),
    n = as.numeric(row$nobs[1])
  )
}

regressions <- c("A - Employment (1/0)", "B - Wage Value")

row_gap_f3 <- function(label, values) {
  paste0(label, " & ", paste(values, collapse = " & & "), " \\\\")
}

post <- lapply(regressions, get_worker_term_f3, term_label = "Flash Flood Post")

lines <- c(
  "\\begin{table}[htb]",
  "  \\centering",
  "  \\tabcaption{Worker-Level Estimates Dropping Units that Moved Across Areas}",
  "  \\label{tab:F3_worker_remove_mob}",
  "  \\scalebox{0.80}{",
  "  \\begin{threeparttable}",
  "    \\begin{tabular}{l c p{0.60cm} c}",
  "    \\toprule",
  "          & (1) & & (2) \\\\",
  "    \\cmidrule(lr){2-2}\\cmidrule(lr){4-4}",
  "    Dep. Var: & Employment & & Wage \\\\",
  "    \\midrule",
  "    \\multicolumn{4}{l}{\\textbf{Panel A: Time-Aggregated DiD}}\\\\",
  row_gap_f3(
    "    Flash Flood Post",
    vapply(post, function(x) fmt_coef(x$estimate, x$p), character(1))
  ),
  row_gap_f3(
    "                     ",
    vapply(post, function(x) fmt_se(x$se), character(1))
  ),
  "    \\midrule",
  "    \\multicolumn{4}{l}{\\textbf{Panel B: Time-Varying DiD}}\\\\"
)

for (year in 2008:2012) {
  term <- paste0("Flash Flood ", year)
  estimates <- lapply(regressions, get_worker_term_f3, term_label = term)
  lines <- c(
    lines,
    row_gap_f3(
      paste0("    ", term),
      vapply(estimates, function(x) fmt_coef(x$estimate, x$p), character(1))
    ),
    row_gap_f3(
      "                     ",
      vapply(estimates, function(x) fmt_se(x$se), character(1))
    )
  )
}

lines <- c(
  lines,
  "    \\midrule",
  row_gap_f3(
    "    Observations",
    vapply(post, function(x) fmt_obs(x$n), character(1))
  ),
  row_gap_f3("    Census Tract Trend", c("Yes", "Yes")),
  "    \\bottomrule",
  "    \\end{tabular}%",
  "    \\begin{tablenotes}[flushleft]",
  paste0(
    "    \\item \\small \\textit{Notes:} This table excludes workers whose ",
    "treated/control-ring status changes across the panel. Panel A uses a ",
    "single post-treatment dummy, and Panel B uses time-varying treatment ",
    "dummies for 2008--2012, with 2007 omitted. Worker and year fixed effects ",
    "and a census-tract linear trend are included. Standard errors clustered ",
    "at the census-tract level are in parentheses. *** p $<$ 0.01, ",
    "** p $<$ 0.05, * p $<$ 0.1."
  ),
  "    \\end{tablenotes}",
  "  \\end{threeparttable}",
  "  }",
  "\\end{table}%"
)

out_path <- file.path(
  tables_dir,
  "Tab_F03_Workers_Dropping_Movers_5km_tract.tex"
)
writeLines(lines, out_path)
log_msg("Saved table: %s", out_path)

log_msg("=== 03_table_F3_dropping_movers_workers.R: done ===")
