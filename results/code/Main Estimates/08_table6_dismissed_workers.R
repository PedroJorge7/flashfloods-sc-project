# ============================================================================
# Table 6 — The Effect of Disaster-Induced Closures on Dismissed Workers
# Treatment: 0-5 km vs. 50-80 km control. Clustering: census tract.
# Employment outcome only. Four columns:
#   (1) All workers, no census-tract trend
#   (2) All workers, with census-tract trend
#   (3) High Skill workers, with trend (grau_instr >= 8 in 2007)
#   (4) Low Skill workers, with trend (grau_instr < 8 in 2007)
# Uses `dados_main` (2002-2012) from 00b_load_worker_data.R.
#
# output_trend (column 2, all workers, with trend) is kept as its own object
# because Figure 5 (the worker event study) depends on it.
# ============================================================================

log_msg("=== 08_table6_dismissed_workers.R: start ===")

educ_2007 <- dados_main %>%
  dplyr::filter(year == 2007) %>%
  dplyr::distinct(cpf, .keep_all = TRUE) %>%
  dplyr::transmute(cpf, grau_instr_2007 = suppressWarnings(as.numeric(grau_instr)))

cpf_higher <- educ_2007 %>% dplyr::filter(grau_instr_2007 >= 8) %>% dplyr::pull(cpf)
cpf_lower  <- educ_2007 %>% dplyr::filter(grau_instr_2007 < 8)  %>% dplyr::pull(cpf)

log_msg("Skill split (2007 baseline): %d High Skill workers, %d Low Skill workers", length(cpf_higher), length(cpf_lower))

dados_higher <- dados_main %>% dplyr::filter(cpf %in% cpf_higher)
dados_lower  <- dados_main %>% dplyr::filter(cpf %in% cpf_lower)

output_notrend <- output_empregados(
  dados_main, WORKER_MIN_TREAT, WORKER_MAX_TREAT, WORKER_MIN_CTRL, WORKER_MAX_CTRL,
  trend = FALSE, se_type = "cluster", cluster_formula = WORKER_CLUSTER_FORMULA
)

output_trend <- output_empregados(
  dados_main, WORKER_MIN_TREAT, WORKER_MAX_TREAT, WORKER_MIN_CTRL, WORKER_MAX_CTRL,
  trend = TRUE, se_type = "cluster", cluster_formula = WORKER_CLUSTER_FORMULA
)

output_high_trend <- output_empregados(
  dados_higher, WORKER_MIN_TREAT, WORKER_MAX_TREAT, WORKER_MIN_CTRL, WORKER_MAX_CTRL,
  trend = TRUE, se_type = "cluster", cluster_formula = WORKER_CLUSTER_FORMULA
)

output_low_trend <- output_empregados(
  dados_lower, WORKER_MIN_TREAT, WORKER_MAX_TREAT, WORKER_MIN_CTRL, WORKER_MAX_CTRL,
  trend = TRUE, se_type = "cluster", cluster_formula = WORKER_CLUSTER_FORMULA
)

# Pull a single coefficient/SE/p/nobs out of an output_empregados() result.
get_worker_term <- function(df, regression_label, term_label) {
  row <- df %>% dplyr::filter(type == "type_treatment", Regression == regression_label, parmseq == term_label)
  if (nrow(row) == 0) return(list(estimate = NA_real_, se = NA_real_, p = NA_real_, n = NA_integer_))
  list(estimate = as.numeric(row$estimate[1]), se = as.numeric(row$se[1]),
       p = as.numeric(row$p.value[1]), n = as.numeric(row$nobs[1]))
}

col_sources <- list(
  list(df = output_notrend,    reg = "A - Employment (1/0)"),
  list(df = output_trend,      reg = "A - Employment (1/0)"),
  list(df = output_high_trend, reg = "A - Employment (1/0)"),
  list(df = output_low_trend,  reg = "A - Employment (1/0)")
)
trend_row <- c("No", "Yes", "Yes", "Yes")

row_gap <- function(label_txt, vals) {
  inter <- as.vector(rbind(vals, rep("", length(vals))))
  paste0(label_txt, " & ", paste(inter, collapse = " & "), " \\\\")
}

post_terms <- lapply(col_sources, function(s) get_worker_term(s$df, s$reg, "Flash Flood Post"))
coefA <- sapply(post_terms, function(x) fmt_coef(x$estimate, x$p))
seA   <- sapply(post_terms, function(x) fmt_se(x$se))
nobsA <- sapply(post_terms, function(x) fmt_obs(x$n))

lines <- c(
  "\\begin{table}[htb]", "  \\centering",
  "  \\tabcaption{The Effect of Disaster-Induced Closures on Dismissed Workers}",
  "  \\label{tab:workers_results}",
  "  \\scalebox{0.75}{",
  "    \\begin{threeparttable}",
  "      \\begin{tabular}{l c p{0.60cm} c p{0.25cm} c p{0.25cm} c}",
  "        \\toprule",
  "        & (1) & & (2) & & (3) & & (4) \\\\",
  "        \\cmidrule(lr){2-2}",
  "        \\cmidrule(lr){4-4}",
  "        \\cmidrule(lr){6-6}",
  "        \\cmidrule(lr){8-8}",
  "        Dep. Var:",
  "        & Employment & & Employment & & Employment & & Employment \\\\",
  "        Sample:",
  row_gap("        ", c("All Workers", "All Workers", "High Skill", "Low Skill")),
  "        \\midrule",
  "        \\multicolumn{8}{l}{\\textbf{Panel A: Time-Aggregated DiD}}\\\\",
  "        Flash Flood Post",
  row_gap("        ", coefA),
  row_gap("        ", seA),
  "        Observations",
  row_gap("        ", nobsA),
  "        Census Tract Trend",
  row_gap("        ", trend_row),
  "        \\midrule",
  "        \\multicolumn{8}{l}{\\textbf{Panel B: Time-Varying DiD}}\\\\"
)

tv_years <- 2008:2012
for (yr in tv_years) {
  term_label <- paste0("Flash Flood ", yr)
  est <- lapply(col_sources, function(s) get_worker_term(s$df, s$reg, term_label))
  coefB <- sapply(est, function(x) fmt_coef(x$estimate, x$p))
  seB   <- sapply(est, function(x) fmt_se(x$se))
  lines <- c(lines,
             sprintf("        Flash Flood %d", yr),
             row_gap("        ", coefB),
             row_gap("        ", seB))
}

nobsB <- sapply(lapply(col_sources, function(s) get_worker_term(s$df, s$reg, "Flash Flood 2008")), function(x) fmt_obs(x$n))
lines <- c(
  lines,
  "        Observations",
  row_gap("        ", nobsB),
  "        Census Tract Trend",
  row_gap("        ", trend_row),
  "        \\bottomrule", "        \\bottomrule",
  "      \\end{tabular}",
  "      \\begin{tablenotes}[flushleft]",
  "        \\item \\small \\textit{Notes:} This table shows estimates from a difference-in-differences model using the formal employment indicator as the outcome variable. Panel A presents results from a specification using a single post-treatment dummy, whereas Panel B presents results from a specification with time-varying treatment dummies (equation (\\ref{eq.eq3})). Columns (1) and (2) use the full matched sample. Column (3) includes high-skill workers, defined as those with higher education, while column (4) includes low-skill workers, defined as those with less than higher education. The treatment group consists of workers formally employed at the end of 2008 at establishments located within a 0--5 km spatial buffer around the flood spots and classified as closed in 2008. The control group consists of matched workers employed at establishments located within the 50--80 km control ring. Worker and year fixed effects are included in all estimations. Standard errors clustered at the census-tract level are in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.",
  "      \\end{tablenotes}",
  "    \\end{threeparttable}",
  "  }",
  "\\end{table}"
)

out_path_tex <- file.path(tables_dir, "Tab_06_Dismissed_Workers_5km_tract.tex")
writeLines(lines, out_path_tex)
log_msg("Saved table: %s", out_path_tex)

# Tidy CSV companion.
tab_trend <- gen_table(output_trend) %>% dplyr::select(-dplyr::any_of("log Wage"))
out_path_csv <- file.path(tables_dir, "Tab_06_Dismissed_Workers_5km_tract.csv")
write.csv(tab_trend, out_path_csv, row.names = FALSE)
log_msg("Saved table: %s", out_path_csv)

log_msg("=== 08_table6_dismissed_workers.R: done ===")
