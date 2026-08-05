# ============================================================================
# Appendix F, Table F.1 — The Effect of the Flash Floods on Exposed Workers
# Treatment: 0-5 km vs. 50-80 km control. Clustering: census tract.
# `exposed_workers = TRUE` uses the full treated-area worker universe, not
# just workers dismissed in 2008. Employment outcome only. Four columns:
#   (1) All workers, no census-tract trend
#   (2) All workers, with census-tract trend
#   (3) High Skill workers, with trend (grau_instr >= 8 in 2007)
#   (4) Low Skill workers, with trend (grau_instr < 8 in 2007)
# Uses `dados_main` (2002-2012) from 00b_load_worker_data.R.
# ============================================================================

log_msg("=== 01_table_F1_exposed_workers.R: start ===")

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
  exposed_workers = TRUE, trend = FALSE, se_type = "cluster", cluster_formula = WORKER_CLUSTER_FORMULA
)

output_trend <- output_empregados(
  dados_main, WORKER_MIN_TREAT, WORKER_MAX_TREAT, WORKER_MIN_CTRL, WORKER_MAX_CTRL,
  exposed_workers = TRUE, trend = TRUE, se_type = "cluster", cluster_formula = WORKER_CLUSTER_FORMULA
)

output_high_trend <- output_empregados(
  dados_higher, WORKER_MIN_TREAT, WORKER_MAX_TREAT, WORKER_MIN_CTRL, WORKER_MAX_CTRL,
  exposed_workers = TRUE, trend = TRUE, se_type = "cluster", cluster_formula = WORKER_CLUSTER_FORMULA
)

output_low_trend <- output_empregados(
  dados_lower, WORKER_MIN_TREAT, WORKER_MAX_TREAT, WORKER_MIN_CTRL, WORKER_MAX_CTRL,
  exposed_workers = TRUE, trend = TRUE, se_type = "cluster", cluster_formula = WORKER_CLUSTER_FORMULA
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

row_gap <- function(label_txt, vals) {
  inter <- as.vector(rbind(vals, rep("", length(vals))))
  paste0(label_txt, " & ", paste(inter, collapse = " & "), " \\\\")
}

post_terms <- lapply(col_sources, function(s) get_worker_term(s$df, s$reg, "Flash Flood Post"))
coefA <- sapply(post_terms, function(x) fmt_coef(x$estimate, x$p))
seA   <- sapply(post_terms, function(x) fmt_se(x$se))
nobsA <- sapply(post_terms, function(x) x$n)

lines <- c(
  "\\begin{table}[htb]", "  \\centering",
  "  \\tabcaption{The Effect of the Flash Floods on Exposed Workers}",
  "  \\label{tab:ExposedWorkers}", "  \\scalebox{0.65}{", "  \\begin{threeparttable}",
  "    \\begin{tabular}{lcccccccc}", "    \\toprule",
  "          & (1) & & (2) & & (3) & & (4) &  \\\\",
  "\\cmidrule(lr){2-2}\\cmidrule(lr){4-4}\\cmidrule(lr){6-6}\\cmidrule(lr){8-8}",
  row_gap("    Dep. Var:", rep("Employment", 4)),
  row_gap("    Sample:", c("All Workers", "All Workers", "High Skill", "Low Skill")),
  "    \\midrule",
  "    \\multicolumn{9}{l}{\\textbf{Panel A: Time-Aggregated DiD}}\\\\",
  row_gap("    Flash Flood Post", coefA),
  row_gap("                     ", seA)
)

tv_years <- 2008:2012
lines <- c(lines, "    \\midrule", "    \\multicolumn{9}{l}{\\textbf{Panel B: Time-Varying DiD}}\\\\")
for (yr in tv_years) {
  term_label <- paste0("Flash Flood ", yr)
  est <- lapply(col_sources, function(s) get_worker_term(s$df, s$reg, term_label))
  coefB <- sapply(est, function(x) fmt_coef(x$estimate, x$p))
  seB   <- sapply(est, function(x) fmt_se(x$se))
  lines <- c(lines, row_gap(sprintf("    Flash Flood %d", yr), coefB), row_gap("                     ", seB))
}

lines <- c(
  lines, "    \\midrule",
  row_gap("    Observations", sapply(nobsA, fmt_obs)),
  "    \\bottomrule", "    \\bottomrule", "    \\end{tabular}%",
  "    \\begin{tablenotes}[flushleft]",
  paste0("    \\small \\textit{Notes:} Panel A presents results from a specification using a single post-treatment dummy, whereas Panel B presents results from a specification with time-varying treatment dummies. ",
         "Columns (3) and (4) split the sample by workers’ education in 2007. Column (3) includes high-skill workers, defined as those with higher education, while column (4) includes low-skill workers, defined as those with less than higher education. ",
         "Worker fixed effects, year fixed effects, and a census-tract linear trend are included in all estimations. Treated workers are employed in establishments located within a 0–5 km spatial buffer around the flood spots. Control workers are matched workers employed in establishments located within the 50–80 km control ring. ",
         "Standard errors clustered at the census-tract level are in parentheses. *** p $<$ 0.01, ** p $<$ 0.05, * p $<$ 0.1."),
  "    \\end{tablenotes}", "  \\end{threeparttable}", "  }", "\\end{table}%"
)

out_path_tex <- file.path(tables_dir, "Tab_F01_Effect_Exposed_Workers_5km_tract.tex")
writeLines(lines, out_path_tex)
log_msg("Saved table: %s", out_path_tex)

log_msg("=== 01_table_F1_exposed_workers.R: done ===")
