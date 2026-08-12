# ============================================================================
# Appendix C, Table C.8 — Robustness of
# Establishment Closure to Alternative Definitions
# 08_table_C8_closure_alternative_definitions.R (replicated verbatim below),
# applied to `main_panel`.
#
# Four columns, all on the Closure outcome:
#   (1) Baseline    — closed in t if absent from RAIS in t+1 (`morte`).
#   (2) Persistent  — closed in t if absent in BOTH t+1 and t+2,
#   (3) Single-branch — (1), restricted to establishments with pre-disaster
#       (2007-or-latest-prior-year) afil == 1.
#   (4) Persistent, single-branch — combines (2) and (3).
#
# ============================================================================

log_msg("=== 08_table_C8_closure_alternative_definitions.R: start ===")

present_lookup <- raw_data %>%
  dplyr::distinct(id_estab, year) %>%
  dplyr::mutate(.present = TRUE)

closure_2yr_wide <- raw_data %>%
  dplyr::mutate(empregados_t = as.numeric(haven::zap_labels(empregados))) %>%
  dplyr::distinct(id_estab, year, empregados_t) %>%
  dplyr::mutate(.year_t1 = year + 1L, .year_t2 = year + 2L) %>%
  dplyr::left_join(
    present_lookup %>% dplyr::rename(.year_t1 = year, .present_t1 = .present),
    by = c("id_estab", ".year_t1")
  ) %>%
  dplyr::left_join(
    present_lookup %>% dplyr::rename(.year_t2 = year, .present_t2 = .present),
    by = c("id_estab", ".year_t2")
  ) %>%
  dplyr::mutate(
    .present_t1 = !is.na(.present_t1),
    .present_t2 = !is.na(.present_t2),
    closure_2yr = as.numeric(!.present_t1 & !.present_t2 & !is.na(empregados_t) & empregados_t > 0)
  ) %>%
  dplyr::select(id_estab, year, closure_2yr)

main_panel <- main_panel %>%
  dplyr::left_join(closure_2yr_wide, by = c("id_estab", "year")) %>%
  dplyr::mutate(closure_2yr = dplyr::if_else(is.na(treat_B), NA_real_, closure_2yr))

main_panel <- main_panel %>% dplyr::mutate(afil_num = as.numeric(haven::zap_labels(afil)))
main_panel$afil_baseline <- make_baseline_value(main_panel, "afil_num")

panel_single_estab <- main_panel %>% dplyr::filter(afil_baseline == 1)
log_msg("  Single-branch (afil_baseline==1) establishments: %d of %d",
        dplyr::n_distinct(panel_single_estab$id_estab), dplyr::n_distinct(main_panel$id_estab))

res_col1 <- fit_establishment_models(main_panel, "morte", context = "Table C.8 / Baseline")
res_col2 <- fit_establishment_models(main_panel, "closure_2yr", context = "Table C.8 / Persistent")
res_col3 <- fit_establishment_models(panel_single_estab, "morte", context = "Table C.8 / Single-branch")
res_col4 <- fit_establishment_models(panel_single_estab, "closure_2yr", context = "Table C.8 / Persistent, single-branch")

results_list <- list(res_col1, res_col2, res_col3, res_col4)
col_labels   <- c("Baseline ($t+1$)", "Persistent ($t+1$ and $t+2$)",
                   "Single-Branch Firms", "Persistent, Single-Branch")

row_gap <- function(label_txt, vals) {
  inter <- as.vector(rbind(vals, rep("", length(vals))))
  paste0(label_txt, " & ", paste(inter, collapse = " & "), " \\\\")
}

post_est <- lapply(results_list, function(r) get_term(r$agg, "treat_B_agg"))
coefA <- sapply(post_est, function(x) fmt_coef(x$estimate, x$p.value))
seA   <- sapply(post_est, function(x) fmt_se(x$std.error))

nobsA <- sapply(results_list, function(r) r$nobs["agg"])

lines <- c(
  "\\begin{table}[htb]", "  \\centering",
  "  \\tabcaption{Robustness of Establishment Closure to Alternative Definitions}",
  "  \\label{tab:closure_robustness_B9}",
  "  \\scalebox{0.66}{", "  \\begin{threeparttable}",
  "    \\begin{tabular}{lcccccccc}", "    \\toprule",
  "          & (1) & & (2) & & (3) & & (4) &  \\\\",
  "\\cmidrule(lr){2-2}\\cmidrule(lr){4-4}\\cmidrule(lr){6-6}\\cmidrule(lr){8-8}",
  row_gap("    Definition:", col_labels),
  "    \\midrule",
  "    \\multicolumn{9}{l}{\\textbf{Panel A: Time-Aggregated DiD}}\\\\",
  row_gap("    Flash Flood Post", coefA),
  row_gap("                     ", seA),
  "    \\midrule",
  "    \\multicolumn{9}{l}{\\textbf{Panel B: Time-Varying DiD}}\\\\"
)

tv_years <- 2008:2012
for (yr in tv_years) {
  term <- paste0("treat_B_", yr)
  est  <- lapply(results_list, function(r) get_term(r$timevar, term))
  coefB <- sapply(est, function(x) fmt_coef(x$estimate, x$p.value))
  seB   <- sapply(est, function(x) fmt_se(x$std.error))
  lines <- c(lines, row_gap(sprintf("    Flash Flood %d", yr), coefB), row_gap("                     ", seB))
}

nobsB <- sapply(results_list, function(r) r$nobs["timevar"])

lines <- c(
  lines, "    \\midrule",
  row_gap("    Observations", sapply(nobsB, fmt_obs)),
  "    \\bottomrule", "    \\bottomrule", "    \\end{tabular}%",
  "    \\begin{tablenotes}[flushleft]",
  "    \\item \\small Notes: Col. (1): baseline closure (absence from RAIS in $t+1$). Col. (2): persistent closure (absence in $t+1$ and $t+2$). Col. (3): Col. (1) sample restricted to establishments that were single-branch firms as of 2007, before the disaster. Col. (4): Col. (2) outcome on the Col. (3) sample. Establishment and year fixed effects and a census-tract linear trend are included in all estimations. Standard errors clustered at the census-tract level are in parentheses. *** p $<$ 0.01, ** p $<$ 0.05, * p $<$ 0.1.",
  "    \\end{tablenotes}", "  \\end{threeparttable}", "  }", "\\end{table}%"
)

out_path_tex <- file.path(tables_dir, "Tab_C08_Closure_Robustness_Alternative_Definitions.tex")
writeLines(lines, out_path_tex)
log_msg("Saved table: %s", out_path_tex)

csv_rows <- list()
for (i in seq_along(results_list)) {
  r <- results_list[[i]]
  agg_t <- get_term(r$agg, "treat_B_agg")
  csv_rows[[length(csv_rows) + 1]] <- data.frame(
    definition = col_labels[i], panel = "A_aggregated", term = "Flash Flood Post",
    estimate = agg_t$estimate, std_error = agg_t$std.error, p_value = agg_t$p.value,
    nobs = r$nobs["agg"], nclust = r$nclust["agg"]
  )
  for (yr in tv_years) {
    tv_t <- get_term(r$timevar, paste0("treat_B_", yr))
    csv_rows[[length(csv_rows) + 1]] <- data.frame(
      definition = col_labels[i], panel = "B_time_varying", term = paste0("Flash Flood ", yr),
      estimate = tv_t$estimate, std_error = tv_t$std.error, p_value = tv_t$p.value,
      nobs = r$nobs["timevar"], nclust = r$nclust["timevar"]
    )
  }
}
csv_out <- do.call(rbind, csv_rows)
out_path_csv <- file.path(tables_dir, "Tab_C08_Closure_Robustness_Alternative_Definitions.csv")
write.csv(csv_out, out_path_csv, row.names = FALSE)
log_msg("Saved table: %s", out_path_csv)

log_msg("=== 08_table_C8_closure_alternative_definitions.R: done ===")
