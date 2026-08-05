# ============================================================================
# Appendix B — Complementary Distance-Band Analysis of Establishment Closure
# Data-driven distance-band exercise: separate regressions for each 2.5 km
# ring from the flood spots (0-2.5, 2.5-5, 5-7.5, 7.5-10, 10-12.5, 12.5-15,
# 15-17.5 km), control ring fixed at 50-80 km, Closure outcome. Census-tract
# trend included. Clustering: census tract. Two panels: Panel A (aggregated
# post-treatment dummy) and Panel B (time-varying).
# Uses `raw_data` (each band needs its own panel build).
# ============================================================================

log_msg("=== 01_table_B1_distance_band_analysis.R: start ===")

band_specs <- list(
  "0-2.5 km"   = c(0, 2.5),
  "2.5-5 km"   = c(2.5, 5),
  "5-7.5 km"   = c(5, 7.5),
  "7.5-10 km"  = c(7.5, 10),
  "10-12.5 km" = c(10, 12.5),
  "12.5-15 km" = c(12.5, 15),
  "15-17.5 km" = c(15, 17.5)
)

results <- list()
for (band_label in names(band_specs)) {
  lo <- band_specs[[band_label]][1]; hi <- band_specs[[band_label]][2]
  treated_rule <- local({ lo_ <- lo; hi_ <- hi; function(d) dplyr::between(d, lo_, hi_) })
  band_tag <- gsub("[^A-Za-z0-9]+", "_", band_label)

  panel <- build_or_load_panel(
    file.path(cache_dir, sprintf("B1_panel_%s.rds", band_tag)),
    function() build_establishment_panel(raw_data, treated_rule, sprintf("Appendix B band=%s", band_label),
                                          control_rule = function(d) dplyr::between(d, 50, 80))
  )

  res <- fit_establishment_models(panel, "morte",
                                   context = sprintf("Appendix B / %s / Closure", band_label))
  results[[band_label]] <- res

  rm(panel)
  gc(full = TRUE)
}

n_col <- length(band_specs)
row_gap <- function(label_txt, vals) paste0(label_txt, " & ", paste(as.vector(rbind(vals, rep("", length(vals)))), collapse = " & "), " \\\\")

post_est <- lapply(results, function(r) get_term(r$agg, "treat_B_agg"))
coefA <- sapply(post_est, function(x) fmt_coef(x$estimate, x$p.value))
seA   <- sapply(post_est, function(x) fmt_se(x$std.error))
nobsA <- sapply(results, function(r) r$nobs["agg"])

lines <- c(
  "\\begin{table}[htb]", "  \\centering",
  "  \\tabcaption{Complementary Distance-Band Analysis of Establishment Closure}",
  "    \\label{tab:def_treat}",
  "    \\scalebox{0.7}{",
  " \\begin{threeparttable}",
  paste0("    \\begin{tabular}{l", strrep("c", 2 * n_col), "}"),
  "\\toprule",
  paste0("          & ", paste(as.vector(rbind(paste0("(", seq_len(n_col), ")"), rep("", n_col))), collapse = " & "), " \\\\"),
  paste(sapply(seq_len(n_col), function(i) sprintf("\\cmidrule(lr){%d-%d}", 2 * i, 2 * i)), collapse = ""),
  paste0("    \\multicolumn{", 2 * n_col + 1, "}{l}{\\textbf{Panel A: Time-Aggregated DiD}}\\\\"),
  row_gap("Treatment Group", names(band_specs)),
  row_gap("Control Group", rep("50–80 km", n_col)),
  "\\midrule",
  row_gap("Flash Flood Post", coefA),
  row_gap("                 ", seA),
  row_gap("Observations", sapply(nobsA, fmt_obs)),
  "\\midrule",
  paste0("    \\multicolumn{", 2 * n_col + 1, "}{l}{\\textbf{Panel B: Time-Varying DiD}}\\\\"),
  row_gap("Treatment Group", names(band_specs)),
  row_gap("Control Group", rep("50–80 km", n_col)),
  "\\midrule"
)

tv_years <- 2008:2012
for (yr in tv_years) {
  term <- paste0("treat_B_", yr)
  est  <- lapply(results, function(r) get_term(r$timevar, term))
  coefB <- sapply(est, function(x) fmt_coef(x$estimate, x$p.value))
  seB   <- sapply(est, function(x) fmt_se(x$std.error))
  lines <- c(lines, row_gap(sprintf("Flash Flood %d", yr), coefB), row_gap("                 ", seB))
}
nobsB <- sapply(results, function(r) r$nobs["timevar"])

lines <- c(
  lines,
  "\\midrule",
  row_gap("Observations", sapply(nobsB, fmt_obs)),
  "   \\bottomrule",
  "   \\bottomrule",
  "    \\end{tabular}%",
  "       \t\\begin{tablenotes}[flushleft] \\item \\small \\textit{Notes:} This table shows the estimation of the difference-in-differences model using business closure as the outcome variable. Each column uses a different contiguous 2.5 km treatment band, as indicated in the second row, while the control group remains fixed at 50--80 km from the flood spots. Panel A presents results from a specification using a single post-treatment dummy, whereas Panel B presents results from a specification with time-varying treatment dummies. Establishment fixed effects, year fixed effects, and a census-tract linear trend are included in all estimations. Standard errors clustered at the census-tract level are shown in parentheses. *** represents p $<$ 0.01,** represents p $<$ 0.05,* represents p $<$ 0.1.",
  "   \t\t\\end{tablenotes}",
  "   \t\t\\end{threeparttable}",
  "   \t\t}",
  "\\end{table}%"
)

out_path <- file.path(tables_dir, "Tab_B01_Distance_Band_Analysis_5km_tract.tex")
writeLines(lines, out_path)
log_msg("Saved table: %s", out_path)

log_msg("=== 01_table_B1_distance_band_analysis.R: done ===")
