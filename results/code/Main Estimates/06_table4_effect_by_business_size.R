# ============================================================================
# Table 4 — The Effect of the Flash Floods on Establishment Closure by
# Business Size
# Treatment: 0-5 km vs. 50-80 km control. Clustering: census tract.
# Both Panel A (aggregated) and Panel B (time-varying) reported.
# Depends on `main_panel`.
#
# No census-tract trend, same reasoning as Table 3: most tracts in these
# size subsamples are entirely treated or entirely control, which
# destabilizes a tract-specific trend.
# ============================================================================

log_msg("=== 06_table4_effect_by_business_size.R: start ===")

size_panel <- add_size_bins(main_panel)
sizes <- c(size_estab1 = "Micro Business", size_estab2 = "Small Business", size_estab3 = "Medium \\& Large Business")

fit_size <- function(size_var) {
  sub <- size_panel %>% filter(.data[[size_var]] == 1)
  fit_establishment_models(sub, "morte", fe_extra = "", fe_extra_event = "",
                            context = sprintf("Table 5 / %s", sizes[[size_var]]))
}

results <- lapply(names(sizes), fit_size)
names(results) <- names(sizes)

n_col <- length(sizes)
row_gap <- function(label_txt, vals) paste0(label_txt, " & ", paste(as.vector(rbind(vals, rep("", length(vals)))), collapse = " & "), " \\\\")

post_est <- lapply(results, function(r) get_term(r$agg, "treat_B_agg"))
coefA <- sapply(post_est, function(x) fmt_coef(x$estimate, x$p.value))
seA   <- sapply(post_est, function(x) fmt_se(x$std.error))
nobsA   <- sapply(results, function(r) r$nobs["agg"])

lines <- c(
  "\\begin{table}[htb]", "  \\centering",
  "  \\tabcaption{The Effect of the Flash Floods on Establishment Closure by Business Size}",
  "  \\label{tab5: hetero_size}", "  \\scalebox{0.75}{", "  \\begin{threeparttable}",
  paste0("    \\begin{tabular}{l", strrep("c", 2 * n_col), "}"),
  "    \\toprule",
  paste0("          & ", paste(as.vector(rbind(paste0("(", seq_len(n_col), ")"), rep("", n_col))), collapse = " & "), " \\\\"),
  paste0("    \\multicolumn{", 2 * n_col + 1, "}{l}{\\textbf{Panel A: Time-Aggregated DiD}}\\\\"),
  row_gap("    Est. Size:", unname(sizes)),
  "    \\midrule",
  row_gap("    Flash Flood Post", coefA),
  row_gap("                     ", seA),
  row_gap("    Observations", sapply(nobsA, fmt_obs)),
  "    \\midrule",
  paste0("    \\multicolumn{", 2 * n_col + 1, "}{l}{\\textbf{Panel B: Time-Varying DiD}}\\\\"),
  row_gap("    Est. Size:", unname(sizes))
)

tv_years <- 2008:2012
for (yr in tv_years) {
  term <- paste0("treat_B_", yr)
  est  <- lapply(results, function(r) get_term(r$timevar, term))
  coefB <- sapply(est, function(x) fmt_coef(x$estimate, x$p.value))
  seB   <- sapply(est, function(x) fmt_se(x$std.error))
  lines <- c(lines, row_gap(sprintf("    Flash Flood %d", yr), coefB), row_gap("                     ", seB))
}
nobsB <- sapply(results, function(r) r$nobs["timevar"])

lines <- c(
  lines,
  row_gap("    Observations", sapply(nobsB, fmt_obs)),
  "    \\bottomrule", "    \\end{tabular}%",
  "    \\begin{tablenotes}[flushleft]",
  paste0("    \\item \\small Notes: This table presents estimates obtained through the differences-in-differences model for different sub-samples categorized by business size. Panel A presents results from a specification using a single post-treatment dummy, whereas Panel B presents results from a specification with time-varying treatment dummies (equation (\\ref{eq.eq1})). Establishment fixed effects, year fixed effects, and census tract trend are included in all estimations. The census-tract clustered standard errors are in parentheses. *** represents p $<$ 0.01, ** represents p $<$ 0.05, * represents p $<$ 0.1. "),
  "    \\end{tablenotes}", "  \\end{threeparttable}", "  }", "\\end{table}%"
)

out_path <- file.path(tables_dir, "Tab_04_Effect_by_Business_Size_5km_tract.tex")
writeLines(lines, out_path)
log_msg("Saved table: %s", out_path)

log_msg("=== 06_table4_effect_by_business_size.R: done ===")
