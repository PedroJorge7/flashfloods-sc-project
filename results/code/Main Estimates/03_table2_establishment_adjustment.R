# ============================================================================
# Table 2 — The Effect of the Flash Floods on Establishments' Adjustment
# Treatment: 0-5 km vs. 50-80 km control. Clustering: census tract.
# Four columns: (1) Closure, no trend; (2) Closure, with trend;
# (3) Relocation, no trend; (4) Relocation, with trend.
# Depends on `main_panel`. res_closure/res_reloc (with-trend fits) are also
# used by Figure 4.
# ============================================================================

log_msg("=== 03_table2_establishment_adjustment.R: start ===")

res_closure <- fit_establishment_models(main_panel, "morte", context = "Table 2 / Closure / Trend")
res_reloc   <- fit_establishment_models(main_panel, "reloc_tract_tminus1", context = "Table 2 / Relocation / Trend")

res_closure_notrend <- fit_establishment_models(main_panel, "morte", fe_extra = "", fe_extra_event = "",
                                                 context = "Table 2 / Closure / No Trend")
res_reloc_notrend   <- fit_establishment_models(main_panel, "reloc_tract_tminus1", fe_extra = "", fe_extra_event = "",
                                                 context = "Table 2 / Relocation / No Trend")

col_sources <- list(
  list(res = res_closure_notrend, label = "Closure"),
  list(res = res_closure,         label = "Closure"),
  list(res = res_reloc_notrend,   label = "Relocation"),
  list(res = res_reloc,           label = "Relocation")
)
trend_row <- c("No", "Yes", "No", "Yes")

row_gap <- function(label_txt, vals) {
  inter <- as.vector(rbind(vals, rep("", length(vals))))
  paste0(label_txt, " & ", paste(inter, collapse = " & "), " \\\\")
}

post_terms <- lapply(col_sources, function(s) get_term(s$res$agg, "treat_B_agg"))
coefA <- sapply(post_terms, function(x) fmt_coef(x$estimate, x$p.value))
seA   <- sapply(post_terms, function(x) fmt_se(x$std.error))
nobsA <- sapply(col_sources, function(s) fmt_obs(s$res$nobs["agg"]))

lines <- c(
  "\\begin{table}[htb]", "  \\centering",
  "  \\tabcaption{The Effect of the Flash Floods on Establishments' Adjustment}",
  "  \\label{tab3: main_results}", "  \\scalebox{0.8}{", "  \\begin{threeparttable}",
  "    \\begin{tabular}{lccccccc}", "    \\toprule",
  "          & (1)   &       & (2)   &       & (3)   &       & (4) \\\\",
  "\\cmidrule(lr){2-2}\\cmidrule(lr){4-4}\\cmidrule(lr){6-6}\\cmidrule(lr){8-8}",
  "    \\multicolumn{8}{l}{\\textbf{Panel A: Time-Agregatted DiD}}\\\\",
  row_gap("    Dep. Var:", sapply(col_sources, function(s) s$label)),
  "   \\midrule",
  row_gap("    Flash Flood Post", coefA),
  row_gap("                     ", seA),
  row_gap("    Observations", nobsA),
  row_gap("    Census Tract Trend", trend_row),
  "    \\midrule",
  "    \\multicolumn{8}{l}{\\textbf{Panel B: Time-Varying DiD}}\\\\",
  row_gap("    Dep. Var:", sapply(col_sources, function(s) s$label)),
  "    \\midrule"
)

tv_years <- 2008:2012
for (yr in tv_years) {
  term <- paste0("treat_B_", yr)
  est  <- lapply(col_sources, function(s) get_term(s$res$timevar, term))
  coefB <- sapply(est, function(x) fmt_coef(x$estimate, x$p.value))
  seB   <- sapply(est, function(x) fmt_se(x$std.error))
  lines <- c(lines, row_gap(sprintf("    Flash Flood %d", yr), coefB), row_gap("                     ", seB))
}

nobsB <- sapply(col_sources, function(s) fmt_obs(s$res$nobs["timevar"]))
lines <- c(
  lines,
  row_gap("    Observations", nobsB),
  row_gap("    Census Tract Trend", trend_row),
  "    \\bottomrule", "    \\bottomrule", "    \\end{tabular}%",
  "    \\begin{tablenotes}[flushleft]",
  paste0("    \\item \\small Notes: This table shows estimates from a difference-in-differences model for the following outcomes: an indicator for business closure (columns (1) and (2)), and for spatial relocation (columns (3) and (4)). Panel A presents results from a specification using a single post-treatment dummy, whereas Panel B presents results from a specification with time-varying treatment dummies (equation (\\ref{eq.eq1})). The treatment radius ranges from 0 to 5 km to flood spots. The control ring is between 50-80 km from the flood spots. Establishment and year fixed effects are included in all estimations. The census-tract clustered standard errors are in parentheses. *** represents p $<$ 0.01, ** represents p $<$ 0.05, * represents p $<$ 0.1. "),
  "    \\end{tablenotes}", "  \\end{threeparttable}", "  }", "\\end{table}%"
)

out_path <- file.path(tables_dir, "Tab_02_Establishment_Adjustment_5km_tract.tex")
writeLines(lines, out_path)
log_msg("Saved table: %s", out_path)

log_msg("=== 03_table2_establishment_adjustment.R: done ===")
