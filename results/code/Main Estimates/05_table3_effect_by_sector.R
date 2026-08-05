# ============================================================================
# Table 3 — The Effect of the Flash Floods on Establishment Closure by Sector
# Treatment: 0-5 km vs. 50-80 km control. Clustering: census tract.
# Both Panel A (aggregated) and Panel B (time-varying) reported.
# Depends on `main_panel`.
#
# No census-tract trend: in these sector subsamples, most tracts are
# entirely treated or entirely control, so a tract-specific trend is
# identified off very few "mixed" tracts and destabilizes the estimates.
#
# Sector is each establishment's earliest recorded subsector code (subs_ibge),
# held fixed across its whole panel, since subs_ibge can change over time for
# ~10% of establishments.
# ============================================================================

log_msg("=== 05_table3_effect_by_sector.R: start ===")

sector_earliest <- main_panel %>%
  dplyr::filter(!is.na(subs_ibge)) %>%
  dplyr::arrange(id_estab, year) %>%
  dplyr::group_by(id_estab) %>%
  dplyr::summarise(subs_ibge_fixed = dplyr::first(subs_ibge), .groups = "drop")

log_msg("Earliest-observed subs_ibge available for %d/%d establishments",
        nrow(sector_earliest), dplyr::n_distinct(main_panel$id_estab))

sector_panel <- main_panel %>%
  dplyr::inner_join(sector_earliest, by = "id_estab") %>%
  dplyr::mutate(subs_ibge = subs_ibge_fixed) %>%
  add_sector_dummies()
sectors <- c(Construcao = "Construction", Transporte = "Transportation", Industria = "Manufacturing",
             Comercio = "Retail and Wholesale", Servicos = "Other Services")

fit_sector <- function(sector_var) {
  sub <- sector_panel %>% filter(.data[[sector_var]] == 1)
  fit_establishment_models(sub, "morte", fe_extra = "", fe_extra_event = "",
                            context = sprintf("Table 4 / %s", sectors[[sector_var]]))
}

results <- lapply(names(sectors), fit_sector)
names(results) <- names(sectors)

n_col <- length(sectors)
row_gap <- function(label_txt, vals) paste0(label_txt, " & ", paste(as.vector(rbind(vals, rep("", length(vals)))), collapse = " & "), " \\\\")

post_est <- lapply(results, function(r) get_term(r$agg, "treat_B_agg"))
coefA <- sapply(post_est, function(x) fmt_coef(x$estimate, x$p.value))
seA   <- sapply(post_est, function(x) fmt_se(x$std.error))
nobsA   <- sapply(results, function(r) r$nobs["agg"])

lines <- c(
  "\\begin{table}[htb]", "  \\centering",
  "  \\tabcaption{The Effect of the Flash Floods on Establishment Closure by Sector}",
  "  \\label{tab4: hetero_industry}", "  \\scalebox{0.7}{", "  \\begin{threeparttable}",
  paste0("    \\begin{tabular}{l", strrep("c", 2 * n_col), "}"),
  "    \\toprule",
  paste0("          & ", paste(as.vector(rbind(paste0("(", seq_len(n_col), ")"), rep("", n_col))), collapse = " & "), " \\\\"),
  paste0("    \\multicolumn{", 2 * n_col + 1, "}{l}{\\textbf{Panel A: Time-Aggregated DiD}}\\\\"),
  row_gap("    Sector:", unname(sectors)),
  "    \\midrule",
  row_gap("    Flash Flood Post", coefA),
  row_gap("                     ", seA),
  row_gap("    Observations", sapply(nobsA, fmt_obs)),
  "    \\midrule",
  paste0("    \\multicolumn{", 2 * n_col + 1, "}{l}{\\textbf{Panel B: Time-Varying DiD}}\\\\"),
  row_gap("    Sector:", unname(sectors))
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
  paste0("    \\item \\small Notes: This table presents estimates obtained through the differences-in-differences model for different sub-samples categorized by sector. Panel A presents results from a specification using a single post-treatment dummy, whereas Panel B presents results from a specification with time-varying treatment dummies (equation (\\ref{eq.eq1})). Establishment fixed effects, year fixed effects, and census tract trend are included in all estimations. The census-tract clustered standard errors are in parentheses. *** represents p $<$ 0.01, ** represents p $<$ 0.05, * represents p $<$ 0.1. "),
  "    \\end{tablenotes}", "  \\end{threeparttable}", "  }", "\\end{table}%"
)

out_path <- file.path(tables_dir, "Tab_03_Effect_by_Sector_5km_tract.tex")
writeLines(lines, out_path)
log_msg("Saved table: %s", out_path)

log_msg("=== 05_table3_effect_by_sector.R: done ===")
