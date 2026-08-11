# ============================================================================
# Table 1 — Summary Statistics for the Pre-Disaster Year 2007
# Treatment: 0-5 km vs. 50-80 km control. Depends on `main_panel`.
# ============================================================================

log_msg("=== 01_table1_summary_statistics.R: start ===")

perform_ttest <- function(data, var, group_var, cluster_var = "code_tract_num") {
  d <- data[!is.na(data[[var]]) & !is.na(data[[group_var]]) & !is.na(data[[cluster_var]]), ]
  g1 <- d[[var]][d[[group_var]] == 1]
  g0 <- d[[var]][d[[group_var]] == 0]

  fml <- as.formula(paste0(var, " ~ ", group_var))
  m   <- feols(fml, data = d, cluster = as.formula(paste0("~", cluster_var)))
  row <- broom::tidy(m)
  row <- row[row$term == group_var, ]

  data.frame(variable = var, mean_treated = mean(g1), sd_treated = sd(g1),
             mean_control = mean(g0), sd_control = sd(g0),
             mean_diff = mean(g1) - mean(g0), p_value = row$p.value,
             n_clust = dplyr::n_distinct(d[[cluster_var]]))
}

# Sector uses each establishment's earliest recorded subs_ibge, held fixed
# (matches Table 4's sector classification).
sector_earliest <- main_panel %>%
  dplyr::filter(!is.na(subs_ibge)) %>%
  dplyr::arrange(id_estab, year) %>%
  dplyr::group_by(id_estab) %>%
  dplyr::summarise(subs_ibge_fixed = dplyr::first(subs_ibge), .groups = "drop")

pre_data <- main_panel %>%
  filter(year == 2007, !is.na(treat_B)) %>%
  dplyr::left_join(sector_earliest, by = "id_estab") %>%
  mutate(
    Agriculture     = as.integer(subs_ibge_fixed == 25),
    Construction    = as.integer(subs_ibge_fixed == 15),
    Manufacturing   = as.integer(dplyr::between(subs_ibge_fixed, 3, 13)),
    RetailWholesale = as.integer(subs_ibge_fixed %in% c(16, 17)),
    OtherServices   = as.integer(subs_ibge_fixed %in% c(20, 21)),
    reloc_tract     = reloc_tract_tminus1
  )

main_vars   <- c("morte", "reloc_tract", "empregados")
sector_vars <- c("Agriculture", "Construction", "Manufacturing", "RetailWholesale", "OtherServices")
var_labels <- c(
  morte = "Establishment Closure", reloc_tract = "Establishment Relocation",
  empregados = "Number of Employees",
  Agriculture = "Agriculture", Construction = "Construction", Manufacturing = "Manufacturing",
  RetailWholesale = "Retail and Wholesale", OtherServices = "Other Services"
)

fmt_row <- function(var) {
  r <- perform_ttest(pre_data, var, "treat_B")
  stars <- dplyr::case_when(r$p_value <= 0.01 ~ "***", r$p_value <= 0.05 ~ "**", r$p_value <= 0.10 ~ "*", TRUE ~ "")
  sprintf("    %s & %s & %s & %s & %s & %s%s \\\\",
          var_labels[[var]], sprintf("%.3f", r$mean_treated), sprintf("%.3f", r$sd_treated),
          sprintf("%.3f", r$mean_control), sprintf("%.3f", r$sd_control), sprintf("%.3f", r$mean_diff), stars)
}

n_treated <- format(sum(pre_data$treat_B == 1, na.rm = TRUE), big.mark = ",")
n_control <- format(sum(pre_data$treat_B == 0, na.rm = TRUE), big.mark = ",")

lines <- c(
  "\\begin{table}[htb]", "  \\centering",
  "  \\tabcaption{Summary Statistics for the Pre-Disaster Year 2007}",
  "  \\label{tab: descritivas}", "  \\begin{threeparttable}",
  "    \\begin{tabular}{lccccc}", "    \\toprule",
  "          & \\multicolumn{2}{c}{Treated Units} & \\multicolumn{2}{c}{Control Units} & \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  "          & Mean & S.D. & Mean & S.D. & Mean Diff. \\\\",
  "    \\midrule",
  "    \\multicolumn{6}{l}{\\textbf{A. Main Variables}}\\\\",
  sapply(main_vars, fmt_row),
  "    \\midrule",
  "    \\multicolumn{6}{l}{\\textbf{B. Sectoral Composition (In \\%)}}\\\\",
  sapply(sector_vars, fmt_row),
  "    \\midrule",
  sprintf("    N. Observations & %s & & %s & & \\\\", n_treated, n_control),
  "    \\bottomrule", "    \\end{tabular}%",
  "    \\begin{tablenotes}[flushleft]",
  "    \\item \\small Note: The table displays the mean and standard deviation (S.D.) in the pre-disaster year for establishments separated by treated and control assignment. The last column presents the differences in means between the treatment and control groups and indicates their statistical significance according to a t-test. *** represents $p < 0.01$,** represents $p < 0.05$,* represents $p < 0.1$.,
  "    \\end{tablenotes}", "  \\end{threeparttable}", "\\end{table}%"
)

out_path <- file.path(tables_dir, "Tab_01_Summary_Statistics_5km_tract.tex")
writeLines(lines, out_path)
log_msg("Saved table: %s", out_path)

log_msg("=== 01_table1_summary_statistics.R: done ===")
