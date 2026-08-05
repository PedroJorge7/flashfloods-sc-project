# ============================================================================
# Appendix A, Table A.1 — Municipal-Level Balance (0-5 km Treatment vs.
# 50-80 km Control)
# Compares geographic/climatic, demographic, economic, infrastructure, and
# industry-mix characteristics of the "nucleo" set of treatment and control
# municipalities (dominant establishment count within each ring), to show
# the two areas are comparable pre-disaster. Industry Mix shares are the
# share of establishments in each sector, computed from RAIS 2002 using the
# same subs_ibge classification as Table 1. Municipality 4208302 (core to
# both treatment and control) is excluded from the two-sample test.
# Reads the pre-built data/municipal_balance_5km_nucleo.rds (see
# "balance exercice/" for how that file's variables were originally sourced
# and merged from external geographic/climatic/socioeconomic data).
# ============================================================================

log_msg("=== 01_table_A1_municipal_balance.R: start ===")

base_muni <- readRDS(data_path("municipal_balance_5km_nucleo.rds"))

# Of the three credit files available in data/, BNDES disbursements provide
# the cleanest municipal-level pre-disaster measure: they are already
# aggregated by municipality/year, cover every municipality in this sample,
# and are reasonably balanced between the two rings.  We use the 2002-2007
# annual average so that a single unusually large operation does not drive
# the comparison. Values are expressed in constant BRL millions.
if (!requireNamespace("arrow", quietly = TRUE)) {
  stop("Package 'arrow' is required to read bndes_desembolsos_gold.parquet.")
}
bndes_pre <- arrow::read_parquet(
  data_path("bndes_desembolsos_gold.parquet"),
  col_select = c("cod_municipio", "ano", "desembolsos_reais_total")
) %>%
  dplyr::mutate(
    codigo_municipio = as.character(cod_municipio),
    ano = as.integer(ano),
    desembolsos_reais_total = as.numeric(desembolsos_reais_total)
  ) %>%
  dplyr::filter(ano >= 2002, ano <= 2007) %>%
  dplyr::group_by(codigo_municipio) %>%
  dplyr::summarise(
    bndes_desembolsos_medios_2002_2007 =
      mean(desembolsos_reais_total, na.rm = TRUE) / 1e6,
    .groups = "drop"
  )

base_muni <- base_muni %>%
  dplyr::left_join(bndes_pre, by = "codigo_municipio")

if (anyNA(base_muni$bndes_desembolsos_medios_2002_2007)) {
  missing_codes <- base_muni$codigo_municipio[
    is.na(base_muni$bndes_desembolsos_medios_2002_2007)
  ]
  stop(
    "BNDES disbursements are missing for municipal code(s): ",
    paste(missing_codes, collapse = ", ")
  )
}

municipios_ambos <- base_muni %>% dplyr::filter(grupo == "Ambos") %>% dplyr::pull(codigo_municipio)
if (length(municipios_ambos) > 0) {
  log_msg("Municipality(ies) 'Ambos' excluded from the test: %s", paste(municipios_ambos, collapse = ", "))
}
amostra <- base_muni %>%
  dplyr::filter(grupo %in% c("Tratamento", "Controle")) %>%
  dplyr::mutate(treat_B = as.integer(grupo == "Tratamento"))

geo_vars <- c(
  avg_tri = "Terrain Ruggedness Index",
  precipitacao_worldclim = "Annual Precipitation (mm)",
  temperatura_worldclim = "Mean Annual Temperature ($^\\circ$C)",
  altitude = "Mean Elevation (m)",
  share_area_water = "Water Area (\\% of Municipal Area)",
  dist_sede_rio_km = "Distance to Nearest River (km)"
)
demo_vars <- c(
  share_pop_urbana = "Urban Population (\\%)",
  prop_idosa_2000 = "Elderly Population (\\%)",
  share_pea = "Economically Active Population (\\%)",
  taxa_analfabetismo_2000 = "Illiteracy Rate (\\%)"
)
econ_vars <- c(
  prop_pobreza_2000 = "Poverty Rate (\\%)",
  renda_pc_2000 = "Per Capita Income (BRL)",
  indice_gini_2000 = "Gini Index",
  idhm_2000 = "Human Development Index",
  prop_ocupados_formalizacao_2000 = "Labor Formalization Rate (\\%)",
  bndes_desembolsos_medios_2002_2007 =
    "Average Annual BNDES Disbursements, 2002--2007 (BRL million)"
)
infra_vars <- c(
  dist_sede_rodovia_km = "Distance to Nearest Major Road (km)",
  taxa_agua_encanada_2000 = "Households with Piped Water (\\%)",
  taxa_energia_eletrica_2000 = "Households with Electricity (\\%)",
  taxa_coleta_lixo_2000 = "Households with Garbage Collection (\\%)"
)
industry_vars <- c(
  share_agriculture_2002 = "Agriculture Share (\\%)",
  share_construction_2002 = "Construction Share (\\%)",
  share_manufacturing_2002 = "Manufacturing Share (\\%)",
  share_retail_wholesale_2002 = "Retail and Wholesale Share (\\%)",
  share_other_services_2002 = "Other Services Share (\\%)"
)

panel_specs <- list(
  "Panel A: Geography and Climate"        = geo_vars,
  "Panel B: Demographic Characteristics"   = demo_vars,
  "Panel C: Socioeconomic Characteristics" = econ_vars,
  "Panel D: Infrastructure"                = infra_vars,
  "Panel E: Industry Composition"          = industry_vars
)

fmt_row <- function(var, label) {
  g1 <- amostra[[var]][amostra$treat_B == 1]; g1 <- g1[!is.na(g1)]
  g0 <- amostra[[var]][amostra$treat_B == 0]; g0 <- g0[!is.na(g0)]
  tt <- t.test(g1, g0)
  stars <- dplyr::case_when(tt$p.value <= 0.01 ~ "***", tt$p.value <= 0.05 ~ "**", tt$p.value <= 0.10 ~ "*", TRUE ~ "")
  sprintf("    %s & %s & %s & %s & %s & %s%s \\\\",
          label, sprintf("%.3f", mean(g1)), sprintf("%.3f", sd(g1)),
          sprintf("%.3f", mean(g0)), sprintf("%.3f", sd(g0)), sprintf("%.3f", mean(g1) - mean(g0)), stars)
}

panel_lines <- unlist(lapply(names(panel_specs), function(panel_title) {
  vars <- panel_specs[[panel_title]]
  c(sprintf("    \\multicolumn{6}{l}{\\textbf{%s}}\\\\", panel_title),
    sapply(names(vars), function(v) fmt_row(v, vars[[v]])),
    "    \\midrule")
}))
panel_lines <- panel_lines[-length(panel_lines)]  # drop trailing \midrule after the last panel

n_treated <- format(sum(amostra$treat_B == 1), big.mark = ",")
n_control <- format(sum(amostra$treat_B == 0), big.mark = ",")

lines <- c(
  "\\begin{table}[htb]", "  \\centering",
  "  \\tabcaption{Pre-Disaster Municipal Characteristics of the Treatment and Control Areas}",
  "  \\label{tab:municipal_balance}", "  \\scalebox{0.72}{", "  \\begin{threeparttable}",
  "    \\begin{tabular}{lccccc}", "    \\toprule",
  "          & \\multicolumn{2}{c}{Treated Municipalities} & \\multicolumn{2}{c}{Control Municipalities} & \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  "          & Mean & S.D. & Mean & S.D. & Mean Diff. \\\\",
  "    \\midrule",
  panel_lines,
  "    \\midrule",
  sprintf("    N. Municipalities & %s & & %s & & \\\\", n_treated, n_control),
  "    \\bottomrule", "    \\bottomrule", "    \\end{tabular}%",
  "    \\begin{tablenotes}[flushleft]",
  "    \\item \\small \\textit{Notes:} This table compares pre-disaster characteristics of municipalities overlapping the treatment and control areas. Municipalities are assigned to each group according to whether their territories overlap the corresponding treatment or control area. BNDES disbursements are the municipality-level annual average over 2002--2007, expressed in constant BRL millions. The final column reports the difference in means between treated and control municipalities. *** represents p $<$ 0.01, ** represents p $<$ 0.05, * represents p $<$ 0.1.",
  "    \\end{tablenotes}", "  \\end{threeparttable}", "  }", "\\end{table}%"
)

out_path <- file.path(tables_dir, "Tab_A01_Municipal_Balance_5km_Nucleo.tex")
writeLines(lines, out_path)
log_msg("Saved table: %s", out_path)

log_msg("=== 01_table_A1_municipal_balance.R: done ===")
