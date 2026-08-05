# ============================================================================
# Appendix E, Table E.1 — Comparison of Variables Before and After Data
# Balancing
# Replicates output_empregados()'s PSM step (0-5 km treatment vs. 50-80 km
# control, 2007 cross-section) independently, to expose the pre- and
# post-matching samples for a balance check. Does not modify
# worker_functions.R. Uses `dados_main` from 00b_load_worker_data.R.
# ============================================================================

log_msg("=== 01_table_E1_workers_balancing.R: start ===")

dados2 <- dados_main %>% dplyr::filter(year == 2007)
dados3 <- dados2 %>%
  dplyr::filter(dplyr::between(dist_flood, WORKER_MIN_TREAT, WORKER_MAX_TREAT) |
                  dplyr::between(dist_flood, WORKER_MIN_CTRL, WORKER_MAX_CTRL)) %>%
  dplyr::mutate(
    treat   = ifelse(ano_morte == 2008 & dplyr::between(dist_flood, WORKER_MIN_TREAT, WORKER_MAX_TREAT), 1, 0),
    control = ifelse(dplyr::between(dist_flood, WORKER_MIN_CTRL, WORKER_MAX_CTRL), 1, 0)
  ) %>%
  dplyr::filter(treat == 1 | control == 1)

dados4 <- dados3 %>%
  dplyr::mutate(
    educ_d1 = ifelse(grau_instr %in% c("1", "2", "3"), 1, 0),
    educ_d2 = ifelse(grau_instr %in% c("4", "5"), 1, 0),
    educ_d3 = ifelse(grau_instr %in% c("6", "7"), 1, 0),
    educ_d4 = ifelse(grau_instr %in% c("8", "9", "10", "11"), 1, 0),
    male    = ifelse(genero == "1", 1, 0),
    idade = as.numeric(idade), grau_instr = as.numeric(grau_instr),
    rem_med_r = as.numeric(rem_med_r), rem_dez_r = as.numeric(rem_dez_r),
    codemun = as.numeric(codemun), temp_empr = as.numeric(temp_empr),
    tamestab = as.numeric(tamestab), subs_ibge = as.numeric(subs_ibge),
    horas_contr = as.numeric(horas_contr),
    Construcao  = ifelse(subs_ibge == 15, 1, 0),
    Industria   = ifelse(subs_ibge %in% 3:13, 1, 0),
    Comercio    = ifelse(subs_ibge %in% c(16, 17), 1, 0),
    servicos    = ifelse(subs_ibge == 21, 1, 0),
    transporte  = ifelse(subs_ibge == 20, 1, 0),
    trabalhador_fixo       = ifelse(tipo_sintetizado %in% c("CLT", "Estatutário"), 1, 0),
    trabalhador_temporario = ifelse(tipo_sintetizado == "Trabalho Temporário", 1, 0),
    trabalhador_outros     = ifelse(tipo_sintetizado %in% c("Outros", "Trabalho Avulso"), 1, 0)
  )

psm <- MatchIt::matchit(
  treat ~ idade + rem_med_r + temp_empr + tamestab,
  method = "nearest",
  exact  = c("educ_d1", "educ_d2", "educ_d3", "educ_d4", "male",
             "Construcao", "Industria", "Comercio", "servicos", "transporte",
             "trabalhador_fixo", "trabalhador_temporario", "trabalhador_outros"),
  replace = FALSE,
  data    = dados4
)
m.data <- MatchIt::match.data(psm)
log_msg("PSM: %d treated, %d control (unmatched); %d treated, %d control (matched)",
        sum(dados4$treat == 1), sum(dados4$treat == 0),
        sum(m.data$treat == 1), sum(m.data$treat == 0))

vars <- c(
  "Establishment Size" = "tamestab", "Average Working Hours" = "horas_contr",
  "Wage Value" = "rem_med_r", "Age" = "idade", "Gender Dummy (Male)" = "male",
  "Incomplete Primary Education" = "educ_d1", "Complete Primary Education" = "educ_d2",
  "Complete High School" = "educ_d3", "Higher Education" = "educ_d4",
  "Retail and Wholesale" = "Comercio", "Construction" = "Construcao",
  "Manufacturing" = "Industria", "Other Sectors" = "servicos", "Transport" = "transporte"
)

balance_row <- function(varname) {
  u1 <- dados4[[varname]][dados4$treat == 1]; u0 <- dados4[[varname]][dados4$treat == 0]
  m1 <- m.data[[varname]][m.data$treat == 1]; m0 <- m.data[[varname]][m.data$treat == 0]
  tt_u <- t.test(u1, u0); tt_m <- t.test(m1, m0)
  star <- function(p) dplyr::case_when(p <= 0.01 ~ "***", p <= 0.05 ~ "**", p <= 0.10 ~ "*", TRUE ~ "")
  list(
    treated_u = mean(u1, na.rm = TRUE), control_u = mean(u0, na.rm = TRUE),
    att = mean(u1, na.rm = TRUE) - mean(u0, na.rm = TRUE), att_star = star(tt_u$p.value),
    treated_m = mean(m1, na.rm = TRUE), control_m = mean(m0, na.rm = TRUE),
    diff = mean(m1, na.rm = TRUE) - mean(m0, na.rm = TRUE), diff_star = star(tt_m$p.value)
  )
}

results <- lapply(vars, balance_row)
names(results) <- names(vars)

fmt2 <- function(x) sprintf("%.2f", x)
row_line <- function(label, v) sprintf(
  "    %s & %s & & %s & & %s%s & & %s & & %s & & %s%s \\\\",
  label, fmt2(v$treated_u), fmt2(v$control_u), fmt2(v$att), v$att_star,
  fmt2(v$treated_m), fmt2(v$control_m), fmt2(v$diff), v$diff_star
)

general_vars   <- c("Establishment Size", "Average Working Hours", "Wage Value", "Age", "Gender Dummy (Male)")
education_vars <- c("Incomplete Primary Education", "Complete Primary Education", "Complete High School", "Higher Education")
sector_vars    <- c("Retail and Wholesale", "Construction", "Manufacturing", "Other Sectors", "Transport")

lines <- c(
  "\\begin{supptable}[H]", "  \\centering",
  "  \\caption{Comparison of Variables Before and After Data Balancing}",
  "  \\label{tab:matching}", "  \\scalebox{0.8}{", "  \\begin{threeparttable}",
  "    \\begin{tabular}{lccccccccccc}", "    \\toprule",
  "          & \\multicolumn{5}{c}{Unmatched Data} &       & \\multicolumn{5}{c}{Matched Data} \\\\",
  "    \\cmidrule{2-6}\\cmidrule{8-12}",
  "          & (1)   &       & (2)   &       & (3)   &       & (4)   &       & (5)   &       & (6) \\\\",
  "    \\cmidrule{2-2}\\cmidrule{4-4}\\cmidrule{6-6}\\cmidrule{8-8}\\cmidrule{10-10}\\cmidrule{12-12}",
  "          & Treated & & Control & & Diffence & & Treated & & Control & & Difference \\\\",
  "    \\midrule",
  "    \\multicolumn{12}{l}{\\textbf{General Characteristics}} \\\\",
  sapply(general_vars, function(v) row_line(v, results[[v]])),
  "    \\\\",
  "    \\multicolumn{12}{l}{\\textbf{Worker’s Education}} \\\\",
  sapply(education_vars, function(v) row_line(v, results[[v]])),
  "    \\\\",
  "    \\multicolumn{12}{l}{\\textbf{Employment Sector}} \\\\",
  sapply(sector_vars, function(v) row_line(v, results[[v]])),
  "    \\bottomrule", "    \\bottomrule", "    \\end{tabular}",
  "    \\begin{tablenotes}[flushleft]",
  "    \\small",
  "    \\textit{Notes:} This table reports mean worker characteristics for the treated and control groups in the baseline year 2007, before and after propensity score matching. Columns (3) and (6) report treated-minus-control mean differences before and after matching, respectively. *** p $<$ 0.01, ** p $<$ 0.05, * p $<$ 0.1",
  "    \\end{tablenotes}", "  \\end{threeparttable}", "  }", "\\end{supptable}"
)

out_path <- file.path(tables_dir, "Tab_E01_Workers_Balancing_5km_tract.tex")
writeLines(lines, out_path)
log_msg("Saved table: %s", out_path)

log_msg("=== 01_table_E1_workers_balancing.R: done ===")
