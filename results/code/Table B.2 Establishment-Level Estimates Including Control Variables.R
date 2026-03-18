############################################################
## Robustez 4 – Table B.2 (Establishments) com CONTROLES
## - MANTÉM APENAS AS COLUNAS COM TENDÊNCIA (Census Tract Trend)
## - Replica Table 3 (mesma construção de variáveis e janela)
## - ONLY: morte e reloc_tract_tminus1 (SEM entry)
## - Adiciona controles: i(year, X2, ref=2007)
##   X2 = baseline por id_estab (2007; senão último <=2007; senão 0)
## - Controles NÃO podem mudar nobs (STOP se mudar)
## - Cluster 2-way: id_estab + year
## - Salva:
##   ./results/analysis/Table_B2_Establishment_Level_Estimates_Including_Control_Variables.tex
############################################################

rm(list = ls())

source('./results/code/path_utils.R')

library(dplyr)
library(haven)
library(fixest)
library(broom)

# ---------------------------------------------------------
# 0) Output
# ---------------------------------------------------------
OUT_DIR <- "./results/analysis"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

OUTFILE <- file.path(
  OUT_DIR,
  "Table_B2_Establishment_Level_Estimates_Including_Control_Variables.tex"
)

# ---------------------------------------------------------
# 1) Load data (NÃO corta antes de construir t-1)
# ---------------------------------------------------------
data <- haven::read_dta(data_path("Natural Disastrer Santa Catarina - Dataset.dta")) %>%
  filter(year >= 2003) %>%
  arrange(id_estab, year)

need <- c("id_estab","year","dist_flood","morte","mover_ano_mun","code_tract")
miss <- setdiff(need, names(data))
if (length(miss) > 0) stop("Faltam colunas na base: ", paste(miss, collapse = ", "))

# ---------------------------------------------------------
# 2) Replicar Table 3: tratamento + forward fill (Stata)
# ---------------------------------------------------------
data <- data %>%
  group_by(id_estab) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    treat_B = case_when(
      dist_flood <= 12.5 ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE ~ NA_real_
    ),
    morte         = as.numeric(haven::zap_labels(morte)),
    mover_ano_mun = as.numeric(haven::zap_labels(mover_ano_mun)),
    morte = if_else(is.na(treat_B), NA_real_, morte)
  ) %>%
  mutate(
    treat_B = {
      tb  <- treat_B
      mov <- mover_ano_mun
      for (i in seq_along(tb)) {
        if (i > 1 && is.na(tb[i]) && !is.na(mov[i]) && mov[i] == 1) tb[i] <- tb[i - 1]
      }
      tb
    }
  ) %>%
  mutate(
    mover_ano_mun = if_else(is.na(treat_B), NA_real_, mover_ano_mun)
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 2.1) Relocation = Census Tract, t-1 (igual Table 3)
# ---------------------------------------------------------
data <- data %>%
  group_by(id_estab) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    ct     = as.character(code_tract),
    ct_lag = lag(ct, 1),
    diff_tract = case_when(
      is.na(ct) | is.na(ct_lag) ~ NA_real_,
      ct != ct_lag              ~ 1,
      TRUE                      ~ 0
    ),
    reloc_tract_tminus1 = lead(diff_tract, 1)
  ) %>%
  ungroup() %>%
  mutate(
    reloc_tract_tminus1 = if_else(is.na(treat_B), NA_real_, reloc_tract_tminus1),
    reloc_tract_tminus1 = as.numeric(reloc_tract_tminus1)
  ) %>%
  select(-ct, -ct_lag, -diff_tract)

# 1º ano observado: se reloc==1 -> 0
data <- data %>%
  group_by(id_estab) %>%
  mutate(
    reloc_tract_tminus1 = if_else(
      year == min(year, na.rm = TRUE) & reloc_tract_tminus1 == 1,
      0, reloc_tract_tminus1
    )
  ) %>%
  ungroup()

# regra Table 3: NA reloc -> 0; se morte NA => reloc NA
data <- data %>%
  mutate(
    reloc_tract_tminus1 = if_else(is.na(reloc_tract_tminus1), 0, reloc_tract_tminus1),
    reloc_tract_tminus1 = if_else(is.na(morte), NA_real_, reloc_tract_tminus1)
  )

# ---------------------------------------------------------
# 3) Post agregado + dummies anuais + trend vars (igual Table 3)
# ---------------------------------------------------------
make_tract_num <- function(x) {
  x_chr <- as.character(x)
  x_chr[x_chr %in% c("", "NA")] <- NA_character_
  x_num <- suppressWarnings(as.numeric(x_chr))
  bad   <- !is.na(x_chr) & is.na(x_num)
  if (sum(!is.na(x_chr)) > 0 && mean(bad) > 0.2) x_num <- as.numeric(factor(x_chr))
  x_num
}

data <- data %>%
  mutate(
    treat_B_agg = case_when(
      year >= 2008 & treat_B == 1 ~ 1,
      !is.na(treat_B)             ~ 0,
      TRUE                        ~ NA_real_
    ),
    treat_trend    = if_else(year >= 2008, 1, 0),
    code_tract_num = make_tract_num(code_tract)
  )

for (y in 2008:2012) {
  v <- paste0("treat_B_", y)
  data[[v]] <- dplyr::case_when(
    data$year == y & !is.na(data$treat_B) ~ data$treat_B,
    !is.na(data$treat_B) & data$year != y ~ 0,
    TRUE                                  ~ NA_real_
  )
}

# janela Table 3
data_tab <- data %>% filter(year >= 2003 & year <= 2012)

# ---------------------------------------------------------
# 4) CONTROLES: baseline <=2007 por id_estab (SEM NA)
# ---------------------------------------------------------
controls_candidates <- c(
  "tamestab", "size", "natjur", "subs_ibge", "afil",
  "age", "idade", "male",
  "rem_dez_r", "rem_med_r", "temp_empr",
  "educ_d1", "educ_d2", "educ_d3", "educ_d4"
)

controls_use <- intersect(controls_candidates, names(data_tab))

baseline_scalar <- function(x, y){
  i07 <- which(y == 2007 & !is.na(x))
  if (length(i07) > 0) return(x[i07[1]])
  ipre <- which(y <= 2007 & !is.na(x))
  if (length(ipre) > 0) return(x[ipre[length(ipre)]])
  0
}

if (length(controls_use) > 0) {
  for (v in controls_use) {
    data_tab[[v]] <- suppressWarnings(as.numeric(haven::zap_labels(data_tab[[v]])))
  }
  
  data_tab <- data_tab %>%
    group_by(id_estab) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
      across(
        all_of(controls_use),
        ~ baseline_scalar(.x, year),
        .names = "{.col}2"
      )
    ) %>%
    ungroup() %>%
    mutate(
      across(ends_with("2"), ~ if_else(is.na(.x), 0, .x))
    )
}

controls_use2 <- paste0(controls_use, "2")
control_terms <- if (length(controls_use2) > 0) {
  paste0("i(year, ", controls_use2, ", ref = 2007)")
} else character(0)

rhs_A <- paste(c("treat_B_agg", control_terms), collapse = " + ")
rhs_B <- paste(c(paste0("treat_B_", 2008:2012), control_terms), collapse = " + ")

# ---------------------------------------------------------
# 5) APENAS MODELOS COM TENDÊNCIA (Trend)
# ---------------------------------------------------------
mA_cl_tr <- feols(as.formula(paste0("morte ~ ", rhs_A,
                                    " | id_estab + year + treat_trend[code_tract_num]")),
                  data = data_tab, cluster = ~ id_estab + year, lean = TRUE)

mA_rl_tr <- feols(as.formula(paste0("reloc_tract_tminus1 ~ ", rhs_A,
                                    " | id_estab + year + treat_trend[code_tract_num]")),
                  data = data_tab, cluster = ~ id_estab + year, lean = TRUE)

mB_cl_tr <- feols(as.formula(paste0("morte ~ ", rhs_B,
                                    " | id_estab + year + treat_trend[code_tract_num]")),
                  data = data_tab, cluster = ~ id_estab + year, lean = TRUE)

mB_rl_tr <- feols(as.formula(paste0("reloc_tract_tminus1 ~ ", rhs_B,
                                    " | id_estab + year + treat_trend[code_tract_num]")),
                  data = data_tab, cluster = ~ id_estab + year, lean = TRUE)

# ---------------------------------------------------------
# 5.1) CHECAGEM: nobs não pode mudar vs Table 3 (trend sem controles)
# ---------------------------------------------------------
mA_cl_tr0 <- feols(morte ~ treat_B_agg | id_estab + year + treat_trend[code_tract_num],
                   data = data_tab, cluster = ~ id_estab + year, lean = TRUE)

mA_rl_tr0 <- feols(reloc_tract_tminus1 ~ treat_B_agg | id_estab + year + treat_trend[code_tract_num],
                   data = data_tab, cluster = ~ id_estab + year, lean = TRUE)

treat_vars0 <- paste0("treat_B_", 2008:2012, collapse = " + ")
mB_cl_tr0 <- feols(as.formula(paste0("morte ~ ", treat_vars0,
                                     " | id_estab + year + treat_trend[code_tract_num]")),
                   data = data_tab, cluster = ~ id_estab + year, lean = TRUE)

mB_rl_tr0 <- feols(as.formula(paste0("reloc_tract_tminus1 ~ ", treat_vars0,
                                     " | id_estab + year + treat_trend[code_tract_num]")),
                   data = data_tab, cluster = ~ id_estab + year, lean = TRUE)

if (nobs(mA_cl_tr) != nobs(mA_cl_tr0) || nobs(mA_rl_tr) != nobs(mA_rl_tr0)) {
  stop("ERRO: controles mudaram o número de observações no Panel A (Trend). Isso NÃO pode acontecer.")
}
if (nobs(mB_cl_tr) != nobs(mB_cl_tr0) || nobs(mB_rl_tr) != nobs(mB_rl_tr0)) {
  stop("ERRO: controles mudaram o número de observações no Panel B (Trend). Isso NÃO pode acontecer.")
}

# ---------------------------------------------------------
# 6) Helpers p/ LaTeX
# ---------------------------------------------------------
get_est <- function(model, term) {
  tt <- broom::tidy(model)
  out <- tt[tt$term == term, c("estimate","std.error","p.value")]
  if (nrow(out) == 0) return(data.frame(estimate=NA_real_, std.error=NA_real_, p.value=NA_real_))
  out[1, , drop = FALSE]
}

stars <- function(p) {
  if (is.na(p)) ""
  else if (p < 0.01) "***"
  else if (p < 0.05) "**"
  else if (p < 0.1)  "*"
  else ""
}

fmt_coef <- function(est, p) if (is.na(est)) "" else sprintf("%.5f%s", est, stars(p))
fmt_se   <- function(se) if (is.na(se)) "" else sprintf("(%.5f)", se)
fmt_obs  <- function(n) gsub(",", "{,}", format(n, big.mark = ",", scientific = FALSE), fixed = TRUE)

pad <- "       "
row_gap2 <- function(label, v) {
  stopifnot(length(v) == 2)
  paste0(label, " & ", v[1], " &", pad, "& ", v[2], "\\\\")
}

# ---------------------------------------------------------
# 7) Build LaTeX (somente Trend)
# ---------------------------------------------------------
lines <- c(
  "\\begin{supptable}[H]",
  "  \\centering",
  "  \\tabcaption{Establishment-Level Estimates Including Control Variables (Trend Only)}",
  "  \\label{tab:b2_controls_trend}",
  "  \\scalebox{0.80}{",
  " \\begin{threeparttable}",
  "    \\begin{tabular}{lccc}",
  "    \\toprule",
  "          & (1)   &       & (2) \\\\",
  "\\cmidrule(lr){2-2}\\cmidrule(lr){4-4}",
  "    \\multicolumn{4}{l}{\\textbf{Panel A: Time-Agregatted DiD}}\\\\",
  "    Dep. Var: & Closure & & Relocation \\\\",
  "    \\midrule"
)

# Panel A: treat_B_agg (Trend)
rowA_cl <- get_est(mA_cl_tr, "treat_B_agg")
rowA_rl <- get_est(mA_rl_tr, "treat_B_agg")

coefA <- c(fmt_coef(rowA_cl$estimate, rowA_cl$p.value),
           fmt_coef(rowA_rl$estimate, rowA_rl$p.value))
seA   <- c(fmt_se(rowA_cl$std.error),
           fmt_se(rowA_rl$std.error))

lines <- c(lines,
           row_gap2("    Flash Flood Post", coefA),
           row_gap2("                     ", seA))

lines <- c(lines,
           row_gap2("    Observations", c(fmt_obs(nobs(mA_cl_tr)), fmt_obs(nobs(mA_rl_tr)))),
           row_gap2("    Census Tract Trend", c("Yes","Yes")),
           row_gap2("    Baseline Controls $\\times$ Year", c("Yes","Yes")),
           "    \\midrule",
           "    \\multicolumn{4}{l}{\\textbf{Panel B: Time-Varying DiD}}\\\\",
           "    Dep. Var: & Closure & & Relocation \\\\",
           "    \\midrule"
)

# Panel B: 2008–2012 (Trend)
for (yr in 2008:2012) {
  term <- paste0("treat_B_", yr)
  rowB_cl <- get_est(mB_cl_tr, term)
  rowB_rl <- get_est(mB_rl_tr, term)
  
  coefB <- c(fmt_coef(rowB_cl$estimate, rowB_cl$p.value),
             fmt_coef(rowB_rl$estimate, rowB_rl$p.value))
  seB   <- c(fmt_se(rowB_cl$std.error),
             fmt_se(rowB_rl$std.error))
  
  lines <- c(lines,
             row_gap2(sprintf("    Flash Flood %d", yr), coefB),
             row_gap2("                     ", seB))
}

lines <- c(lines,
           row_gap2("    Observations", c(fmt_obs(nobs(mB_cl_tr)), fmt_obs(nobs(mB_rl_tr)))),
           row_gap2("    Census Tract Trend", c("Yes","Yes")),
           row_gap2("    Baseline Controls $\\times$ Year", c("Yes","Yes")),
           "    \\bottomrule",
           "    \\end{tabular}%",
           "   \t\\begin{tablenotes}[flushleft] \\item \\small \\textit{Notes:} This table replicates the main specification (Table 3) but reports only the versions including census tract trends (treat\\_trend[code\\_tract\\_num]) and adds baseline controls interacted with year dummies (i(year, X$_{\\le 2007}$, ref=2007)). Baseline values are taken in 2007 when available; otherwise the last non-missing value observed up to 2007; remaining missing are set to 0 to avoid changing the estimation sample. Closure is measured by \\textit{morte}. Relocation is defined as \\textit{Census Tract, t-1}: $reloc\\_tract\\_tminus1(t)=1$ if the establishment changes census tract between $t$ and $t+1$ (constructed from \\texttt{code\\_tract} using a lead of the tract-change indicator), with the first observed year forced to have $reloc\\_tract\\_tminus1=0$ when it would otherwise be 1. Treatment radius is 0--12.5 km and control ring is 50--80 km. Establishment and year fixed effects in all models. Two-way clustered standard errors (establishment and year) in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.",
           "   \t\\end{tablenotes}",
           "   \t\\end{threeparttable}",
           "   \t}",
           "\\end{supptable}%"
)

writeLines(lines, OUTFILE)
