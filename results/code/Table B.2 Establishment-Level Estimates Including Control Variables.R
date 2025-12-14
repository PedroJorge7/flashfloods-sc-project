############################################################
## Appendix – Table B.2
## Establishment-Level Estimates Including Control Variables
## (R version)
############################################################

rm(list = ls())

library(dplyr)
library(haven)
library(fixest)
library(broom)
library(stringr)

# ---------------------------------------------------------
# 1) Carregar base (painel completo, sem cortar ano ainda)
# ---------------------------------------------------------

data <- haven::read_dta("./data/Natural Disastrer Santa Catarina - Dataset.dta") %>%
  arrange(id_estab, year)

# ---------------------------------------------------------
# 2) Replicar construção de tratamento do Stata
# ---------------------------------------------------------
# Stata:
# drop treat_*
# gen treat_B = 1 if dist_flood <= 12.5
# replace treat_B = 0 if inrange(dist_flood,50,80) & treat_B == .
# gen morte_orig = morte
# gen new_firm_orig = new_firm
# gen mover_ano_mun_orig = mover_ano_mun
# replace morte = .  if treat_B == .
# replace new_firm = .  if treat_B == .
# bysort id_estab (year): replace treat_B = treat_B[_n-1] if treat_B == . & mover_ano_mun == 1
# replace mover_ano_mun = .  if treat_B == .

data <- data %>%
  group_by(id_estab) %>%
  mutate(
    treat_B = case_when(
      dist_flood <= 12.5 ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE ~ NA_real_
    ),
    morte_orig         = morte,
    new_firm_orig      = new_firm,
    mover_ano_mun_orig = mover_ano_mun
  ) %>%
  # outcomes ficam NA onde treat_B é missing
  mutate(
    morte    = if_else(is.na(treat_B), NA_real_, morte),
    new_firm = if_else(is.na(treat_B), NA_real_, new_firm)
  ) %>%
  # imputação de treat_B dentro do estabelecimento se mover_ano_mun == 1
  mutate(
    treat_B = {
      tb  <- treat_B
      mov <- mover_ano_mun
      for (i in seq_along(tb)) {
        if (i > 1 && is.na(tb[i]) && !is.na(mov[i]) && mov[i] == 1) {
          tb[i] <- tb[i - 1]
        }
      }
      tb
    }
  ) %>%
  # mover_ano_mun também vira NA onde treat_B é missing
  mutate(
    mover_ano_mun = if_else(is.na(treat_B), NA_real_, mover_ano_mun)
  ) %>%
  ungroup()

if ("mover_ano_tract" %in% names(data)) {
  data <- data %>%
    mutate(mover_ano_tract = if_else(is.na(treat_B), NA_real_, mover_ano_tract))
}
if ("mover_ano_cep" %in% names(data)) {
  data <- data %>%
    mutate(mover_ano_cep = if_else(is.na(treat_B), NA_real_, mover_ano_cep))
}

# ---------------------------------------------------------
# 3) treat_B_agg, treat_trend e dummies 2008–2012
# ---------------------------------------------------------

data <- data %>%
  mutate(
    treat_B_agg = case_when(
      year >= 2008 & treat_B == 1 ~ 1,
      !is.na(treat_B)             ~ 0,
      TRUE                        ~ NA_real_
    ),
    treat_trend = if_else(year >= 2008, 1, 0)
  )

for (y in 2008:2012) {
  var <- paste0("treat_B_", y)
  data[[var]] <- dplyr::case_when(
    data$year == y & !is.na(data$treat_B) ~ data$treat_B,
    !is.na(data$treat_B) & data$year != y ~ 0,
    TRUE                                  ~ NA_real_
  )
}

# ---------------------------------------------------------
# 4) Construir variáveis de baseline (ano base 2007)
#    foreach i in afil size natjur subs_ibge rem_dez_r temp_empr
#                   idade male educ_d2 educ_d3 educ_d4 age
# ---------------------------------------------------------

baseline_vars <- c("afil", "size", "natjur", "subs_ibge",
                   "rem_dez_r", "temp_empr",
                   "idade", "male",
                   "educ_d2", "educ_d3", "educ_d4",
                   "age")

missing_baseline <- setdiff(baseline_vars, names(data))
if (length(missing_baseline) > 0) {
  stop("Variáveis ausentes na base para construir baseline (X2): ",
       paste(missing_baseline, collapse = ", "))
}

for (v in baseline_vars) {
  tmp <- ifelse(data$year == 2007, data[[v]], NA)
  v2 <- ave(tmp, data$id_estab,
            FUN = function(x) {
              if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
            })
  data[[paste0(v, "2")]] <- v2
}

# ---------------------------------------------------------
# 5) Cortar para 2003–2012 (mesmo horizonte da Tabela 3)
# ---------------------------------------------------------

data_tabB2 <- data %>%
  filter(year >= 2003 & year <= 2012)

# ---------------------------------------------------------
# 6) Estimar modelos – Painel A igual; Painel B com controles
# ---------------------------------------------------------

outcomes <- c("morte", "mover_ano_mun")

panelA <- list()
panelB <- list()

# dummies de tratamento 2008–2012
treat_vars <- paste0("treat_B_", 2008:2012, collapse = " + ")

# controles: i.year#c.X2  -> i(year, X2) no fixest
baseline_vars2 <- paste0(baseline_vars, "2")
control_terms  <- paste0("i(year, ", baseline_vars2, ")")
controls_rhs   <- paste(control_terms, collapse = " + ")

for (outcome in outcomes) {
  
  # ----- Painel A: agregado (sem controles extras) -----
  fml_A1 <- as.formula(paste0(outcome, " ~ treat_B_agg | id_estab + year"))
  fml_A2 <- as.formula(paste0(outcome, " ~ treat_B_agg | id_estab + year + treat_trend[code_tract]"))
  
  mA1 <- feols(
    fml_A1,
    data    = data_tabB2,
    cluster = ~ id_estab,
    lean    = TRUE
  )
  
  mA2 <- feols(
    fml_A2,
    data    = data_tabB2,
    cluster = ~ id_estab,
    lean    = TRUE
  )
  
  panelA[[outcome]] <- list(no_trend = mA1, trend = mA2)
  
  # ----- Painel B: dummies 2008–2012 + controles i(year, X2) -----
  rhs_B <- paste(c(treat_vars, controls_rhs), collapse = " + ")
  
  fml_B1 <- as.formula(paste0(outcome, " ~ ", rhs_B, " | id_estab + year"))
  fml_B2 <- as.formula(paste0(outcome, " ~ ", rhs_B, " | id_estab + year + treat_trend[code_tract]"))
  
  mB1 <- feols(
    fml_B1,
    data    = data_tabB2,
    cluster = ~ id_estab + year,
    lean    = TRUE
  )
  
  mB2 <- feols(
    fml_B2,
    data    = data_tabB2,
    cluster = ~ id_estab + year,
    lean    = TRUE
  )
  
  panelB[[outcome]] <- list(no_trend = mB1, trend = mB2)
}

mods_A <- list(
  panelA[["morte"]]$no_trend,
  panelA[["morte"]]$trend,
  panelA[["mover_ano_mun"]]$no_trend,
  panelA[["mover_ano_mun"]]$trend
)

mods_B <- list(
  panelB[["morte"]]$no_trend,
  panelB[["morte"]]$trend,
  panelB[["mover_ano_mun"]]$no_trend,
  panelB[["mover_ano_mun"]]$trend
)

# ---------------------------------------------------------
# 7) Funções auxiliares
# ---------------------------------------------------------

get_est <- function(model, term) {
  tt <- broom::tidy(model)
  out <- tt[tt$term == term, c("estimate", "std.error", "p.value")]
  if (nrow(out) == 0) stop(paste("Termo não encontrado no modelo:", term))
  out
}

stars <- function(p) {
  ifelse(p < 0.01, "***",
         ifelse(p < 0.05, "**",
                ifelse(p < 0.1, "*", "")))
}

fmt_coef <- function(est, p) sprintf("%.5f%s", est, stars(p))
fmt_se   <- function(se) sprintf("(%.5f)", se)
fmt_obs  <- function(n)  format(n, big.mark = ",", scientific = FALSE)

# ---------------------------------------------------------
# 8) Montar LaTeX – Table B.2
# ---------------------------------------------------------

lines <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Table B.2: Establishment-Level Estimates Including Control Variables}",
  "\\label{tab:B2_estab_controls}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & (1) & (2) & (3) & (4)\\\\",
  "\\midrule"
)

# ------------------ Painel A -----------------------------

lines <- c(lines,
           "\\multicolumn{5}{l}{\\textbf{Panel A: Time-Aggregated DiD}}\\\\",
           "Dep. Var. & \\multicolumn{2}{c}{Closure} & \\multicolumn{2}{c}{Relocation}\\\\",
           "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}"
)

rowA <- lapply(mods_A, get_est, term = "treat_B_agg")
coefA <- sapply(rowA, function(x) fmt_coef(x$estimate, x$p.value))
seA   <- sapply(rowA, function(x) fmt_se(x$std.error))

lines <- c(lines,
           sprintf("Flash Flood Post & %s & %s & %s & %s\\\\",
                   coefA[1], coefA[2], coefA[3], coefA[4]),
           sprintf(" & %s & %s & %s & %s\\\\",
                   seA[1], seA[2], seA[3], seA[4])
)

obsA <- sapply(mods_A, nobs)
obsA_str <- sapply(obsA, fmt_obs)

lines <- c(lines,
           "\\addlinespace",
           sprintf("Observations & %s & %s & %s & %s\\\\",
                   obsA_str[1], obsA_str[2], obsA_str[3], obsA_str[4]),
           "Census Tract Trend & No & Yes & No & Yes\\\\",
           "\\addlinespace"
)

# ------------------ Painel B -----------------------------

lines <- c(lines,
           "\\multicolumn{5}{l}{\\textbf{Panel B: Time-Varying DiD with Baseline Controls}}\\\\",
           "Dep. Var. & \\multicolumn{2}{c}{Closure} & \\multicolumn{2}{c}{Relocation}\\\\",
           "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}"
)

years_evt <- 2008:2012
terms_evt <- paste0("treat_B_", years_evt)

for (k in seq_along(years_evt)) {
  yr   <- years_evt[k]
  term <- terms_evt[k]
  
  rowB <- lapply(mods_B, get_est, term = term)
  coefB <- sapply(rowB, function(x) fmt_coef(x$estimate, x$p.value))
  seB   <- sapply(rowB, function(x) fmt_se(x$std.error))
  
  lines <- c(lines,
             sprintf("Flash Flood %d & %s & %s & %s & %s\\\\",
                     yr, coefB[1], coefB[2], coefB[3], coefB[4]),
             sprintf(" & %s & %s & %s & %s\\\\",
                     seB[1], seB[2], seB[3], seB[4])
  )
}

obsB <- sapply(mods_B, nobs)
obsB_str <- sapply(obsB, fmt_obs)

lines <- c(lines,
           "\\addlinespace",
           sprintf("Observations & %s & %s & %s & %s\\\\",
                   obsB_str[1], obsB_str[2], obsB_str[3], obsB_str[4]),
           "Census Tract Trend & No & Yes & No & Yes\\\\",
           "\\midrule",
           "\\multicolumn{5}{p{12cm}}{\\footnotesize Notes: This table shows establishment-level estimates from a difference-in-differences model for business closure (columns (1) and (2)) and relocation (columns (3) and (4)). Panel A presents results from a specification using a single post-treatment dummy. Panel B presents results from a specification with time-varying treatment dummies (2008--2012), additionally controlling for interactions between year dummies and a rich set of baseline firm characteristics (affiliation, size, legal status, sector, wage bill, tenure, age, gender and education composition). Establishment and year fixed effects are included in all estimations. Standard errors are clustered at the establishment and year level. *** p$<0.01$, ** p$<0.05$, * p$<0.1$.}\\\\",
           "\\bottomrule",
           "\\end{tabular}",
           "\\end{table}"
)

# ---------------------------------------------------------
# 9) Salvar LaTeX em arquivo
# ---------------------------------------------------------

outfile_B2 <- "Table_B2_Establishment_Level_Estimates_Including_Control_Variables.tex"
writeLines(lines, outfile_B2)
