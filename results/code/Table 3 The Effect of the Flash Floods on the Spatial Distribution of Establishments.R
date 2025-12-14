############################################################
## Table 3 – The Effect of the Flash Floods on the Spatial
##            Distribution of Establishments (R version)
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
    # mesmo critério de banda do Stata
    treat_B = case_when(
      dist_flood <= 12.5 ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE ~ NA_real_
    ),
    morte_orig         = morte,
    new_firm_orig      = new_firm,
    mover_ano_mun_orig = mover_ano_mun
  ) %>%
  # outcomes ficam NA onde treat_B é missing (como no Stata)
  mutate(
    morte    = if_else(is.na(treat_B), NA_real_, morte),
    new_firm = if_else(is.na(treat_B), NA_real_, new_firm)
  ) %>%
  # replicar: by id_estab (year): replace treat_B = treat_B[_n-1] if treat_B==. & mover_ano_mun==1
  mutate(
    treat_B = {
      tb  <- treat_B
      mov <- mover_ano_mun       # usa mover_ano_mun "bruto", como no Stata
      for (i in seq_along(tb)) {
        if (i > 1 && is.na(tb[i]) && !is.na(mov[i]) && mov[i] == 1) {
          tb[i] <- tb[i - 1]
        }
      }
      tb
    }
  ) %>%
  # mover_ano_mun também vira NA onde treat_B continua missing (depois da imputação)
  mutate(
    mover_ano_mun = if_else(is.na(treat_B), NA_real_, mover_ano_mun)
  ) %>%
  ungroup()

# Se existirem essas variáveis na base, aplica o mesmo corte
if ("mover_ano_tract" %in% names(data)) {
  data <- data %>%
    mutate(mover_ano_tract = if_else(is.na(treat_B), NA_real_, mover_ano_tract))
}
if ("mover_ano_cep" %in% names(data)) {
  data <- data %>%
    mutate(mover_ano_cep = if_else(is.na(treat_B), NA_real_, mover_ano_cep))
}

# ---------------------------------------------------------
# 3) Replicar treat_B_agg, treat_B_2008-2012 e tendência
#    (ainda NO PAINEL COMPLETO)
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

# Só precisamos de 2008–2012 para a Tabela 3
for (y in 2008:2012) {
  var <- paste0("treat_B_", y)
  data[[var]] <- dplyr::case_when(
    data$year == y & !is.na(data$treat_B) ~ data$treat_B,
    !is.na(data$treat_B) & data$year != y ~ 0,
    TRUE                                  ~ NA_real_
  )
}

# ---------------------------------------------------------
# 4) Agora SIM cortar para 2003–2012 (como na Tabela 3)
# ---------------------------------------------------------

data_tab3 <- data %>%
  filter(year >= 2003 & year <= 2012)

# ---------------------------------------------------------
# 5) Estimar modelos equivalentes ao reghdfe do Stata
# ---------------------------------------------------------
# Painel A: DiD agregado (treat_B_agg)
#   (1),(2) morte
#   (3),(4) mover_ano_mun
#
# Painel B: DiD com dummies anuais 2008–2012
#   mesma ordem de desfechos, cluster(id_estab year)
#
# "Census Tract Trend" = absorb(... i.treat_trend#c.code_tract)
# em fixest: treat_trend[code_tract]

outcomes <- c("morte", "mover_ano_mun")

panelA <- list()
panelB <- list()

for (outcome in outcomes) {
  
  # ----- Painel A: agregado -----
  fml_A1 <- as.formula(paste0(outcome, " ~ treat_B_agg | id_estab + year"))
  fml_A2 <- as.formula(paste0(outcome, " ~ treat_B_agg | id_estab + year + treat_trend[code_tract]"))
  
  mA1 <- feols(
    fml_A1,
    data    = data_tab3,
    cluster = ~ id_estab,
    lean    = TRUE
  )
  
  mA2 <- feols(
    fml_A2,
    data    = data_tab3,
    cluster = ~ id_estab,
    lean    = TRUE
  )
  
  panelA[[outcome]] <- list(no_trend = mA1, trend = mA2)
  
  # ----- Painel B: dummies anuais 2008–2012 -----
  treat_vars <- paste0("treat_B_", 2008:2012, collapse = " + ")
  fml_B1 <- as.formula(paste0(outcome, " ~ ", treat_vars, " | id_estab + year"))
  fml_B2 <- as.formula(paste0(outcome, " ~ ", treat_vars, " | id_estab + year + treat_trend[code_tract]"))
  
  mB1 <- feols(
    fml_B1,
    data    = data_tab3,
    cluster = ~ id_estab + year,
    lean    = TRUE
  )
  
  mB2 <- feols(
    fml_B2,
    data    = data_tab3,
    cluster = ~ id_estab + year,
    lean    = TRUE
  )
  
  panelB[[outcome]] <- list(no_trend = mB1, trend = mB2)
}

# Ordenar modelos na mesma sequência da tabela:
# Colunas (1)–(4): Closure, Closure, Relocation, Relocation
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
# 6) Funções auxiliares (coef, se, p-valor, estrelas)
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
# 7) Montar linhas do LaTeX – só Closure e Relocation
# ---------------------------------------------------------

lines <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{The Effect of the Flash Floods on the Spatial Distribution of Establishments}",
  "\\label{tab:flash_floods_spatial}",
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

# Flash Flood Post (treat_B_agg)
rowA <- lapply(mods_A, get_est, term = "treat_B_agg")
coefA <- sapply(rowA, function(x) fmt_coef(x$estimate, x$p.value))
seA   <- sapply(rowA, function(x) fmt_se(x$std.error))

lines <- c(lines,
           sprintf("Flash Flood Post & %s & %s & %s & %s\\\\",
                   coefA[1], coefA[2], coefA[3], coefA[4]),
           sprintf(" & %s & %s & %s & %s\\\\",
                   seA[1], seA[2], seA[3], seA[4])
)

# Observações Painel A
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
           "\\multicolumn{5}{l}{\\textbf{Panel B: Time-Varying DiD}}\\\\",
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

# Observações Painel B
obsB <- sapply(mods_B, nobs)
obsB_str <- sapply(obsB, fmt_obs)

lines <- c(lines,
           "\\addlinespace",
           sprintf("Observations & %s & %s & %s & %s\\\\",
                   obsB_str[1], obsB_str[2], obsB_str[3], obsB_str[4]),
           "Census Tract Trend & No & Yes & No & Yes\\\\",
           "\\midrule",
           "\\multicolumn{5}{p{12cm}}{\\footnotesize Notes: This table shows estimates from a difference-in-differences model for the following outcomes: an indicator for business closure (columns (1) and (2)) and relocation (columns (3) and (4)). Panel A presents results from a specification using a single post-treatment dummy, whereas Panel B presents results from a specification with time-varying treatment dummies. Establishment and year fixed effects are included in all estimations. Standard errors are clustered at the establishment and year level. *** p$<0.01$, ** p$<0.05$, * p$<0.1$.}\\\\",
           "\\bottomrule",
           "\\end{tabular}",
           "\\end{table}"
)

# ---------------------------------------------------------
# 8) Salvar LaTeX em arquivo
# ---------------------------------------------------------

writeLines(lines, "Tab_03_Effect_Spatial_Distribution.tex")
cat("Tabela 3 salva em: Tab_03_Effect_Spatial_Distribution.tex\n")
