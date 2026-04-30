############################################################
## Table 4 - The Effect of the Flash Floods on Establishment
##           Closure by Sector (Painel A/B em .tex)
############################################################

rm(list = ls())

source("./results/code/path_utils.R")

library(dplyr)
library(tidyr)
library(haven)
library(fixest)
library(broom)
library(stringr)

fixest_startup_msg(FALSE)

# ---------------------------------------------------------
# 1) Carregar base
# ---------------------------------------------------------

data <- haven::read_dta(data_path("Natural Disastrer Santa Catarina - Dataset.dta"))

data <- data %>%
  filter(year >= 2003 & year <= 2012) %>%
  arrange(id_estab, year)

# ---------------------------------------------------------
# 2) Construir tratamento e variaveis de fechamento
# ---------------------------------------------------------

data <- data %>%
  arrange(id_estab, year) %>%
  group_by(id_estab) %>%
  mutate(
    # tratamento principal 0-12.5 vs 50-80
    treat_B = case_when(
      dist_flood <= 12.5 ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE ~ NA_real_
    ),
    morte_orig         = morte,
    new_firm_orig      = new_firm,
    mover_ano_mun_orig = mover_ano_mun
  ) %>%
  # Definir outcomes como missing fora das bandas
  mutate(
    morte    = if_else(is.na(treat_B), NA_real_, morte),
    new_firm = if_else(is.na(treat_B), NA_real_, new_firm)
  ) %>%
  # Propagar treat_B para relocalizacoes municipais
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
  # Definir mover_ano_mun como missing fora das bandas
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

# dummy agregada pos-choque
data <- data %>%
  mutate(
    treat_B_agg = case_when(
      year >= 2008 & treat_B == 1 ~ 1,
      !is.na(treat_B)             ~ 0,
      TRUE                        ~ NA_real_
    ),
    treat_trend = if_else(year >= 2008, 1, 0)
  )

# dummies ano-especificas
for (y in 2003:2012) {
  v <- paste0("treat_B_", y)
  data[[v]] <- dplyr::case_when(
    data$year == y & !is.na(data$treat_B) ~ data$treat_B,
    !is.na(data$treat_B) & data$year != y ~ 0,
    TRUE                                  ~ NA_real_
  )
}

# ---------------------------------------------------------
# 3) Dummies de setor
# ---------------------------------------------------------

data <- data %>%
  mutate(
    Construcao = as.integer(subs_ibge == 15),
    Transporte = as.integer(subs_ibge == 20),
    Industria  = as.integer(dplyr::between(subs_ibge, 3, 13)),
    Comercio   = as.integer(subs_ibge %in% c(16, 17, 20, 21)),
    Servicos   = as.integer(subs_ibge %in% c(20, 21))
  )

sector_vars <- c("Construcao", "Transporte", "Industria", "Comercio", "Servicos")

# ---------------------------------------------------------
# 4) Helpers de formatacao
# ---------------------------------------------------------

fmt_coef <- function(b, p) {
  base  <- sprintf("%.5f", b)
  stars <- ifelse(p < 0.01, "***",
                  ifelse(p < 0.05, "**",
                         ifelse(p < 0.10, "*", "")))
  paste0(base, stars)
}

fmt_se <- function(se) {
  paste0("(", sprintf("%.5f", se), ")")
}

fmt_n <- function(n) {
  s <- format(n, big.mark = ",", scientific = FALSE, trim = TRUE)
  gsub(",", "{,}", s)
}

join_cols <- function(vals) {
  cells <- character(0)
  for (k in seq_along(vals)) {
    cells <- c(cells, vals[k])
    if (k < length(vals)) cells <- c(cells, "")
  }
  paste(cells, collapse = " & ")
}

# ---------------------------------------------------------
# 5) Estimacoes por setor (Closure apenas)
# ---------------------------------------------------------

panelA_coef_str <- c()
panelA_se_str   <- c()
panelA_n_str    <- c()

panelB_store <- list()
years_tv     <- 2008:2012

for (s in sector_vars) {

  sector_data <- data %>% filter(.data[[s]] == 1)

  # Painel A: Flash Flood Post
  fmlA <- morte ~ treat_B_agg | id_estab + year
  modA <- feols(
    fmlA,
    data     = sector_data,
    vcov     = ~ id_estab,
    fixef.rm = "none",
    lean     = TRUE
  )
  tidA <- broom::tidy(modA)
  rowA <- tidA[tidA$term == "treat_B_agg", ]

  panelA_coef_str <- c(panelA_coef_str,
                       fmt_coef(rowA$estimate, rowA$p.value))
  panelA_se_str   <- c(panelA_se_str,
                       fmt_se(rowA$std.error))
  panelA_n_str    <- c(panelA_n_str,
                       fmt_n(modA$nobs))

  # Painel B: Flash Flood 2008-2012
  tv_terms <- paste0("treat_B_", years_tv, collapse = " + ")
  fmlB <- as.formula(
    paste0("morte ~ ", tv_terms, " | id_estab + year")
  )
  modB <- feols(
    fmlB,
    data     = sector_data,
    vcov     = ~ id_estab + year,
    fixef.rm = "none",
    lean     = TRUE
  )
  tidB <- broom::tidy(modB) %>%
    dplyr::filter(grepl("^treat_B_", term)) %>%
    mutate(
      year     = as.integer(sub("treat_B_", "", term)),
      coef_str = fmt_coef(estimate, p.value),
      se_str   = fmt_se(std.error)
    ) %>%
    arrange(year)

  panelB_store[[s]] <- list(
    df   = tidB,
    nobs = modB$nobs
  )
}

# nobs painel B
panelB_n_str <- sapply(sector_vars, function(s) fmt_n(panelB_store[[s]]$nobs))

# ---------------------------------------------------------
# 6) Construir linhas LaTeX
# ---------------------------------------------------------

# Painel A
lineA1 <- paste0(
  "    Flash Flood Post & ",
  join_cols(panelA_coef_str),
  " \\\\"
)
lineA2 <- paste0(
  "                     & ",
  join_cols(panelA_se_str),
  " \\\\"
)
lineA3 <- paste0(
  "    Observations & ",
  join_cols(panelA_n_str),
  " \\\\"
)

# Painel B
panelB_lines <- c()
for (yr in years_tv) {
  coef_vals <- sapply(sector_vars, function(s) {
    df <- panelB_store[[s]]$df
    df$coef_str[df$year == yr]
  })
  se_vals <- sapply(sector_vars, function(s) {
    df <- panelB_store[[s]]$df
    df$se_str[df$year == yr]
  })

  l1 <- paste0(
    "    Flash Flood ", yr, " & ",
    join_cols(coef_vals),
    " \\\\"
  )
  l2 <- paste0(
    "                     & ",
    join_cols(se_vals),
    " \\\\"
  )
  panelB_lines <- c(panelB_lines, l1, l2)
}

lineB_obs <- paste0(
  "    Observations & ",
  join_cols(panelB_n_str),
  " \\\\"
)

# ---------------------------------------------------------
# 8) Montar .tex e salvar
# ---------------------------------------------------------

latex_lines <- c(
  "\\begin{table}[htb!]",
  "  \\centering",
  "  \\tabcaption{The Effect of the Flash Floods on Establishment Closure by Sector}",
  "  \\label{tab4: hetero_industry}",
  "  \\scalebox{0.75}{",
  " \\begin{threeparttable}",
  "    \\begin{tabular}{lccccccccc}",
  "\\toprule",
  "      & (1)   &       & (2)   &       & (3)   &       & (4)   &       & (5) \\\\",
  "\\cmidrule(lr){2-2}\\cmidrule(lr){4-4}\\cmidrule(lr){6-6}\\cmidrule(lr){8-8}\\cmidrule(lr){10-10}",
  "\\multicolumn{10}{l}{\\textbf{Panel A: Time-Aggregated DiD}}\\\\",
  "Sector:      & Construction &       & Transportation &       & Manufacturing &       & Retail and Wholesale &       & Other Services \\\\",
  "\\midrule",
  lineA1,
  lineA2,
  lineA3,
  "\\midrule",
  "\\multicolumn{10}{l}{\\textbf{Panel B: Time-Varying DiD}}\\\\",
  "Sector:      & Construction &       & Transportation &       & Manufacturing &       & Retail and Wholesale &       & Other Services \\\\",
  "\\midrule",
  panelB_lines,
  lineB_obs,
  "\\bottomrule",
  "    \\end{tabular}%",
  "    \\begin{tablenotes}[flushleft] \\item \\small Notes: This table presents estimates obtained through the differences-in-differences model for different sub-samples categorized by sector. Panel A presents results from a specification using a single post-treatment dummy, whereas Panel B presents results from a specification with time-varying treatment dummies. Establishment fixed effects and year fixed effects are included in all estimations. Panel A uses establishment-clustered robust standard errors, while Panel B uses two-way clustered standard errors at the establishment and year level. *** represents p $<$ 0.01, ** represents p $<$ 0.05, * represents p $<$ 0.1.",
  "    \\end{tablenotes}",
  "    \\end{threeparttable}",
  "    }",
  "\\end{table}%"
)

writeLines(latex_lines,
           "results/analysis/Tab_04_Effect_by_Sector.tex")
