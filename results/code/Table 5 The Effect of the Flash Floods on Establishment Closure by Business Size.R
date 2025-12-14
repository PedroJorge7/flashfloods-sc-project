############################################################
## Table 5 – The Effect of the Flash Floods on Establishment
##           Closure by Business Size (R version)
############################################################

rm(list = ls())

library(dplyr)
library(tidyr)
library(haven)
library(fixest)
library(broom)
library(stringr)

# ---------------------------------------------------------
# 1) Carregar base
# ---------------------------------------------------------

data <- haven::read_dta("./data/Natural Disastrer Santa Catarina - Dataset.dta")

# Manter 2003–2012
data <- data %>%
  filter(year >= 2003 & year <= 2012) %>%
  arrange(id_estab, year)

# ---------------------------------------------------------
# 2) Replicar construção de tratamento/fechamento
#    EXATAMENTE como na Figure 3 (que bateu com o Stata)
# ---------------------------------------------------------

data <- data %>%
  arrange(id_estab, year) %>%
  group_by(id_estab) %>%
  mutate(
    # critério de banda (0–12.5 km vs 50–80 km)
    treat_B = case_when(
      dist_flood <= 12.5 ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE ~ NA_real_
    ),
    morte_orig         = morte,
    new_firm_orig      = new_firm,
    mover_ano_mun_orig = mover_ano_mun
  ) %>%
  # outcomes = NA onde treat_B é missing
  mutate(
    morte    = if_else(is.na(treat_B), NA_real_, morte),
    new_firm = if_else(is.na(treat_B), NA_real_, new_firm)
  ) %>%
  # forward fill do tratamento só quando há mudança de município
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
  # relocação vira NA onde treat_B continua missing
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

# Tendência pós-choque
data <- data %>%
  mutate(
    treat_trend = if_else(year >= 2008, 1, 0)
  )

# Dummy agregada pós-choque (Flash Flood Post)
data <- data %>%
  mutate(
    treat_B_agg = case_when(
      year >= 2008 & treat_B == 1 ~ 1,
      !is.na(treat_B)             ~ 0,
      TRUE                        ~ NA_real_
    )
  )

# Dummies ano-específicas de tratamento (2003–2012)
for (y in 2003:2012) {
  v <- paste0("treat_B_", y)
  data[[v]] <- dplyr::case_when(
    data$year == y & !is.na(data$treat_B) ~ data$treat_B,
    !is.na(data$treat_B) & data$year != y ~ 0,
    TRUE                                  ~ NA_real_
  )
}

# ---------------------------------------------------------
# 3) Dummies de tamanho da firma – igual ao Stata
#    gen size_estab1 = 1 if empregado < 10
#    gen size_estab2 = 1 if inrange(empregado,10,50)
#    gen size_estab3 = 1 if empregado > 50
# (assumindo que "empregados" é o "empregado" do do-file)
# ---------------------------------------------------------

data <- data %>%
  mutate(
    size_estab1 = as.integer(empregados < 10),
    size_estab2 = as.integer(empregados >= 10 & empregados <= 50),
    size_estab3 = as.integer(empregados > 50)
  )

size_vars    <- c("size_estab1", "size_estab2", "size_estab3")
size_labels  <- c("Micro Business", "Small Business", "Medium \\& Large Business")

# ---------------------------------------------------------
# 4) Helpers de formatação
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
# 5) Estimações por tamanho – Closure (morte)
#    Painel A: Post (treat_B_agg)
#    Painel B: Flash Flood 2008–2012 (treat_B_2008 … treat_B_2012)
#    FE: id_estab + year + census trend (treat_trend[code_tract])
#    Cluster: id_estab + year
# ---------------------------------------------------------

panelA_coef_str <- c()
panelA_se_str   <- c()
panelA_n_str    <- c()

panelB_store <- list()
years_tv     <- 2008:2012

for (s in size_vars) {
  
  size_data <- data %>% filter(.data[[s]] == 1, !is.na(treat_B))
  
  # ----- Painel A: Flash Flood Post -----
  fmlA <- morte ~ treat_B_agg | id_estab + year + treat_trend[code_tract]
  modA <- feols(
    fmlA,
    data    = size_data,
    cluster = ~ id_estab + year,
    lean    = TRUE
  )
  tidA <- broom::tidy(modA)
  rowA <- tidA[tidA$term == "treat_B_agg", ]
  
  panelA_coef_str <- c(panelA_coef_str,
                       fmt_coef(rowA$estimate, rowA$p.value))
  panelA_se_str   <- c(panelA_se_str,
                       fmt_se(rowA$std.error))
  panelA_n_str    <- c(panelA_n_str,
                       fmt_n(modA$nobs))
  
  # ----- Painel B: Flash Flood 2008–2012 -----
  tv_terms <- paste0("treat_B_", years_tv, collapse = " + ")
  fmlB <- as.formula(
    paste0("morte ~ ", tv_terms,
           " | id_estab + year + treat_trend[code_tract]")
  )
  modB <- feols(
    fmlB,
    data    = size_data,
    cluster = ~ id_estab + year,
    lean    = TRUE
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

panelB_n_str <- sapply(size_vars, function(s) fmt_n(panelB_store[[s]]$nobs))

# ---------------------------------------------------------
# 6) Montar linhas LaTeX
# ---------------------------------------------------------

## Painel A
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

## Painel B
panelB_lines <- c()
for (yr in years_tv) {
  coef_vals <- sapply(size_vars, function(s) {
    df <- panelB_store[[s]]$df
    df$coef_str[df$year == yr]
  })
  se_vals <- sapply(size_vars, function(s) {
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
# 7) Escrever .tex no formato da Tabela 5
# ---------------------------------------------------------

latex_lines <- c(
  "\\begin{table}[htb!]",
  "  \\centering",
  "  \\tabcaption{The Effect of the Flash Floods on Establishment Closure by Business Size}",
  "  \\label{tab5: hetero_size}",
  "  \\scalebox{0.75}{",
  " \\begin{threeparttable}",
  "    \\begin{tabular}{lccccc}",
  "  \\toprule",
  " & (1)   &       & (2)   &       & (3) \\\\",
  "\\cmidrule(lr){2-2}\\cmidrule(lr){4-4}\\cmidrule(lr){6-6}",
  "\\multicolumn{6}{l}{\\textbf{Panel A: Time-Aggregated DiD}}\\\\",
  "Est. Size: & Micro Business &       & Small Business &       & Medium \\& Large Business \\\\",
  "\\midrule",
  lineA1,
  lineA2,
  lineA3,
  "\\midrule",
  "\\multicolumn{6}{l}{\\textbf{Panel B: Time-Varying DiD}}\\\\",
  "Est. Size: & Micro Business &       & Small Business &       & Medium \\& Large Business \\\\",
  "\\midrule",
  panelB_lines,
  lineB_obs,
  "    \\bottomrule",
  "    \\end{tabular}%",
  "   \\begin{tablenotes}[flushleft] \\item \\small Notes: This table presents estimates obtained through the differences-in-differences model for different sub-samples categorized by business size. Panel A presents results from a specification using a single post-treatment dummy, whereas Panel B presents results from a specification with time-varying treatment dummies. Establishment fixed effects, year fixed effects, and census tract trend are included in all estimations. Two-way clustered-robust standard errors at the establishment and year level are in parentheses. *** represents p $<$ 0.01, ** represents p $<$ 0.05, * represents p $<$ 0.1.",
  "   \\end{tablenotes}",
  "   \\end{threeparttable}",
  "   }",
  "\\end{table}%"
)


writeLines(latex_lines,
           "establishments/analysis/Tab_05_Effect_by_Business_Size.tex")
