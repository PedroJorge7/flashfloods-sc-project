############################################################
## Table 6 – Hazard of Establishment Closure (Cox model)
## Espelhando o código Stata (stset / stcox)
############################################################

rm(list = ls())

library(dplyr)
library(haven)
library(survival)
library(knitr)
library(kableExtra)

# ---------------------------------------------------------
# 1) Carregar base
# ---------------------------------------------------------

data <- haven::read_dta("./data/Natural Disastrer Santa Catarina - Dataset.dta")

# ---------------------------------------------------------
# 2) Replicar preparação do Stata
# ---------------------------------------------------------
# drop __000000 (se existir) e ordenar
if ("__000000" %in% names(data)) {
  data <- data %>% select(-`__000000`)
}

data <- data %>%
  arrange(id_estab, year)

# keep if year >= 2003 & inrange(year,2003,2012)
data <- data %>%
  filter(year >= 2003 & year <= 2012)

# set new treat distance – MESMO CRITÉRIO DO STATA
data <- data %>%
  group_by(id_estab) %>%
  mutate(
    treat_B = case_when(
      dist_flood <= 12.5                     ~ 1,
      dist_flood >= 50 & dist_flood <= 80    ~ 0,
      TRUE                                   ~ NA_real_
    ),
    morte_orig         = morte,
    new_firm_orig      = new_firm,
    mover_ano_mun_orig = mover_ano_mun
  ) %>%
  # replace morte = . if treat_B == .
  mutate(
    morte    = if_else(is.na(treat_B), NA_real_, morte),
    new_firm = if_else(is.na(treat_B), NA_real_, new_firm)
  ) %>%
  # bysort id_estab (year): replace treat_B = treat_B[_n-1] if treat_B==. & mover_ano_mun==1
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
  # replace mover_ano_mun = . if treat_B == .
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

# treat_B_agg = 1 if year >= 2008 & treat_B == 1; else 0 se treat_B != .
data <- data %>%
  mutate(
    treat_B_agg = if_else(
      year >= 2008 & treat_B == 1, 1,
      if_else(!is.na(treat_B), 0, NA_real_)
    )
  )

# ---------------------------------------------------------
# 3) Controles de firma – replicando primeiro_ano + temp + egen max
# ---------------------------------------------------------

# primeiro_ano por firma
data <- data %>%
  group_by(id_estab) %>%
  mutate(primeiro_ano = min(year, na.rm = TRUE)) %>%
  ungroup()

# variável de comércio internacional (Importador/Exportador)
data <- data %>%
  mutate(
    international_tr = if_else(
      (Importador == 1 | Exportador == 1),
      1, 0,
      missing = 0
    )
  )

# helper para fazer: gen temp = x if year==primeiro_ano; by id: egen x2 = max(temp)
make_firstyear_max <- function(df, varname) {
  tmp <- df %>%
    mutate(
      temp = if_else(
        year == primeiro_ano,
        as.numeric(.data[[varname]]),
        NA_real_
      )
    ) %>%
    group_by(id_estab) %>%
    mutate(newvar = suppressWarnings(max(temp, na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(
      newvar = ifelse(is.infinite(newvar), NA_real_, newvar)
    ) %>%
    select(newvar)
  tmp$newvar
}

vars_controls <- c("empregados", "afil", "yr_abert",
                   "international_tr", "educ_d2", "educ_d3", "educ_d4")

for (v in vars_controls) {
  newname <- paste0(v, "2")
  data[[newname]] <- make_firstyear_max(data, v)
}

# ---------------------------------------------------------
# 4) Dados para o Cox – painel longo com intervals
#     stset year, failure(morte) id(id_estab)
# ---------------------------------------------------------

cox_data <- data %>%
  # stcox só usa obs com regressors e failure não-missing
  filter(!is.na(treat_B_agg)) %>%
  mutate(
    # counting process: tempo em anos (origem arbitrária)
    # aqui: tstart = year - min(year_i), tstop = tstart + 1
    min_year_firm = ave(year, id_estab, FUN = min),
    tstart = year - min_year_firm,
    tstop  = tstart + 1
  )

# ---------------------------------------------------------
# 5) Modelos Cox como no Stata
#     stcox treat_B_agg, strata(subs_ibge year) vce(cluster id_estab)
# ---------------------------------------------------------

cox1 <- coxph(
  Surv(tstart, tstop, morte) ~ treat_B_agg +
    strata(subs_ibge, year) +
    cluster(id_estab),
  data = cox_data,
  ties = "breslow"
)

cox2 <- coxph(
  Surv(tstart, tstop, morte) ~ treat_B_agg +
    empregados2 + afil2 + yr_abert2 +
    international_tr2 + educ_d22 + educ_d32 + educ_d42 +
    strata(subs_ibge, year) +
    cluster(id_estab),
  data = cox_data,
  ties = "breslow"
)

# ---------------------------------------------------------
# 6) Extrair coef, se robusta, p-valor, HR
# ---------------------------------------------------------

extract_treat <- function(model) {
  s <- summary(model)
  row <- s$coefficients["treat_B_agg", , drop = TRUE]
  coef <- as.numeric(row["coef"])
  se   <- as.numeric(row["robust se"])
  z    <- coef / se
  p    <- 2 * (1 - pnorm(abs(z)))
  hr   <- exp(coef)
  list(coef = coef, se = se, p = p, hr = hr)
}

star <- function(p) {
  if (is.na(p)) "" else if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.10) "*" else ""
}

fmt_coef <- function(x, p) sprintf("%.5f%s", x, star(p))
fmt_se   <- function(se)  sprintf("(%.5f)", se)

r1 <- extract_treat(cox1)
r2 <- extract_treat(cox2)

n_obs <- nrow(cox_data)

# ---------------------------------------------------------
# 7) Construir tabela no formato 4 colunas
# ---------------------------------------------------------

tab <- data.frame(
  row = c(
    "Flash Flood Post",
    "",
    "Observations",
    "Sector FE",
    "Year FE",
    "Firm Controls"
  ),
  `(1) Coefficient`  = c(
    fmt_coef(r1$coef, r1$p),
    fmt_se(r1$se),
    format(n_obs, big.mark = ",", scientific = FALSE),
    "Yes",
    "Yes",
    "No"
  ),
  `(1) Hazard Ratio` = c(
    fmt_coef(r1$hr, r1$p),  # HR puro; se quiser HR-1, troca aqui
    "",
    format(n_obs, big.mark = ",", scientific = FALSE),
    "Yes",
    "Yes",
    "No"
  ),
  `(2) Coefficient`  = c(
    fmt_coef(r2$coef, r2$p),
    fmt_se(r2$se),
    format(n_obs, big.mark = ",", scientific = FALSE),
    "Yes",
    "Yes",
    "Yes"
  ),
  `(2) Hazard Ratio` = c(
    fmt_coef(r2$hr, r2$p),
    "",
    format(n_obs, big.mark = ",", scientific = FALSE),
    "Yes",
    "Yes",
    "Yes"
  ),
  check.names = FALSE
)

# ---------------------------------------------------------
# 8) Exportar .tex (miolo da tabela)
# ---------------------------------------------------------

tex_tab <- knitr::kable(
  tab,
  format   = "latex",
  booktabs = TRUE,
  align    = "lcccc",
  caption  = "Hazard of Establishment Closure – Cox Proportional Hazards Model"
) %>%
  kableExtra::kable_styling(
    latex_options = c("hold_position", "scale_down")
  )

writeLines(as.character(tex_tab),
           "Tab_06_Hazard_Establishment_Closure.tex")
