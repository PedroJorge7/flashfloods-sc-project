# ============================================================================
# Table 1 - Summary Statistics for the Pre-Disaster Year 2007
# - Uses the establishment treatment definition from the main tables
# - Reports the pre-disaster balance table shown in the paper
# - Uses the relocation measure that matches the descriptive table in the paper
# ============================================================================

rm(list = ls())

source("./results/code/path_utils.R")

library(dplyr)
library(haven)
library(knitr)
library(kableExtra)

options(scipen = 999)

perform_ttest <- function(data, var, group_var) {
  g1 <- data[[var]][data[[group_var]] == 1]
  g0 <- data[[var]][data[[group_var]] == 0]

  g1 <- g1[!is.na(g1)]
  g0 <- g0[!is.na(g0)]

  tt <- t.test(g1, g0)

  data.frame(
    variable     = var,
    mean_treated = mean(g1, na.rm = TRUE),
    sd_treated   = sd(g1, na.rm = TRUE),
    mean_control = mean(g0, na.rm = TRUE),
    sd_control   = sd(g0, na.rm = TRUE),
    mean_diff    = mean(g1, na.rm = TRUE) - mean(g0, na.rm = TRUE),
    p_value      = tt$p.value
  )
}

data <- haven::read_dta(data_path("Natural Disastrer Santa Catarina - Dataset.dta")) %>%
  filter(year >= 2003 & year <= 2012) %>%
  arrange(id_estab, year) %>%
  mutate(
    treat_B = case_when(
      dist_flood <= 12.5 ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE ~ NA_real_
    ),
    morte = as.numeric(haven::zap_labels(morte)),
    mover_ano_mun = as.numeric(haven::zap_labels(mover_ano_mun)),
    massa_salarial_th = massa_salarial / 1000,
    Agriculture = as.integer(subs_ibge == 25),
    Construction = as.integer(subs_ibge == 15),
    Manufacturing = as.integer(dplyr::between(subs_ibge, 3, 13)),
    RetailWholesale = as.integer(subs_ibge %in% c(16, 17)),
    OtherServices = as.integer(subs_ibge %in% c(20, 21))
  )

if (!("code_tract" %in% names(data))) {
  stop("Variable 'code_tract' is not available in the dataset.")
}

data <- data %>%
  group_by(id_estab) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    ct = as.character(code_tract),
    ct_lag = dplyr::lag(ct, 1),
    ct_lead = dplyr::lead(ct, 1),
    reloc_tract_in = if_else(!is.na(ct) & !is.na(ct_lag) & ct != ct_lag, 1, 0),
    reloc_tract_out = if_else(!is.na(ct) & !is.na(ct_lead) & ct != ct_lead, 1, 0)
  ) %>%
  ungroup() %>%
  mutate(
    reloc_tract_in = if_else(is.na(treat_B), NA_real_, as.numeric(reloc_tract_in)),
    reloc_tract_out = if_else(is.na(treat_B), NA_real_, as.numeric(reloc_tract_out))
  )

idx_in <- !is.na(data$mover_ano_mun) & !is.na(data$reloc_tract_in)
idx_out <- !is.na(data$mover_ano_mun) & !is.na(data$reloc_tract_out)

match_in <- if (any(idx_in)) mean(abs(data$mover_ano_mun[idx_in] - data$reloc_tract_in[idx_in])) else Inf
match_out <- if (any(idx_out)) mean(abs(data$mover_ano_mun[idx_out] - data$reloc_tract_out[idx_out])) else Inf

data$reloc_tract <- if (is.finite(match_out) && (match_out <= match_in)) data$reloc_tract_out else data$reloc_tract_in

data <- data %>%
  select(-ct, -ct_lag, -ct_lead, -reloc_tract_in, -reloc_tract_out)

pre_data <- data %>% filter(year == 2007, !is.na(treat_B))

test_vars <- c(
  "morte",
  "reloc_tract",
  "empregados",
  "massa_salarial_th"
)

sector_vars <- c(
  "Agriculture",
  "Construction",
  "Manufacturing",
  "RetailWholesale",
  "OtherServices"
)

var_labels <- c(
  morte = "Establishment Closure",
  reloc_tract = "Establishment Relocation",
  empregados = "Number of Employees",
  massa_salarial_th = "Payroll Value (thousand, in BRL)",
  Agriculture = "Agriculture",
  Construction = "Construction",
  Manufacturing = "Manufacturing",
  RetailWholesale = "Retail and Wholesale",
  OtherServices = "Other Services"
)

var_order <- c(
  "morte",
  "reloc_tract",
  "empregados",
  "massa_salarial_th",
  "Agriculture",
  "Construction",
  "Manufacturing",
  "RetailWholesale",
  "OtherServices"
)

results_df <- bind_rows(
  lapply(test_vars, function(v) perform_ttest(pre_data, v, "treat_B")) %>% bind_rows(),
  lapply(sector_vars, function(v) perform_ttest(pre_data, v, "treat_B")) %>% bind_rows()
) %>%
  mutate(variable = factor(variable, levels = var_order)) %>%
  arrange(variable) %>%
  mutate(
    across(c(mean_treated, sd_treated, mean_control, sd_control, mean_diff), ~ round(., 3)),
    stars = case_when(
      p_value <= 0.01 ~ "***",
      p_value <= 0.05 ~ "**",
      p_value <= 0.10 ~ "*",
      TRUE            ~ ""
    ),
    mean_diff_star = paste0(mean_diff, stars),
    var_label = var_labels[as.character(variable)]
  ) %>%
  select(
    Variable = var_label,
    `Mean (Treated)` = mean_treated,
    SD_Treated = sd_treated,
    `Mean (Control)` = mean_control,
    SD_Control = sd_control,
    Difference = mean_diff_star
  ) %>%
  mutate(
    across(-Variable, as.character)
  )

results_df <- bind_rows(
  results_df,
  data.frame(
    Variable = "Observations",
    `Mean (Treated)` = format(sum(pre_data$treat_B == 1, na.rm = TRUE), big.mark = ",", scientific = FALSE),
    SD_Treated = "",
    `Mean (Control)` = format(sum(pre_data$treat_B == 0, na.rm = TRUE), big.mark = ",", scientific = FALSE),
    SD_Control = "",
    Difference = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
)

pre_table <- results_df %>%
  knitr::kable(
    format = "latex",
    booktabs = TRUE,
    caption = "Summary Statistics for the Pre-Disaster Year 2007",
    col.names = c(
      "Variable",
      "Mean (Treated)", "SD",
      "Mean (Control)", "SD",
      "Mean Diff."
    )
  ) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))

writeLines(as.character(pre_table), "./results/analysis/Tab_01_Summary_Statistics_Pre_Disaster.tex")
