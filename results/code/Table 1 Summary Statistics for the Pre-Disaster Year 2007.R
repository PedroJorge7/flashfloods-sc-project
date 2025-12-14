# ============================================================================
# Table 1: Summary Statistics – Pre (2007) and Post (2008)
#   FIXES:
#   (1) Relocation from code_tract with correct timing (auto-matched to mover_ano_mun)
#   (2) Payroll in thousand BRL (massa_salarial/1000) -> matches paper scale
# ============================================================================

rm(list = ls())

library(dplyr)
library(tidyr)
library(haven)
library(knitr)
library(kableExtra)

options(scipen = 999)

# ---------------------------------------------------------------------------
# Helper: (same structure as your original)
# ---------------------------------------------------------------------------
perform_ttest <- function(data, var, group_var) {
  g1 <- data[[var]][data[[group_var]] == 1]
  g0 <- data[[var]][data[[group_var]] == 0]
  
  g1 <- g1[!is.na(g1)]
  g0 <- g0[!is.na(g0)]
  
  tt <- t.test(g1, g0)  # mantém como estava
  
  data.frame(
    variable     = var,
    mean_treated = mean(g1, na.rm = TRUE),
    sd_treated   = sd(g1,   na.rm = TRUE),
    mean_control = mean(g0, na.rm = TRUE),
    sd_control   = sd(g0,   na.rm = TRUE),
    mean_diff    = mean(g1, na.rm = TRUE) - mean(g0, na.rm = TRUE),
    p_value      = tt$p.value
  )
}

# ============================================================================
# 1) Load data + replicate Stata sample definition (treat_B etc.) – same as original
# ============================================================================
data <- haven::read_dta("./data/Natural Disastrer Santa Catarina - Dataset.dta")

data <- data %>%
  filter(year >= 2003 & year <= 2012) %>%
  arrange(id_estab, year) %>%
  group_by(id_estab) %>%
  mutate(
    treat_B = case_when(
      dist_flood <= 12.5                  ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE                                ~ NA_real_
    ),
    morte_orig         = morte,
    new_firm_orig      = new_firm,
    mover_ano_mun_orig = mover_ano_mun
  ) %>%
  mutate(
    morte    = if_else(is.na(treat_B), NA_real_, morte),
    new_firm = if_else(is.na(treat_B), NA_real_, new_firm)
  ) %>%
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
  mutate(
    mover_ano_mun = if_else(is.na(treat_B), NA_real_, mover_ano_mun)
  ) %>%
  ungroup()

if ("mover_ano_tract" %in% names(data)) {
  data <- data %>% mutate(mover_ano_tract = if_else(is.na(treat_B), NA_real_, mover_ano_tract))
}
if ("mover_ano_cep" %in% names(data)) {
  data <- data %>% mutate(mover_ano_cep = if_else(is.na(treat_B), NA_real_, mover_ano_cep))
}

# ============================================================================
# 1b) NEW relocation from code_tract
#     Build BOTH timings, then choose the one that matches mover_ano_mun best.
# ============================================================================
if (!("code_tract" %in% names(data))) {
  stop("Variável 'code_tract' não existe na base.")
}

data <- data %>%
  group_by(id_estab) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    ct      = as.character(code_tract),
    ct_lag  = dplyr::lag(ct,  1),
    ct_lead = dplyr::lead(ct, 1),
    
    # Timing A: move INTO year t (compare t vs t-1)
    reloc_tract_in = if_else(!is.na(ct) & !is.na(ct_lag)  & ct != ct_lag,  1, 0),
    
    # Timing B: move OUT of year t (compare t vs t+1)
    reloc_tract_out = if_else(!is.na(ct) & !is.na(ct_lead) & ct != ct_lead, 1, 0)
  ) %>%
  ungroup() %>%
  mutate(
    reloc_tract_in  = if_else(is.na(treat_B), NA_real_, as.numeric(reloc_tract_in)),
    reloc_tract_out = if_else(is.na(treat_B), NA_real_, as.numeric(reloc_tract_out))
  )

# --- choose timing (NO dplyr::if_else here; scalar selection) ---
if ("mover_ano_mun" %in% names(data)) {
  idx_in  <- !is.na(data$mover_ano_mun) & !is.na(data$reloc_tract_in)
  idx_out <- !is.na(data$mover_ano_mun) & !is.na(data$reloc_tract_out)
  
  match_in  <- if (any(idx_in))  mean(abs(data$mover_ano_mun[idx_in]  - data$reloc_tract_in[idx_in]))  else Inf
  match_out <- if (any(idx_out)) mean(abs(data$mover_ano_mun[idx_out] - data$reloc_tract_out[idx_out])) else Inf
  
  use_out <- is.finite(match_out) && (match_out <= match_in)
  
  data$reloc_tract <- if (use_out) data$reloc_tract_out else data$reloc_tract_in
  
  message(
    "[Relocation timing] Using ",
    ifelse(use_out, "OUT (t vs t+1)", "IN (t vs t-1)"),
    " | match_in=", round(match_in, 6),
    " | match_out=", round(match_out, 6)
  )
} else {
  # fallback: most common definition in panel is t vs t-1
  data$reloc_tract <- data$reloc_tract_in
  message("[Relocation timing] mover_ano_mun not found; defaulting to IN (t vs t-1).")
}

data <- data %>%
  select(-ct, -ct_lag, -ct_lead, -reloc_tract_in, -reloc_tract_out)

# ============================================================================
# 1c) Payroll scale (matches paper: thousand BRL)
# ============================================================================
data <- data %>%
  mutate(massa_salarial_th = massa_salarial / 1000)

# ============================================================================
# 2) Sector dummies – same as original
# ============================================================================
data <- data %>%
  mutate(
    Agriculture     = as.integer(subs_ibge == 25),
    Construction    = as.integer(subs_ibge == 15),
    Manufacturing   = as.integer(dplyr::between(subs_ibge, 3, 13)),
    RetailWholesale = as.integer(subs_ibge %in% c(16, 17)),
    OtherServices   = as.integer(subs_ibge %in% c(20, 21))
  )

# ============================================================================
# 3) Subsamples 2007 (pre) and 2008 (post)
# ============================================================================
pre_data  <- data %>% filter(year == 2007, !is.na(treat_B))
post_data <- data %>% filter(year == 2008, !is.na(treat_B))

# ============================================================================
# 4) Variables (UPDATED: relocation + payroll-thousand)
# ============================================================================
test_vars <- c(
  "morte",            # Establishment Closure
  "reloc_tract",      # Establishment Relocation (Census tract)
  "new_firm",         # Entry
  "empregados",       # Number of Employees
  "massa_salarial_th" # Payroll Value (thousand, in BRL)
)

sector_vars <- c(
  "Agriculture",
  "Construction",
  "Manufacturing",
  "RetailWholesale",
  "OtherServices"
)

var_labels <- c(
  morte             = "Establishment Closure",
  reloc_tract       = "Establishment Relocation",
  new_firm          = "Establishment Entry",
  empregados        = "Number of Employees",
  massa_salarial_th = "Payroll Value (thousand, in BRL)",
  Agriculture       = "Agriculture",
  Construction      = "Construction",
  Manufacturing     = "Manufacturing",
  RetailWholesale   = "Retail and Wholesale",
  OtherServices     = "Other Services"
)

var_order <- c(
  "morte", "reloc_tract", "new_firm",
  "empregados", "massa_salarial_th",
  "Agriculture", "Construction", "Manufacturing",
  "RetailWholesale", "OtherServices"
)

# ============================================================================
# 5) t-tests pre and post
# ============================================================================
pre_results <- lapply(test_vars,  function(v) perform_ttest(pre_data,  v, "treat_B")) %>% bind_rows()
pre_sector  <- lapply(sector_vars,function(v) perform_ttest(pre_data,  v, "treat_B")) %>% bind_rows()
pre_all_results <- bind_rows(pre_results, pre_sector) %>%
  mutate(variable = factor(variable, levels = var_order)) %>%
  arrange(variable)

post_results <- lapply(test_vars,  function(v) perform_ttest(post_data, v, "treat_B")) %>% bind_rows()
post_sector  <- lapply(sector_vars,function(v) perform_ttest(post_data, v, "treat_B")) %>% bind_rows()
post_all_results <- bind_rows(post_results, post_sector) %>%
  mutate(variable = factor(variable, levels = var_order)) %>%
  arrange(variable)

# ============================================================================
# 6) Format table (diff + stars)
# ============================================================================
add_stars <- function(df) {
  df %>%
    mutate(
      across(c(mean_treated, sd_treated, mean_control, sd_control, mean_diff),
             ~ round(., 3)),
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
      Variable         = var_label,
      `Mean (Treated)` = mean_treated,
      SD_Treated       = sd_treated,
      `Mean (Control)` = mean_control,
      SD_Control       = sd_control,
      Difference       = mean_diff_star
    )
}

pre_tab_df  <- add_stars(pre_all_results)
post_tab_df <- add_stars(post_all_results)

# ============================================================================
# 7) LaTeX outputs
# ============================================================================
pre_table <- pre_tab_df %>%
  knitr::kable(
    format    = "latex",
    booktabs  = TRUE,
    caption   = "Summary Statistics for the Pre-Disaster Year 2007",
    col.names = c("Variable",
                  "Mean (Treated)", "SD",
                  "Mean (Control)", "SD",
                  "Mean Diff.")
  ) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))

writeLines(as.character(pre_table), "./results/analysis/Tab_01_Summary_Statistics_Pre_Disaster.tex")

post_table <- post_tab_df %>%
  knitr::kable(
    format    = "latex",
    booktabs  = TRUE,
    caption   = "Summary Statistics for the Post-Disaster Year 2008",
    col.names = c("Variable",
                  "Mean (Treated)", "SD",
                  "Mean (Control)", "SD",
                  "Mean Diff.")
  ) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"))

writeLines(as.character(post_table), "./results/analysis/Tab_01_Summary_Statistics_Post_Disaster.tex")
