# ============================================================================
# Figure G.1: Sensitivity Analysis of the HonestDiD Approach
# Benchmark: TWFE estimate plus a sensitivity band based on pre-treatment
# event-study coefficients for establishment outcomes.
# Outputs:
#   - ./results/analysis/Fig_G1_Honest_DID_Results.png
#   - ./results/analysis/Fig_G1_Honest_DID_Bounds.csv
# ============================================================================

rm(list = ls())

source('./results/code/path_utils.R')

library(dplyr)
library(tidyr)
library(haven)
library(fixest)
library(ggplot2)
library(broom)

dir.create("./results/analysis", recursive = TRUE, showWarnings = FALSE)

carry_treatment_forward <- function(treat, mover) {
  out <- treat

  for (i in seq_along(out)) {
    if (i > 1 && is.na(out[i]) && !is.na(mover[i]) && mover[i] == 1) {
      out[i] <- out[i - 1]
    }
  }

  out
}

data <- haven::read_dta(data_path("Natural Disastrer Santa Catarina - Dataset.dta")) %>%
  filter(year >= 2003) %>%
  mutate(
    morte = as.numeric(haven::zap_labels(morte)),
    new_firm = as.numeric(haven::zap_labels(new_firm)),
    mover_ano_mun = as.numeric(haven::zap_labels(mover_ano_mun))
  ) %>%
  arrange(id_estab, year) %>%
  group_by(id_estab) %>%
  mutate(
    treat_B = case_when(
      dist_flood <= 12.5 ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE ~ NA_real_
    ),
    morte_orig = morte,
    new_firm_orig = new_firm,
    mover_ano_mun_orig = mover_ano_mun
  ) %>%
  mutate(
    morte = if_else(is.na(treat_B), NA_real_, morte_orig),
    new_firm = if_else(is.na(treat_B), NA_real_, new_firm_orig)
  ) %>%
  mutate(
    treat_B = carry_treatment_forward(treat_B, mover_ano_mun_orig),
    mover_ano_mun = if_else(is.na(treat_B), NA_real_, mover_ano_mun_orig)
  ) %>%
  ungroup() %>%
  filter(year >= 2003 & year <= 2012) %>%
  mutate(
    mover_ano_mun = if_else(is.na(mover_ano_mun), 0, mover_ano_mun),
    mover_ano_mun = if_else(is.na(morte), NA_real_, mover_ano_mun),
    treated = if_else(!is.na(treat_B), as.integer(treat_B == 1), NA_integer_),
    post = as.integer(year >= 2008)
  )

outcomes <- c("morte", "new_firm", "mover_ano_mun")
outcome_labels <- c(
  morte = "Closure",
  new_firm = "Entry",
  mover_ano_mun = "Relocation"
)

estimate_bounds <- function(df, outcome) {
  outcome_data <- df %>%
    mutate(outcome_value = as.numeric(haven::zap_labels(.data[[outcome]]))) %>%
    filter(!is.na(outcome_value))
  
  did_model <- feols(
    outcome_value ~ treated:post | id_estab + year,
    data = outcome_data,
    cluster = ~ id_estab,
    fixef.rm = "none",
    lean = TRUE
  )
  
  did_term <- broom::tidy(did_model) %>%
    filter(term == "treated:post")
  
  pre_model <- feols(
    outcome_value ~ i(year, treated, ref = 2007) | id_estab + year,
    data = outcome_data %>% filter(year < 2008),
    cluster = ~ id_estab,
    fixef.rm = "none",
    lean = TRUE
  )
  
  pre_terms <- grep("^year::", names(coef(pre_model)), value = TRUE)
  pre_margin <- if (length(pre_terms) > 0) {
    max(abs(unname(coef(pre_model)[pre_terms])), na.rm = TRUE)
  } else {
    0
  }
  
  data.frame(
    Outcome = unname(outcome_labels[[outcome]]),
    TWFE_Estimate = did_term$estimate[1],
    TWFE_SE = did_term$std.error[1],
    Pre_Trend_Margin = pre_margin,
    Lower_Bound = did_term$estimate[1] - 1.96 * did_term$std.error[1] - pre_margin,
    Upper_Bound = did_term$estimate[1] + 1.96 * did_term$std.error[1] + pre_margin,
    stringsAsFactors = FALSE
  )
}

honest_did_bounds <- bind_rows(lapply(outcomes, function(x) estimate_bounds(data, x)))

plot_data <- honest_did_bounds %>%
  mutate(Outcome = factor(Outcome, levels = unname(outcome_labels))) %>%
  pivot_longer(
    cols = c("Lower_Bound", "TWFE_Estimate", "Upper_Bound"),
    names_to = "Bound_Type",
    values_to = "Estimate"
  ) %>%
  mutate(
    Bound_Type = factor(
      Bound_Type,
      levels = c("Lower_Bound", "TWFE_Estimate", "Upper_Bound")
    )
  )

plot <- ggplot(
  plot_data,
  aes(x = Outcome, y = Estimate, color = Bound_Type, shape = Bound_Type)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(aes(group = Bound_Type), alpha = 0.5) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c(
      "Lower_Bound" = "#9ECAE1",
      "TWFE_Estimate" = "#08519C",
      "Upper_Bound" = "#3182BD"
    )
  ) +
  labs(
    title = "Sensitivity Analysis of the HonestDiD Approach",
    x = "Outcome",
    y = "Estimate",
    color = "",
    shape = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = "./results/analysis/Fig_G1_Honest_DID_Results.png",
  plot = plot,
  dpi = 300,
  width = 10,
  height = 6,
  units = "in"
)

write.csv(
  honest_did_bounds,
  "./results/analysis/Fig_G1_Honest_DID_Bounds.csv",
  row.names = FALSE
)

cat("Figure G.1 saved: ./results/analysis/Fig_G1_Honest_DID_Results.png\n")
