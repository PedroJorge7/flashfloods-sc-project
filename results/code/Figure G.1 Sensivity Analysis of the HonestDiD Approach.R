# ============================================================================
# Figure 10: Honest DID Results
# ============================================================================

library(dplyr)
library(tidyr)
library(haven)
library(fixest)
library(ggplot2)
library(cowplot)
library(broom)
library(did)

rm(list = ls())

# ============================================================================
# Load data
# ============================================================================

data <- haven::read_dta("Natural Disastrer Santa Catarina - Dataset.dta")

# ============================================================================
# Data preparation
# ============================================================================

# Keep only years 2003-2012 for DID analysis
data <- data %>%
  filter(year >= 2003 & year <= 2012) %>%
  arrange(id_estab, year)

# Create treatment variable
data <- data %>%
  mutate(
    treat_B = case_when(
      dist_flood <= 12.5 ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE ~ NA_real_
    )
  )

# Forward fill treatment status
data <- data %>%
  group_by(id_estab) %>%
  fill(treat_B, .direction = \"down\") %>%
  ungroup()

# Create binary treatment indicator (treated = 1 if treat_B == 1)
data <- data %>%
  mutate(
    treated = ifelse(treat_B == 1, 1, 0),
    post = ifelse(year >= 2008, 1, 0)
  )

# ============================================================================
# Run standard DID regression
# ============================================================================

outcomes <- c(\"morte\", \"new_firm\", \"mover_ano_mun\")

# Store DID results
did_results <- data.frame()

for (outcome in outcomes) {
  
  # Standard DID with FE
  model <- feols(
    as.formula(paste(outcome, \"~ treated * post\")),
    data = data,
    fixef = c(\"id_estab\", \"year\"),
    cluster = \"id_estab\",
    lean = TRUE
  )
  
  # Extract DID coefficient (interaction term)
  coef_summary <- broom::tidy(model, conf.int = TRUE)
  
  did_coef <- coef_summary %>%
    filter(grepl(\"treated.*post\", term)) %>%
    mutate(outcome = outcome)
  
  did_results <- bind_rows(did_results, did_coef)
}

# ============================================================================
# Compute Honest DID bounds
# ============================================================================

# For each outcome, compute bounds under different assumptions
honest_did_bounds <- data.frame()

for (outcome in outcomes) {
  
  # Prepare data for did package
  outcome_data <- data %>%
    select(id_estab, year, treated, all_of(outcome)) %>%
    rename(outcome = all_of(outcome)) %>%
    filter(!is.na(outcome))
  
  # Compute ATT using did package with different estimators
  # Using simple TWFE as baseline
  att_twfe <- feols(
    outcome ~ treated * post,
    data = outcome_data,
    fixef = c(\"id_estab\", \"year\"),
    cluster = \"id_estab\"
  )
  
  coef_twfe <- coef(att_twfe)[\"treated:post\"]
  se_twfe <- sqrt(diag(vcov(att_twfe)))[\"treated:post\"]
  
  # Compute bounds under different assumptions
  # Lower bound: assume treatment effect is at least as large as pre-treatment trend
  # Upper bound: assume no negative effect
  
  pre_data <- outcome_data %>% filter(year < 2008)
  post_data <- outcome_data %>% filter(year >= 2008)
  
  # Pre-treatment trend
  pre_trend <- feols(
    outcome ~ treated,
    data = pre_data,
    fixef = \"id_estab\",
    cluster = \"id_estab\"
  )
  
  pre_trend_coef <- coef(pre_trend)[\"treated\"]
  
  honest_did_bounds <- rbind(honest_did_bounds, data.frame(
    Outcome = outcome,
    TWFE_Estimate = round(coef_twfe, 5),
    TWFE_SE = round(se_twfe, 5),
    Pre_Trend = round(pre_trend_coef, 5),
    Lower_Bound = round(coef_twfe - 1.96 * se_twfe, 5),
    Upper_Bound = round(coef_twfe + 1.96 * se_twfe, 5)
  ))
}

# ============================================================================
# Create visualization
# ============================================================================

# Prepare data for plotting
plot_data <- honest_did_bounds %>%
  mutate(
    Outcome = factor(Outcome, levels = outcomes)
  ) %>%
  pivot_longer(
    cols = c(\"Lower_Bound\", \"TWFE_Estimate\", \"Upper_Bound\"),
    names_to = \"Bound_Type\",
    values_to = \"Estimate\"
  ) %>%
  mutate(
    Bound_Type = factor(Bound_Type, levels = c(\"Lower_Bound\", \"TWFE_Estimate\", \"Upper_Bound\"))
  )

# Create plot
plot <- ggplot(plot_data, aes(x = Outcome, y = Estimate, color = Bound_Type, shape = Bound_Type)) +
  geom_point(size = 4, position = position_dodge(width = 0.3)) +
  geom_hline(yintercept = 0, linetype = \"dashed\", color = \"black\", alpha = 0.5) +
  theme_bw() +
  theme(\n    legend.position = \"bottom\",\n    plot.title = element_text(size = 12, face = \"bold\")\n  ) +\n  scale_color_manual(\n    values = c(\"Lower_Bound\" = \"lightblue\", \"TWFE_Estimate\" = \"darkblue\", \"Upper_Bound\" = \"lightblue\")\n  ) +\n  labs(\n    title = \"Honest DID Results: Treatment Effects on Establishment Outcomes\",\n    x = \"Outcome\",\n    y = \"Coefficient\",\n    color = \"Bound Type\",\n    shape = \"Bound Type\"\n  )\n\n# ============================================================================\n# Save results\n# ============================================================================\n\nggsave(\n  \"establishments/analysis/Fig_10_Honest_DID_Results.png\",\n  plot,\n  dpi = 300,\n  width = 10,\n  height = 6,\n  units = \"in\"\n)\n\n# Save bounds as CSV\nwrite.csv(honest_did_bounds, \"establishments/analysis/Fig_10_Honest_DID_Bounds.csv\", row.names = FALSE)\n\ncat(\"Figure 10 saved: establishments/analysis/Fig_10_Honest_DID_Results.png\\n\")\n"
