# ============================================================================
# Table A.11: The Effect of the Flash Floods on Exposed Workers
# ============================================================================

library(dplyr)
library(tidyr)
library(haven)
library(modelsummary)

rm(list = ls())

# ============================================================================
# Load regression results from Stata
# ============================================================================

# Load the tab_workers_results.xls file
results <- readxl::read_excel("tab_workers_results.xls")

# ============================================================================
# Format results
# ============================================================================

results_formatted <- results %>%
  arrange(Outcome)

# ============================================================================
# Generate LaTeX table
# ============================================================================

tex_output <- knitr::kable(
  results_formatted,
  format = "latex",
  booktabs = TRUE,
  caption = "The Effect of the Flash Floods on Exposed Workers"
) %>%
  kableExtra::kable_styling(
    latex_options = c("striped", "hold_position", "scale_down")
  ) %>%
  kableExtra::column_spec(1, width = "3cm")

# ============================================================================
# Save LaTeX table
# ============================================================================

writeLines(
  tex_output,
  "Tab_A11_Effect_Exposed_Workers.tex"
)

cat("Table A.11 saved: Tab_A11_Effect_Exposed_Workers.tex\n")
