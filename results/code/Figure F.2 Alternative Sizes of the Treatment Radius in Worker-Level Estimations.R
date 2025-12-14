# ============================================================================
# Figure 9: Alternative Sizes of Treatment Radius - Worker Level
# ============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(haven)

rm(list = ls())

# ============================================================================
# Load results for alternative treatment radius (worker level)
# ============================================================================

treatment_radii <- c("2.5", "7.5", "17.5", "22.5", "30")

# Load all results
all_results <- data.frame()

for (radius in treatment_radii) {
  filename <- paste0("workers_agg_radius_", radius, ".dta")
  
  tryCatch({
    result <- haven::read_dta(filename) %>%
      mutate(
        outcome = "Dismissed Workers",
        treatment_radius = as.numeric(radius)
      )
    all_results <- bind_rows(all_results, result)
  }, error = function(e) {
    cat("File not found:", filename, "\n")
  })
}

# ============================================================================
# Create visualization
# ============================================================================

plot <- ggplot(all_results, aes(x = treatment_radius, y = estimate)) +
  geom_point(size = 3, color = "darkblue") +
  geom_line(linetype = "solid", color = "darkblue") +
  geom_ribbon(
    aes(ymin = min95, ymax = max95),
    alpha = 0.2,
    fill = "lightblue"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Alternative Sizes of the Treatment Radius in Worker-Level Estimations",
    x = "Treatment Radius (km)",
    y = "Coefficient (95% CI)"
  )

# ============================================================================
# Save plot
# ============================================================================

ggsave(
  "Fig_09_Alternative_Treatment_Radius_Workers.png",
  plot,
  dpi = 300,
  width = 10,
  height = 6,
  units = "in"
)

cat("Figure 9 saved: Fig_09_Alternative_Treatment_Radius_Workers.png\n")
