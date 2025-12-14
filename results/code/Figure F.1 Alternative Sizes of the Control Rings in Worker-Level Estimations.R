# ============================================================================
# Figure 8: Alternative Sizes of Control Rings - Worker Level
# ============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(haven)

rm(list = ls())

# ============================================================================
# Load results for alternative control boundaries (worker level)
# ============================================================================

control_rings <- c("150_170", "170_190", "190_210", "210_230")

# Load all results
all_results <- data.frame()

for (ring in control_rings) {
  filename <- paste0("workers_agg_false_radius_", ring, ".dta")
  
  tryCatch({
    result <- haven::read_dta(filename) %>%
      mutate(
        outcome = "Dismissed Workers",
        control_ring = ring
      )
    all_results <- bind_rows(all_results, result)
  }, error = function(e) {
    cat("File not found:", filename, "\n")
  })
}

# ============================================================================
# Create visualization
# ============================================================================

plot <- ggplot(all_results, aes(x = control_ring, y = estimate)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(
    aes(ymin = min95, ymax = max95),
    width = 0.2,
    color = "darkblue"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Alternative Sizes of the Control Rings in Worker-Level Estimations",
    x = "Control Ring Size (km)",
    y = "Coefficient (95% CI)"
  )

# ============================================================================
# Save plot
# ============================================================================

ggsave(
  "Fig_08_Alternative_Control_Rings_Workers.png",
  plot,
  dpi = 300,
  width = 10,
  height = 6,
  units = "in"
)

cat("Figure 8 saved: Fig_08_Alternative_Control_Rings_Workers.png\n")
