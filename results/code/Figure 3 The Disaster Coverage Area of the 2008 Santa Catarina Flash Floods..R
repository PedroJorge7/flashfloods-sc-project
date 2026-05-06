# ============================================================================
# Figure 3: The Disaster Coverage Area of the 2008 Santa Catarina Flash Floods
# ============================================================================

library(dplyr)
library(sf)
library(ggplot2)
library(mapview)

rm(list = ls())

dir.create('./results/analysis', recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# Load map data
# ============================================================================

# Load the spatial data source for the figure, such as a shapefile
# Update the file path below to match the local data source

# Example: load a shapefile
# map_data <- sf::st_read("path/to/map.shp")

# ============================================================================
# Create map visualization
# ============================================================================

# The map should show:
# - The affected municipalities in gray
# - The location of Santa Catarina in Latin America
# - The flood spots/areas

# Create a basic map plot
plot <- ggplot() +
  theme_minimal() +
  labs(
    title = "The Disaster Coverage Area of the 2008 Santa Catarina Flash Floods",
    subtitle = "Municipalities declaring public calamity status"
  )

# ============================================================================
# Save plot
# ============================================================================

ggsave(
  "./results/analysis/Fig_03_Disaster_Coverage_Area.png",
  plot,
  dpi = 300,
  width = 12,
  height = 8,
  units = "in"
)

cat("Saved figure: ./results/analysis/Fig_03_Disaster_Coverage_Area.png\n")
cat("Provide the spatial input files before running the final map version.\n")




