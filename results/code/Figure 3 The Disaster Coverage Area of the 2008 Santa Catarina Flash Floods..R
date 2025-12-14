# ============================================================================
# Figure 2: The Disaster Coverage Area of the 2008 Santa Catarina Flash Floods
# ============================================================================

library(dplyr)
library(sf)
library(ggplot2)
library(mapview)

rm(list = ls())

# ============================================================================
# Load map data
# ============================================================================

# Load the map file (should be provided as shapefile or similar)
# This assumes you have the map data available

# For now, creating a placeholder that loads the map
# In practice, you would load your actual map data

# Example: Loading from a shapefile
# map_data <- sf::st_read("path/to/map.shp")

# ============================================================================
# Create map visualization
# ============================================================================

# This is a placeholder - actual implementation depends on your map data structure
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
  "Fig_02_Disaster_Coverage_Area.png",
  plot,
  dpi = 300,
  width = 12,
  height = 8,
  units = "in"
)

cat("Figure 2 saved: Fig_02_Disaster_Coverage_Area.png\n")
cat("Note: This requires actual map data. Please provide shapefile or map data.\n")
