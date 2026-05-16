# ============================================================================
# Figure 3: The Disaster Coverage Area of the 2008 Santa Catarina Flash Floods
# Output: ./results/analysis/Fig_03_Disaster_Coverage_Area.png
# ============================================================================

rm(list = ls())

source("./results/code/path_utils.R")

library(dplyr)
library(geobr)
library(ggplot2)
library(ggspatial)
library(haven)
library(sf)

dir.create(project_path("results", "analysis"), recursive = TRUE, showWarnings = FALSE)

figure_data_path <- function(...) {
  preferred <- data_path(..., must_work = FALSE)
  if (file.exists(preferred)) {
    return(preferred)
  }

  fallback <- project_path("data", ..., must_work = FALSE)
  normalizePath(fallback, winslash = "/", mustWork = TRUE)
}

# ----------------------------------------------------------------------------
# 1) Load spatial inputs and standardize projections
# ----------------------------------------------------------------------------

sc_municipalities <- geobr::read_municipality(
  code_muni = 42,
  year = 2020,
  showProgress = FALSE
) %>%
  st_transform(4326)

sc_outline <- sc_municipalities %>%
  summarise(geometry = st_union(geom)) %>%
  st_as_sf()

flood_projected <- st_read(
  figure_data_path("inundacao", "inundacao_2008.shp"),
  quiet = TRUE
) %>%
  st_make_valid()

flood_crs <- st_crs(flood_projected)

sc_outline_projected <- sc_outline %>%
  st_transform(flood_crs) %>%
  st_make_valid()

flood_union <- flood_projected %>%
  summarise(geometry = st_union(geometry)) %>%
  st_as_sf() %>%
  st_make_valid()

treated_buffer <- flood_union %>%
  st_buffer(dist = 12500) %>%
  st_intersection(sc_outline_projected) %>%
  st_difference(st_union(flood_union)) %>%
  mutate(zone = "Treated Area")

control_buffer <- flood_union %>%
  st_buffer(dist = 80000) %>%
  st_intersection(sc_outline_projected) %>%
  st_difference(st_union(flood_union %>% st_buffer(dist = 50000))) %>%
  mutate(zone = "Control Area")

flood_spots <- flood_union %>%
  st_intersection(sc_outline_projected) %>%
  mutate(zone = "Flood Spots")

coverage_zones <- bind_rows(
  treated_buffer %>% select(zone),
  control_buffer %>% select(zone)
) %>%
  st_transform(4326)

flood_spots <- flood_spots %>%
  st_transform(4326)

map_layers <- bind_rows(
  coverage_zones,
  flood_spots %>% select(zone)
)

map_layers$zone <- factor(
  map_layers$zone,
  levels = c("Flood Spots", "Control Area", "Treated Area")
)

map_extent <- st_bbox(st_union(st_geometry(map_layers)))

# ----------------------------------------------------------------------------
# 2) Load rivers and establishments shown in the close-up panel
# ----------------------------------------------------------------------------

rivers <- st_read(
  figure_data_path("novo_dado_rio", "geoft_bho_rio.shp"),
  quiet = TRUE
) %>%
  st_transform(4326) %>%
  st_intersection(sc_outline)

firm_points <- haven::read_dta(
  figure_data_path("firm_coordinates.dta"),
  col_select = c(id_estab, year, Lon, Lat, dist_flood)
) %>%
  filter(
    year == 2007,
    !is.na(Lon),
    !is.na(Lat),
    !is.na(dist_flood),
    dist_flood <= 12.5 | between(dist_flood, 50, 80)
  ) %>%
  distinct(id_estab, .keep_all = TRUE) %>%
  mutate(sample_band = if_else(dist_flood <= 12.5, "Treated firms", "Control firms")) %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326, remove = FALSE)

# ----------------------------------------------------------------------------
# 3) Build the close-up map and the Santa Catarina locator
# ----------------------------------------------------------------------------

map1 <- ggplot() +
  geom_sf(data = sc_municipalities, color = "grey34", fill = NA, show.legend = FALSE) +
  geom_sf(data = map_layers, aes(fill = zone), color = "grey34", linewidth = 0.18) +
  scale_fill_manual(
    values = c(
      "Flood Spots" = "red",
      "Control Area" = scales::alpha("grey92", 0.75),
      "Treated Area" = scales::alpha("grey54", 0.75)
    ),
    breaks = c("Flood Spots", "Control Area", "Treated Area"),
    name = ""
  ) +
  geom_sf(data = rivers, color = "deepskyblue4", linewidth = 0.8, show.legend = FALSE) +
  geom_sf(
    data = firm_points,
    aes(color = "Establishments dots"),
    alpha = 0.5,
    size = 0.5
  ) +
  scale_color_manual(
    values = c("Establishments dots" = "grey54"),
    name = "Establishments"
  ) +
  coord_sf(
    xlim = c(map_extent[["xmin"]] * 1.002, map_extent[["xmax"]] * 0.998),
    ylim = c(map_extent[["ymin"]] * 1.002, map_extent[["ymax"]] * 0.998),
    expand = FALSE,
    datum = NA
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(fill = NA),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.key = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    plot.background = element_rect(fill = "white", color = NA)
  )

map2 <- ggplot() +
  geom_sf(data = sc_municipalities, color = "grey34", fill = NA, show.legend = FALSE) +
  geom_sf(data = map_layers, aes(fill = zone), color = "black", linewidth = 0.12, show.legend = FALSE) +
  scale_fill_manual(
    values = c(
      "Flood Spots" = "red",
      "Control Area" = scales::alpha("grey92", 0.75),
      "Treated Area" = scales::alpha("grey54", 0.75)
    )
  ) +
  geom_rect(
    xmin = map_extent[["xmin"]],
    xmax = map_extent[["xmax"]],
    ymin = map_extent[["ymin"]],
    ymax = map_extent[["ymax"]],
    fill = NA,
    colour = "black",
    linewidth = 1.5
  ) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.5) +
  ggspatial::annotation_north_arrow(
    location = "tr",
    style = ggspatial::north_arrow_nautical,
    which_north = "true",
    height = grid::unit(1, "cm"),
    width = grid::unit(1, "cm"),
    pad_x = grid::unit(0, "in"),
    pad_y = grid::unit(0, "in")
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA)
  )

map <- ggplot() +
  coord_equal(xlim = c(0, 30), ylim = c(0, 20), expand = FALSE) +
  annotation_custom(ggplotGrob(map1), xmin = 0, xmax = 20, ymin = 0, ymax = 20) +
  annotation_custom(ggplotGrob(map2), xmin = 20, xmax = 30, ymin = 8, ymax = 17.5) +
  geom_curve(
    aes(x = 14, y = 11, xend = 26.5, yend = 14.5),
    arrow = arrow(type = "closed"),
    curvature = -0.2
  ) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# ----------------------------------------------------------------------------
# 4) Save figure
# ----------------------------------------------------------------------------

output_file <- project_path("results", "analysis", "Fig_03_Disaster_Coverage_Area.png")

ggsave(
  filename = output_file,
  plot = map,
  dpi = 300,
  height = 15,
  width = 25,
  units = "cm",
  bg = "white"
)

cat("Saved figure:", output_file, "\n")
