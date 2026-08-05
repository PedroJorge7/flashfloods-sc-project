# ============================================================================
# Figure 1 — Geographical Distribution of the Affected Area
#
# Highlights the Santa Catarina municipalities that declared a state of public
# calamity after the 2008 flash floods and adds a South America locator inset.
# Output name follows the LaTeX source: Figures2/map_new.png.
# ============================================================================

log_msg("=== 01_figure1_geographical_distribution_affected_area.R: start ===")

required_packages <- c("cowplot", "dplyr", "geobr", "ggplot2", "ggspatial", "sf")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Install the packages required by Figure 1: ",
       paste(missing_packages, collapse = ", "))
}

# Municipality codes used in the original public-calamity map. Keeping the
# list here makes the content of the figure explicit and reproducible.
ecp_municipalities_2008 <- c(
  420220, 420240, 420290, 420320, 420590, 420710, 420820,
  420845, 421000, 421150, 421320, 421470, 421510, 421820
)

sc_municipalities <- geobr::read_municipality(
  code_muni = 42,
  year = 2020,
  showProgress = FALSE
) |>
  sf::st_transform(4326) |>
  dplyr::mutate(
    map_group = dplyr::if_else(
      code_muni %in% ecp_municipalities_2008,
      "Public calamity",
      "Other municipalities"
    )
  )

if (!all(ecp_municipalities_2008 %in% sc_municipalities$code_muni)) {
  missing_codes <- setdiff(ecp_municipalities_2008, sc_municipalities$code_muni)
  stop("Figure 1 municipality codes not found in the geobr boundaries: ",
       paste(missing_codes, collapse = ", "))
}

state_map <- ggplot2::ggplot(sc_municipalities) +
  ggplot2::geom_sf(
    ggplot2::aes(fill = map_group),
    color = "grey35",
    linewidth = 0.12
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Public calamity" = "grey62",
      "Other municipalities" = "white"
    ),
    name = NULL
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = ggspatial::north_arrow_nautical,
    height = grid::unit(1.2, "cm"),
    width = grid::unit(1.2, "cm")
  ) +
  ggspatial::annotation_scale(
    location = "br",
    width_hint = 0.16,
    text_cex = 0.55
  ) +
  ggplot2::coord_sf(datum = NA, expand = FALSE) +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "bottom",
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    panel.background = ggplot2::element_rect(fill = "white", color = NA)
  )

brazil <- geobr::read_country(
  code_country = "BR",
  year = 2020,
  showProgress = FALSE
) |>
  sf::st_transform(4326)

sc_outline <- sc_municipalities |>
  dplyr::summarise() |>
  sf::st_as_sf()

locator_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(
    data = brazil,
    fill = "grey88",
    color = "black",
    linewidth = 0.18
  ) +
  ggplot2::geom_sf(
    data = sc_outline,
    inherit.aes = FALSE,
    fill = "red",
    color = "red",
    linewidth = 0.2
  ) +
  ggplot2::coord_sf(
    expand = FALSE,
    datum = NA
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(
    panel.border = ggplot2::element_rect(fill = NA, color = "grey25"),
    plot.background = ggplot2::element_rect(fill = "white", color = "grey25")
  )

figure_1 <- cowplot::ggdraw(state_map) +
  cowplot::draw_plot(locator_map, x = 0.045, y = 0.055, width = 0.27, height = 0.31)

out_path <- file.path(figures_dir, "map_new.png")
ggplot2::ggsave(
  filename = out_path,
  plot = figure_1,
  dpi = 300,
  width = 12,
  height = 8,
  units = "in",
  bg = "white"
)

log_msg("Saved figure: %s", out_path)
log_msg("=== 01_figure1_geographical_distribution_affected_area.R: done ===")
