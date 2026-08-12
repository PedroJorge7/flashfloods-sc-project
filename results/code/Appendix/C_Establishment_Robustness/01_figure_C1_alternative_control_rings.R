# ============================================================================
# Appendix C, Figure C.1 — Alternative Sizes of the
# Control Rings in Establishment-Level Estimates
# Treatment fixed at 0-5 km; control ring varies (40-70, 30-80, 50-100 km,
# plus 50-80 km as reference). Clustering: census tract. Uses `raw_data`
# (each ring needs its own panel build via
# build_establishment_panel() -- fixed baseline treatment, no mover-year
# outcome nulling).
# ============================================================================

log_msg("=== 01_figure_C1_alternative_control_rings.R: start ===")

treated_rule_5km <- function(d) d <= 5
control_specs <- list(
  "40-70 km"          = function(d) dplyr::between(d, 40, 70),
  "30-80 km"          = function(d) dplyr::between(d, 30, 80),
  "50-100 km"         = function(d) dplyr::between(d, 50, 100),
  "50-80 km (current)" = function(d) dplyr::between(d, 50, 80)
)

outcomes <- c(morte = "Closure", reloc_tract_tminus1 = "Relocation")
all_results <- data.frame()

for (ring_label in names(control_specs)) {
  ring_tag <- gsub("[^A-Za-z0-9]+", "_", ring_label)
  panel <- build_or_load_panel(
    file.path(cache_dir, sprintf("C1_panel_%s.rds", ring_tag)),
    function() build_establishment_panel(raw_data, treated_rule_5km, sprintf("C.1 ring=%s", ring_label),
                                          control_rule = control_specs[[ring_label]])
  )
  for (outcome in names(outcomes)) {
    res <- fit_establishment_models(panel, outcome, context = sprintf("C.1 / %s / %s", ring_label, outcomes[[outcome]]))
    if (!is.null(res$agg)) {
      agg_row <- broom::tidy(res$agg) %>% filter(term == "treat_B_agg") %>%
        transmute(Control_Ring = ring_label, Outcome = outcomes[[outcome]], Period = "Post",
                  Coefficient = estimate, SE = std.error, CI_Low = estimate - 1.96*std.error, CI_High = estimate + 1.96*std.error)
      all_results <- bind_rows(all_results, agg_row)
    }
    if (!is.null(res$timevar)) {
      evt_rows <- broom::tidy(res$timevar) %>% filter(grepl("^treat_B_", term)) %>%
        mutate(Period = gsub("treat_B_", "", term)) %>%
        transmute(Control_Ring = ring_label, Outcome = outcomes[[outcome]], Period = Period,
                  Coefficient = estimate, SE = std.error, CI_Low = estimate - 1.96*std.error, CI_High = estimate + 1.96*std.error)
      all_results <- bind_rows(all_results, evt_rows)
    }
    rm(res)
  }
  rm(panel)
  gc(full = TRUE)
}

all_results <- all_results %>%
  mutate(
    Period = factor(Period, levels = c("Post", "2008", "2009", "2010", "2011", "2012")),
    Control_Ring = factor(Control_Ring, levels = names(control_specs))
  )

ring_colors <- c("40-70 km" = "#FEE0D2", "30-80 km" = "#FB6A4A", "50-100 km" = "#CB181D", "50-80 km (current)" = "#6BAED6")

make_panel <- function(df, title_label) {
  ggplot(df, aes(x = Period, y = Coefficient, color = Control_Ring)) +
    geom_point(position = position_dodge(width = 0.4), size = 2) +
    geom_errorbar(aes(ymin = CI_Low, ymax = CI_High), position = position_dodge(width = 0.4), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_discrete(drop = FALSE) +
    scale_color_manual(values = ring_colors, name = "Control Ring") +
    labs(x = "Year", y = "Coefficient", title = title_label) + theme_bw() + theme(legend.position = "bottom")
}

pA <- make_panel(filter(all_results, Outcome == "Closure"), "A - Establishment Closure (0/1)")
pB <- make_panel(filter(all_results, Outcome == "Relocation"), "B - Establishment Relocation (0/1)")
fig <- ggpubr::ggarrange(pA, pB, ncol = 2, common.legend = TRUE, legend = "bottom")

out_path <- file.path(figures_dir, "Fig_C01_Alternative_Control_Rings_5km_tract.png")
ggsave(filename = out_path, plot = fig, dpi = 300, width = 12, height = 6, units = "in")
log_msg("Saved figure: %s", out_path)

log_msg("=== 01_figure_C1_alternative_control_rings.R: done ===")
