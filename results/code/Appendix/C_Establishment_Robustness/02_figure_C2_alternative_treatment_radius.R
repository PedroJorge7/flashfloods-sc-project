# ============================================================================
# Appendix C, Figure C.2 — Alternative Sizes of the
# Treatment Radius in Establishment-Level Estimates
# Radius ladder: 2.5, 5 (reference), 7.5, 10, 12.5, 15 km; control fixed at
# 50-80 km. Clustering: census tract. Uses `raw_data` (each radius needs its
# own panel build via build_establishment_panel()).
# ============================================================================

log_msg("=== 02_figure_C2_alternative_treatment_radius.R: start ===")

radius_specs <- list(
  "0-2.5 km"          = 2.5,
  "0-5 km (reference)" = 5,
  "0-7.5 km"          = 7.5,
  "0-10 km"           = 10,
  "0-12.5 km"         = 12.5,
  "0-15 km"           = 15
)

outcomes <- c(morte = "Closure", reloc_tract_tminus1 = "Relocation")
all_results <- data.frame()

for (rad_label in names(radius_specs)) {
  r <- radius_specs[[rad_label]]
  treated_rule <- local({ rr <- r; function(d) d <= rr })
  rad_tag <- gsub("[^A-Za-z0-9]+", "_", rad_label)
  panel <- build_or_load_panel(
    file.path(cache_dir, sprintf("C2_panel_%s.rds", rad_tag)),
    function() build_establishment_panel(raw_data, treated_rule, sprintf("C.2 radius=%s", rad_label))
  )
  for (outcome in names(outcomes)) {
    res <- fit_establishment_models(panel, outcome, context = sprintf("C.2 / %s / %s", rad_label, outcomes[[outcome]]))
    if (!is.null(res$agg)) {
      agg_row <- broom::tidy(res$agg) %>% filter(term == "treat_B_agg") %>%
        transmute(Treatment_Radius = rad_label, Outcome = outcomes[[outcome]], Period = "Post",
                  Coefficient = estimate, SE = std.error, CI_Low = estimate - 1.96*std.error, CI_High = estimate + 1.96*std.error)
      all_results <- bind_rows(all_results, agg_row)
    }
    if (!is.null(res$timevar)) {
      evt_rows <- broom::tidy(res$timevar) %>% filter(grepl("^treat_B_", term)) %>%
        mutate(Period = gsub("treat_B_", "", term)) %>%
        transmute(Treatment_Radius = rad_label, Outcome = outcomes[[outcome]], Period = Period,
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
    Treatment_Radius = factor(Treatment_Radius, levels = names(radius_specs))
  )

rad_colors <- c("0-2.5 km" = "#FEE0D2", "0-5 km (reference)" = "#6BAED6", "0-7.5 km" = "#FCBBA1",
                "0-10 km" = "#FC9272", "0-12.5 km" = "#FB6A4A", "0-15 km" = "#CB181D")

make_panel <- function(df, title_label) {
  ggplot(df, aes(x = Period, y = Coefficient, color = Treatment_Radius)) +
    geom_point(position = position_dodge(width = 0.4), size = 2) +
    geom_errorbar(aes(ymin = CI_Low, ymax = CI_High), position = position_dodge(width = 0.4), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_discrete(drop = FALSE) +
    scale_color_manual(values = rad_colors, name = "Treatment Radius") +
    labs(x = "Year", y = "Coefficient", title = title_label) + theme_bw() + theme(legend.position = "bottom")
}

pA <- make_panel(filter(all_results, Outcome == "Closure"), "A - Establishment Closure (0/1)")
pB <- make_panel(filter(all_results, Outcome == "Relocation"), "B - Establishment Relocation (0/1)")
fig <- ggpubr::ggarrange(pA, pB, ncol = 2, common.legend = TRUE, legend = "bottom")

out_path <- file.path(figures_dir, "Fig_C02_Alternative_Treatment_Radius_5km_tract.png")
ggsave(filename = out_path, plot = fig, dpi = 300, width = 12, height = 6, units = "in")
log_msg("Saved figure: %s", out_path)

log_msg("=== 02_figure_C2_alternative_treatment_radius.R: done ===")
