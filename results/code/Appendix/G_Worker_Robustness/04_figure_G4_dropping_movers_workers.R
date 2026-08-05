# ============================================================================
# Appendix G, Figure G.4 — Worker Level Estimates Dropping Units that Moved
# Across Areas
# Treatment: 0-5 km vs. 50-80 km control. Clustering: census tract.
# Employment outcome only, shown as a coefficient-by-period figure (Post +
# year-specific effects). `remove_treat_control_mob = TRUE` drops workers
# whose treated/control ring status switches across the panel. Uses
# `dados_main` (2002-2012).
# ============================================================================

log_msg("=== 04_figure_G4_dropping_movers_workers.R: start ===")

output_trend <- output_empregados(
  dados_main, WORKER_MIN_TREAT, WORKER_MAX_TREAT, WORKER_MIN_CTRL, WORKER_MAX_CTRL,
  remove_treat_control_mob = TRUE, trend = TRUE,
  se_type = "cluster", cluster_formula = WORKER_CLUSTER_FORMULA
)

all_results <- output_trend %>%
  filter(type == "type_treatment", Regression == "A - Employment (1/0)", grepl("^Flash Flood", parmseq)) %>%
  mutate(
    Period = factor(gsub("Flash Flood ", "", parmseq), levels = c("Post", as.character(2008:2012))),
    Coefficient = as.numeric(estimate), SE = as.numeric(se),
    CI_Low = Coefficient - 1.96 * SE, CI_High = Coefficient + 1.96 * SE
  )

fig <- ggplot(all_results, aes(x = Period, y = Coefficient)) +
  geom_point(size = 2, color = "firebrick") +
  geom_errorbar(aes(ymin = CI_Low, ymax = CI_High), width = 0.2, color = "firebrick") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Year", y = "Coefficient") +
  theme_bw()

out_path <- file.path(figures_dir, "drop_units_empregados.png")
ggsave(filename = out_path, plot = fig, dpi = 300, width = 8, height = 6, units = "in")
log_msg("Saved figure: %s", out_path)

log_msg("=== 04_figure_G4_dropping_movers_workers.R: done ===")
