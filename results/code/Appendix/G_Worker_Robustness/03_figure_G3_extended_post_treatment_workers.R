# ============================================================================
# Appendix G, Figure G.3 — Worker Level Estimates Increasing Post-Treatment
# Period
# Treatment: 0-5 km vs. 50-80 km control. Clustering: census tract.
# Employment outcome only, shown as a coefficient-by-period figure (Post +
# year-specific effects, 2008-2016). Uses the full `dados` (2002-2016, not
# truncated to 2012 like `dados_main`) from 00b_load_worker_data.R.
# ============================================================================

log_msg("=== 03_figure_G3_extended_post_treatment_workers.R: start ===")

output_trend <- output_empregados(
  dados, WORKER_MIN_TREAT, WORKER_MAX_TREAT, WORKER_MIN_CTRL, WORKER_MAX_CTRL,
  trend = TRUE, se_type = "cluster", cluster_formula = WORKER_CLUSTER_FORMULA
)

all_results <- output_trend %>%
  filter(type == "type_treatment", Regression == "A - Employment (1/0)", grepl("^Flash Flood", parmseq)) %>%
  mutate(
    Period = factor(gsub("Flash Flood ", "", parmseq), levels = c("Post", as.character(2008:2016))),
    Coefficient = as.numeric(estimate), SE = as.numeric(se),
    CI_Low = Coefficient - 1.96 * SE, CI_High = Coefficient + 1.96 * SE
  )

fig <- ggplot(all_results, aes(x = Period, y = Coefficient)) +
  geom_point(size = 2, color = "firebrick") +
  geom_errorbar(aes(ymin = CI_Low, ymax = CI_High), width = 0.2, color = "firebrick") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Year", y = "Coefficient") +
  theme_bw()

out_path <- file.path(figures_dir, "longpanel_empregados.png")
ggsave(filename = out_path, plot = fig, dpi = 300, width = 8, height = 6, units = "in")
log_msg("Saved figure: %s", out_path)

log_msg("=== 03_figure_G3_extended_post_treatment_workers.R: done ===")
