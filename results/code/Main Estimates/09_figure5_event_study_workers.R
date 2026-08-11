# ============================================================================
# Figure 5 — Event Study: Effect of Disaster-Induced Closures on Dismissed
# Workers
# Treatment: 0-5 km vs. 50-80 km control. Clustering: census tract.
# Employment outcome only. Depends on `output_trend` (all workers, with
# trend) from 08_table6_dismissed_workers.R.
# ============================================================================

log_msg("=== 09_figure5_event_study_workers.R: start ===")

es_data <- output_trend %>% filter(!grepl("migration", Regression, ignore.case = TRUE))

fig <- event_study_plot("A - Employment (1/0)", es_data) + labs(title = NULL)

out_path <- file.path(figures_dir, "Fig_05_EventStudy_Workers_5km_tract.png")
ggsave(filename = out_path, plot = fig, dpi = 300, width = 18, height = 12, units = "cm")
log_msg("Saved figure: %s", out_path)

log_msg("=== 09_figure5_event_study_workers.R: done ===")
