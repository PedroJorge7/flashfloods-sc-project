# ============================================================================
# Figure 5b — Event Study: Effect of Disaster-Induced Closures on Dismissed
# Workers, by Skill Level
# Treatment: 0-5 km vs. 50-80 km control. Clustering: census tract.
# Employment outcome only. Depends on `output_high_trend` / `output_low_trend`
# (high-skill / low-skill workers, with trend, separately matched by skill
# group) from 08_table6_dismissed_workers.R.
# ============================================================================

log_msg("=== 10_figure5b_event_study_workers_by_skill.R: start ===")

es_high <- output_high_trend %>% filter(!grepl("migration", Regression, ignore.case = TRUE))
es_low  <- output_low_trend  %>% filter(!grepl("migration", Regression, ignore.case = TRUE))

fig_high <- event_study_plot("A - Employment (1/0)", es_high) + labs(title = "High Skill")
fig_low  <- event_study_plot("A - Employment (1/0)", es_low)  + labs(title = "Low Skill")

out_path_high <- file.path(figures_dir, "Fig_05b_EventStudy_Workers_HighSkill_5km_tract.png")
ggsave(filename = out_path_high, plot = fig_high, dpi = 300, width = 18, height = 12, units = "cm")
log_msg("Saved figure: %s", out_path_high)

out_path_low <- file.path(figures_dir, "Fig_05b_EventStudy_Workers_LowSkill_5km_tract.png")
ggsave(filename = out_path_low, plot = fig_low, dpi = 300, width = 18, height = 12, units = "cm")
log_msg("Saved figure: %s", out_path_low)

fig_combined <- ggpubr::ggarrange(fig_high, fig_low, nrow = 1, ncol = 2)
out_path_combined <- file.path(figures_dir, "Fig_05b_EventStudy_Workers_BySkill_5km_tract.png")
ggsave(filename = out_path_combined, plot = fig_combined, dpi = 300, width = 30, height = 12, units = "cm")
log_msg("Saved figure: %s", out_path_combined)

log_msg("=== 10_figure5b_event_study_workers_by_skill.R: done ===")
