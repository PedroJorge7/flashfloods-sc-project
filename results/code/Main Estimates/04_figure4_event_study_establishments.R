# ============================================================================
# Figure 4 — Event Study: Effect of the Flash Floods on Establishments'
# Adjustment
# Treatment: 0-5 km vs. 50-80 km control. Clustering: census tract.
# Depends on `main_panel` and `res_closure`/`res_reloc` from
# 03_table2_establishment_adjustment.R (same models feed both exhibits).
# ============================================================================

log_msg("=== 04_figure4_event_study_establishments.R: start ===")

emit_closure_relocation_event_study(
  res_closure, res_reloc,
  out_path = file.path(figures_dir, "Fig_04_EventStudy_Establishments_5km_tract.png")
)

log_msg("=== 04_figure4_event_study_establishments.R: done ===")
