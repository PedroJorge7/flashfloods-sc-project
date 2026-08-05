# ============================================================================
# Appendix C, Table C.4 — Establishment-Level Estimates Increasing the
# Post-Treatment Period
# Treatment: 0-5 km vs. 50-80 km control. Clustering: census tract. Sample
# extended to 2003-2016 (vs. 2003-2012 in the main table). Depends on
# `extended_panel`.
# ============================================================================

log_msg("=== 04_table_C4_extended_post_treatment.R: start ===")

res_closure <- fit_establishment_models(extended_panel, "morte", tv_years = 2008:2016,
                                         event_lag_years = 2008:2016, context = "Table C.4 / Closure")
res_reloc   <- fit_establishment_models(extended_panel, "reloc_tract_tminus1", tv_years = 2008:2016,
                                         event_lag_years = 2008:2016, context = "Table C.4 / Relocation")

emit_closure_relocation_table(
  res_closure, res_reloc,
  out_path = file.path(tables_dir, "Tab_C04_Extended_Post_Treatment_5km_tract.tex"),
  caption  = "Establishment-Level Estimates Increasing the Post-Treatment Period",
  label    = "rob5:est",
  notes_full = "Establishment fixed effects, year fixed effects, and a census tract trend are included in all estimations. The treatment radius ranges from 0--5 km from flood spots; the control ring is between 50--80 km. Standard errors clustered at the census-tract level are in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.1."
)

log_msg("=== 04_table_C4_extended_post_treatment.R: done ===")
