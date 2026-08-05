# ============================================================================
# Appendix C, Table C.6 — Establishment-Level Estimates Dropping Units that
# Moved Across Areas
# Treatment: 0-5 km vs. 50-80 km control. Clustering: census tract. Sample
# restricted to establishments whose treated/control status never switches
# across the panel. Depends on `main_panel`.
# ============================================================================

log_msg("=== 06_table_C6_business_sorting.R: start ===")

stable_ids <- main_panel %>%
  filter(year >= 2003, year <= 2012, !is.na(treat_B)) %>%
  select(id_estab, treat_B) %>%
  group_by(id_estab) %>%
  summarise(stable = dplyr::n_distinct(treat_B) == 1L, .groups = "drop") %>%
  filter(stable) %>%
  select(id_estab)

stable_panel <- main_panel %>%
  filter(year >= 2003, year <= 2012) %>%
  semi_join(stable_ids, by = "id_estab")

rm(stable_ids)
invisible(gc(full = TRUE))

log_msg("Stable (non-mover) sample: %d rows (vs. %d in main_panel)", nrow(stable_panel), nrow(main_panel))

res_closure <- fit_establishment_models(stable_panel, "morte", context = "Table C.6 / Closure")
res_reloc   <- fit_establishment_models(stable_panel, "reloc_tract_tminus1", context = "Table C.6 / Relocation")

emit_closure_relocation_table(
  res_closure, res_reloc,
  out_path = file.path(tables_dir, "Tab_C06_Dropping_Movers_5km_tract.tex"),
  caption  = "Establishment-Level Estimates Dropping Units that Moved Across Areas",
  label    = "rob8:est",
  notes_full = "Establishment fixed effects, year fixed effects, and a census tract trend are included in all estimations. The treatment radius ranges from 0--5 km from flood spots; the control ring is between 50--80 km. Standard errors clustered at the census-tract level are in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.1."
)

log_msg("=== 06_table_C6_business_sorting.R: done ===")
