# ============================================================================
#  Appendix C, Table C.6 — Establishment-
# Level Estimates Dropping Units that Moved Across Areas
# Treatment: 0-5 km vs. 50-80 km control. Clustering: census tract. Depends
# on `main_panel` (corrected, full-life panel).
# ============================================================================

log_msg("=== 06_table_C6_business_sorting.R: start ===")

stable_panel <- main_panel %>%
  filter(year >= 2003 & year <= 2012) %>%
  mutate(
    .this_year_band = case_when(
      dist_flood <= 5 ~ 1,
      dplyr::between(dist_flood, 50, 80) ~ 0,
      TRUE ~ NA_real_
    ),
    .crossed = !is.na(.this_year_band) & .this_year_band != treat_B
  ) %>%
  group_by(id_estab) %>%
  filter(!any(.crossed)) %>%
  ungroup() %>%
  select(-.this_year_band, -.crossed)

log_msg("Non-ring-crossing (stable) sample: %d rows (vs. %d in main_panel)", nrow(stable_panel), nrow(main_panel))

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
