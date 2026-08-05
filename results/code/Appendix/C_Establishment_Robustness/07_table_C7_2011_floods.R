# ============================================================================
# Appendix C, Table C.7 — Establishment-Level Estimates Dropping Units
# Affected by the 2011 Floods
# Treatment: 0-5 km vs. 50-80 km control. Clustering: census tract. Drops
# establishments located in the 11-municipality, 2011-specific ECP list
# entirely. Depends on `main_panel`.
# ============================================================================

log_msg("=== 07_table_C7_2011_floods.R: start ===")

if (!("mun" %in% names(main_panel))) stop("Column 'mun' not found; cannot apply ECP-2011 municipality filter.")

ecp2011_panel <- main_panel %>%
  mutate(
    mun_num = make_numeric_id(mun),
    ecp_2011 = if_else(!is.na(mun_num) & mun_num %in% ecp_municipalities_2011, 1, 0)
  ) %>%
  filter(ecp_2011 == 0)

log_msg("ECP-2011-filtered sample: %d rows (vs. %d in main_panel)", nrow(ecp2011_panel), nrow(main_panel))

res_closure <- fit_establishment_models(ecp2011_panel, "morte", context = "Table C.7 / Closure")
res_reloc   <- fit_establishment_models(ecp2011_panel, "reloc_tract_tminus1", context = "Table C.7 / Relocation")

emit_closure_relocation_table(
  res_closure, res_reloc,
  out_path = file.path(tables_dir, "Tab_C07_Dropping_2011_Floods_5km_tract.tex"),
  caption  = "Establishment-Level Estimates Dropping Units Affected by the 2011 Floods",
  label    = "rob9:est",
  notes_full = "Both estimates exclude establishments affected by the 2011 floods. Establishment fixed effects, year fixed effects, and a census tract trend are included in all estimations. The treatment radius ranges from 0--5 km from flood spots; the control ring is between 50--80 km. Standard errors clustered at the census-tract level are in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.1."
)

log_msg("=== 07_table_C7_2011_floods.R: done ===")
