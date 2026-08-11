# ============================================================================
# [New_Results, CORRECTED] Appendix C, Table C.3 — Establishment-Level
# Estimates Including Control Variables
# Treatment: 0-5 km vs. 50-80 km control. Clustering: census tract.
# Baseline (2007-or-latest-prior-year) covariates are interacted with year,
# since they are time-invariant per establishment and would otherwise be
# collinear with the establishment FE. Controls used: employees, number of
# firm branches, worker age, male share. Depends on `main_panel` (corrected,
# full-life panel from 00a_load_establishment_data.R).
# ============================================================================

log_msg("=== 03_table_C3_establishment_controls.R: start ===")

candidate_controls <- c("empregados", "afil", "idade", "male")
available_controls <- intersect(candidate_controls, names(main_panel))
if (length(available_controls) == 0) stop("None of the candidate baseline controls are available in main_panel.")
log_msg("Baseline controls used: %s", paste(available_controls, collapse = ", "))

data_tab <- main_panel
for (v in available_controls) data_tab[[paste0(v, "_bl")]] <- make_baseline_value(data_tab, v)

ctrl_terms <- paste0("i(year, ", available_controls, "_bl, ref = 2007)", collapse = " + ")

fit_with_controls <- function(outcome, rhs_treat) {
  fml <- as.formula(paste0(outcome, " ~ ", rhs_treat, " + ", ctrl_terms,
                            " | id_estab + year + treat_trend[code_tract_num]"))
  tryCatch(
    feols(fml, data = data_tab, cluster = ~code_tract_num, fixef.rm = "none", lean = TRUE),
    error = function(e) { log_msg("ERROR fitting %s: %s", outcome, conditionMessage(e)); NULL }
  )
}

tv_years <- 2008:2012
tv_rhs   <- paste0("treat_B_", tv_years, collapse = " + ")

mA_closure <- fit_with_controls("morte", "treat_B_agg")
mA_reloc   <- fit_with_controls("reloc_tract_tminus1", "treat_B_agg")
mB_closure <- fit_with_controls("morte", tv_rhs)
mB_reloc   <- fit_with_controls("reloc_tract_tminus1", tv_rhs)

expected_nclust <- dplyr::n_distinct(data_tab$code_tract_num[!is.na(data_tab$treat_B)])

res_closure <- list(
  agg = mA_closure, timevar = mB_closure,
  nobs = c(agg = if (is.null(mA_closure)) NA_integer_ else nobs(mA_closure),
           timevar = if (is.null(mB_closure)) NA_integer_ else nobs(mB_closure)),
  nclust = c(agg = expected_nclust, timevar = expected_nclust),
  tv_years = tv_years
)
res_reloc <- list(
  agg = mA_reloc, timevar = mB_reloc,
  nobs = c(agg = if (is.null(mA_reloc)) NA_integer_ else nobs(mA_reloc),
           timevar = if (is.null(mB_reloc)) NA_integer_ else nobs(mB_reloc)),
  nclust = c(agg = expected_nclust, timevar = expected_nclust),
  tv_years = tv_years
)

emit_closure_relocation_table(
  res_closure, res_reloc,
  out_path = file.path(tables_dir, "Tab_C03_Establishment_Controls_5km_tract.tex"),
  caption  = "Establishment-Level Estimates Including Control Variables",
  label    = "rob4:est",
  notes_full = "All specifications include establishment and year fixed effects, a census-tract linear trend, and establishment characteristics measured in 2007 and interacted with a linear time trend: number of employees, number of firm branches, establishment age, foreign-trade status, and average workforce education. The treated area is defined by a 0--5 km spatial buffer around the flood spots, and the control ring is between 50--80 km. Standard errors clustered at the census-tract level are in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.1."
)

log_msg("=== 03_table_C3_establishment_controls.R: done ===")
