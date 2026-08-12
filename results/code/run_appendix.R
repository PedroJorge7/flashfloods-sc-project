# ============================================================================
# run_appendix.R  [appendix robustness checks]
#
# Runs the appendix robustness scripts using
# utils_establishment.R / worker_functions.R.
# (same correction as run_main_estimates.R: fixed baseline treatment, no
# mover-year outcome nulling).
#
# `steps` is built up incrementally, simplest checks first: scripts that
# just call build_establishment_panel()/output_empregados()
# with different bands need no new logic and were added first; scripts that
# need new logic in the shared utils (remove_treat_control_mob, exposed
# workers, custom inference, etc.) are added as that logic is built out.
# ============================================================================

run_start_time <- Sys.time()

resolve_paths <- function() {
  ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(ofile) || !nzchar(ofile)) {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
    if (length(file_arg) > 0) ofile <- file_arg[1]
  }
  if (!is.null(ofile) && nzchar(ofile) && file.exists(ofile)) {
    code_dir          <- normalizePath(dirname(ofile), winslash = "/", mustWork = TRUE)
    results_dir       <- dirname(code_dir)
    true_project_root <- dirname(results_dir)
  } else {
    true_project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
    results_dir       <- file.path(true_project_root, "results")
    code_dir          <- file.path(results_dir, "code")
  }
  list(code_dir = code_dir, results_dir = results_dir, project_root = true_project_root)
}
paths <- resolve_paths()

project_root <- paths$project_root
script_dir   <- paths$code_dir
if (!dir.exists(file.path(project_root, "data"))) stop("Could not confirm project_root (data/ not found) at: ", project_root)

main_dir     <- file.path(script_dir, "Main Estimates")
appendix_dir <- file.path(script_dir, "Appendix")
output_root  <- file.path(paths$results_dir, "analysis")
tables_dir   <- file.path(output_root, "tables")
figures_dir  <- file.path(output_root, "figures")
logs_dir     <- file.path(output_root, "logs")
cache_dir    <- file.path(output_root, "cache")
for (d in c(tables_dir, figures_dir, logs_dir, cache_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

source(file.path(script_dir, "utils", "path_utils.R"))
source(file.path(script_dir, "utils", "utils_establishment.R"))

source(file.path(main_dir, "00a_load_establishment_data.R"))
source(file.path(main_dir, "00b_load_worker_data.R"))

steps <- c(
  # --- unaffected supporting exhibits retained in the complete runner ---
  "A_Municipal_Balance/01_table_A1_municipal_balance.R",
  # --- simplest: vary distance bands only, reuse panel builder / output_empregados() as-is ---
  "B_Distance_Band_Analysis/01_table_B1_distance_band_analysis.R",
  "C_Establishment_Robustness/01_figure_C1_alternative_control_rings.R",
  "C_Establishment_Robustness/02_figure_C2_alternative_treatment_radius.R",
  "C_Establishment_Robustness/07_table_C7_2011_floods.R",
  "C_Establishment_Robustness/09_figure_C9_placebo_test.R",
  "C_Establishment_Robustness/06_table_C6_business_sorting.R",
  "G_Worker_Robustness/01_figure_G1_alternative_control_rings_workers.R",
  "G_Worker_Robustness/02_figure_G2_alternative_treatment_radius_workers.R",
  "G_Worker_Robustness/03_figure_G3_extended_post_treatment_workers.R",
  # --- more complex: extra covariates, custom inference, extended definitions, new modes ---
  "C_Establishment_Robustness/03_table_C3_establishment_controls.R",
  "C_Establishment_Robustness/04_table_C4_extended_post_treatment.R",
  "C_Establishment_Robustness/08_table_C8_closure_alternative_definitions.R",
  "C_Establishment_Robustness/05_table_C5_alternative_inference.R",
  "E_Workers_Balancing/01_table_E1_workers_balancing.R",
  "F_Exposed_Workers/01_table_F1_exposed_workers.R",
  "F_Exposed_Workers/03_table_F3_dropping_movers_workers.R",
  "G_Worker_Robustness/04_figure_G4_dropping_movers_workers.R",
  "H_Robust_Confidence_Intervals/01_figure_H1_honestdid_sensitivity.R"
)

for (f in steps) {
  log_msg("--- Running Appendix/%s ---", f)
  source(file.path(appendix_dir, f))
}

log_msg("run_appendix.R: all steps complete in %.1f min",
        as.numeric(difftime(Sys.time(), run_start_time, units = "mins")))
