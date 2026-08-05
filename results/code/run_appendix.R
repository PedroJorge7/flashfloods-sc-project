# ============================================================================
# run_appendix.R
#
# Runs every script that produces an appendix table or figure, organized by
# appendix section (A-H) and, within each section, in the order results
# appear. See README.md for details and required inputs.
#
# Appendix D (Auxiliary Maps) has no script: those are static images, not
# computed from the data.
# ============================================================================

run_start_time <- Sys.time()

resolve_project_root <- function() {
  ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(ofile) && nzchar(ofile)) {
    return(normalizePath(dirname(ofile), winslash = "/", mustWork = TRUE))
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

project_root <- resolve_project_root()
setwd(project_root)
if (!dir.exists(file.path(project_root, "results", "code")) || !dir.exists(file.path(project_root, "data"))) {
  stop("Could not confirm project_root at: ", project_root)
}

script_dir    <- file.path(project_root, "results", "code")
main_dir      <- file.path(script_dir, "Main Estimates")
appendix_dir  <- file.path(script_dir, "Appendix")
output_root   <- file.path(project_root, "results")
tables_dir    <- file.path(output_root, "analysis")
figures_dir   <- file.path(output_root, "analysis")
logs_dir      <- file.path(output_root, "logs")
cache_dir     <- file.path(output_root, "cache")
for (d in c(tables_dir, figures_dir, logs_dir, cache_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

source(file.path(script_dir, "utils", "path_utils.R"))
source(file.path(script_dir, "utils", "utils_establishment.R"))

# Several appendix sections need both the establishment panel and the worker
# data, so both loaders are run up front (mirrors run_main_estimates.R).
source(file.path(main_dir, "00a_load_establishment_data.R"))
source(file.path(main_dir, "00b_load_worker_data.R"))

steps <- c(
  "A_Municipal_Balance/01_table_A1_municipal_balance.R",
  "B_Distance_Band_Analysis/01_table_B1_distance_band_analysis.R",
  "C_Establishment_Robustness/01_figure_C1_alternative_control_rings.R",
  "C_Establishment_Robustness/02_figure_C2_alternative_treatment_radius.R",
  "C_Establishment_Robustness/03_table_C3_establishment_controls.R",
  "C_Establishment_Robustness/04_table_C4_extended_post_treatment.R",
  "C_Establishment_Robustness/05_table_C5_alternative_inference.R",
  "C_Establishment_Robustness/06_table_C6_business_sorting.R",
  "C_Establishment_Robustness/07_table_C7_2011_floods.R",
  "C_Establishment_Robustness/08_table_C8_closure_alternative_definitions.R",
  "C_Establishment_Robustness/09_figure_C9_placebo_test.R",
  "E_Workers_Balancing/01_table_E1_workers_balancing.R",
  "F_Exposed_Workers/01_table_F1_exposed_workers.R",
  "G_Worker_Robustness/01_figure_G1_alternative_control_rings_workers.R",
  "G_Worker_Robustness/02_figure_G2_alternative_treatment_radius_workers.R",
  "G_Worker_Robustness/03_figure_G3_extended_post_treatment_workers.R",
  "G_Worker_Robustness/04_figure_G4_dropping_movers_workers.R",
  "H_Robust_Confidence_Intervals/01_figure_H1_honestdid_sensitivity.R"
)

start_from <- Sys.getenv("RUN_APPENDIX_FROM", unset = "")
if (nzchar(start_from)) {
  start_index <- match(start_from, steps)
  if (is.na(start_index)) {
    stop(
      "RUN_APPENDIX_FROM must exactly match one of the appendix step paths. Got: ",
      start_from
    )
  }
  steps <- steps[start_index:length(steps)]
  log_msg("Resuming appendix run from: %s", start_from)
}

for (f in steps) {
  log_msg("--- Running Appendix/%s ---", f)
  # Keep exhibit-specific models and temporary panels out of the global
  # environment. This is especially important after the 999-replication
  # bootstrap in C.5, which otherwise leaves enough objects resident to make
  # later dplyr operations swap heavily on Windows.
  step_env <- new.env(parent = .GlobalEnv)
  sys.source(file.path(appendix_dir, f), envir = step_env)
  rm(step_env)
  invisible(gc(full = TRUE))
}

log_msg("run_appendix.R: all steps complete in %.1f min",
        as.numeric(difftime(Sys.time(), run_start_time, units = "mins")))
