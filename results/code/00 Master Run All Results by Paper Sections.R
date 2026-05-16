# ============================================================================
# Master runner: paper results ordered by section
# Use:
#   source("./results/code/00 Master Run All Results by Paper Sections.R")
# ============================================================================

resolve_project_root <- function() {
  ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  
  if (!is.null(ofile) && nzchar(ofile)) {
    return(
      normalizePath(
        file.path(dirname(ofile), "..", ".."),
        winslash = "/",
        mustWork = TRUE
      )
    )
  }
  
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

project_root <- resolve_project_root()
setwd(project_root)

dir.create("./results/analysis", recursive = TRUE, showWarnings = FALSE)

scripts_by_section <- list(
  # 1. Descriptive Evidence
  "1. Descriptive Evidence" = c(
    "Table 1" = "./results/code/Table 1 Summary Statistics for the Pre-Disaster Year 2007.R",
    "Figure 2" = "./results/code/Figure 2 Evolution of Aggregate Outcomes (2003-2012).R",
    "Figure 3" = "./results/code/Figure 3 The Disaster Coverage Area of the 2008 Santa Catarina Flash Floods..R"
  ),
  
  # 2. Main Establishment Results
  "2. Main Establishment Results" = c(
    "Table 2" = "./results/code/Table 2 Definition of Treatment Radius Effects of Flooding using Different Treatment Bands.R",
    "Table 3" = "./results/code/Table 3 The Effect of the Flash Floods on the Spatial Distribution of Establishments.R",
    "Figure 4" = "./results/code/Figure 4 Event Study Effect of the Flash Floods on the Spatial Distribution of Establishments.R",
    "Table 4" = "./results/code/Table 4 The Effect of the Flash Floods on Establishment Closure by Sector.R",
    "Table 5" = "./results/code/Table 5 The Effect of the Flash Floods on Establishment Closure by Business Size.R",
    "Table 6" = "./results/code/Table 6 Effect of Flash Floods on Hazard of Establishment Closure.R"
  ),
  
  # 3. Worker Results
  "3. Worker Results" = c(
    "Table 7" = "./results/code/Table 7 The Effect of Disaster-induced Closures on the Dismissed Workers.R",
    "Figure 5" = "./results/code/Figure 5 Event Study Effect of Disaster-induced Closures on Dismissed Workers.R"
  ),
  
  # 4. Appendix A
  "4. Appendix A" = c(
    "Table A.1" = "./results/code/Table A.1 Definition of Treatment Radius Effects of Flooding on Relocation.R"
  ),
  
  # 5. Appendix B
  "5. Appendix B" = c(
    "Figure B.1" = "./results/code/Figure B.1 Alternative Sizes of the Control Rings in Establishment-Level Estimates..R",
    "Figure B.2" = "./results/code/Figure B.2 Alternative Sizes of the Treatment Radius in Establishment-Level Estimates..R",
    "Figure B.3" = "./results/code/Figure B.3 Establishment Level Estimates Using Alternative Inference Procedures.R",
    "Figure B.4" = "./results/code/Figure B.4 Falsification Test for the Control Area in Establishment-Level Estimates.R",
    "Table B.1" = "./results/code/Table B.1 Establishment-Level Estimates Including Control Variables.R",
    "Table B.2" = "./results/code/Table B.2 Establishment-Level Estimates Increasing the Post-Treatment Period.R",
    "Table B.3" = "./results/code/Table B.3 Establishment-Level Estimates Dropping Units that Moved Across Areas.R",
    "Table B.4" = "./results/code/Table B.4 Establishment-Level Estimates Dropping Units Affected by the 2011 Floods.R"
  ),
  
  # 6. Appendix D
  "6. Appendix D" = c(
    "Table D.1" = "./results/code/Table D.1 Comparison of Variables Before and After Data Balancing.R"
  ),
  
  # 7. Appendix E
  "7. Appendix E" = c(
    "Table E.1" = "./results/code/Table E.1 The Effect of the Flash Floods on Exposed Workers.R"
  ),
  
  # 8. Appendix F
  "8. Appendix F" = c(
    "Figure F.1" = "./results/code/Figure F.1 Alternative Sizes of the Control Rings in Worker-Level Estimations.R",
    "Figure F.2" = "./results/code/Figure F.2 Alternative Sizes of the Treatment Radius in Worker-Level Estimations.R",
    "Table F.1" = "./results/code/Table F.1 Worker Level Estimates Increasing Post-Treatment Period.R",
    "Table F.2" = "./results/code/Table F.2 Worker Level Estimates Dropping Units that Moved Across Areas.R"
  ),
  
  # 9. Appendix G
  "9. Appendix G" = c(
    "Figure G.1" = "./results/code/Figure G.1 Sensivity Analysis of the HonestDiD Approach.R"
  )
)

run_one_script <- function(paper_id, script_path, continue_on_error = TRUE) {
  file_info <- file.info(script_path)
  
  if (nrow(file_info) == 0 || is.na(file_info$size)) {
    message(sprintf("[MISSING] %s -> %s", paper_id, script_path))
    return(data.frame(item = paper_id, script = script_path, status = "missing", stringsAsFactors = FALSE))
  }
  
  if (file_info$size == 0) {
    message(sprintf("[SKIP] %s -> empty file: %s", paper_id, script_path))
    return(data.frame(item = paper_id, script = script_path, status = "empty", stringsAsFactors = FALSE))
  }
  
  message(sprintf("[RUN] %s -> %s", paper_id, basename(script_path)))
  script_env <- new.env(parent = globalenv())
  
  tryCatch(
    {
      source(script_path, local = script_env, chdir = FALSE, encoding = "UTF-8")
      data.frame(item = paper_id, script = script_path, status = "ok", stringsAsFactors = FALSE)
    },
    error = function(e) {
      message(sprintf("[ERROR] %s -> %s", paper_id, conditionMessage(e)))
      if (!continue_on_error) {
        stop(e)
      }
      data.frame(item = paper_id, script = script_path, status = "error", stringsAsFactors = FALSE)
    }
  )
}

run_all_results <- function(continue_on_error = TRUE) {
  run_log <- list()
  log_index <- 1L
  
  for (section_name in names(scripts_by_section)) {
    cat("\n# ", section_name, "\n", sep = "")
    section_scripts <- scripts_by_section[[section_name]]
    
    for (paper_id in names(section_scripts)) {
      log_entry <- run_one_script(
        paper_id = paper_id,
        script_path = unname(section_scripts[[paper_id]]),
        continue_on_error = continue_on_error
      )
      log_entry$section <- section_name
      run_log[[log_index]] <- log_entry
      log_index <- log_index + 1L
    }
  }
  
  log_df <- do.call(rbind, run_log)
  rownames(log_df) <- NULL
  
  cat("\n# Run Summary\n")
  print(log_df[, c("section", "item", "status")], row.names = FALSE)
  
  invisible(log_df)
}

run_all_results(continue_on_error = TRUE)
