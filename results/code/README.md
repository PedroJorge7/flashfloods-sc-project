# Replication code

Code for reproducing every table and figure in *Flash Floods, Business
Adjustment, and Ripple Effects on Displaced Workers* (Alves, Ehrl, and Lima).

## Folder structure

```
results/code/
  README.md
  run_main_estimates.R      Runs every main-text table/figure, in order
  run_appendix.R            Runs every appendix table/figure, in order
  run_single_appendix_step.R  Runs one appendix script in its own process (low-memory machines)
  utils/
    path_utils.R              data_path() helper
    utils_establishment.R     Establishment-level panel builder, model fitting, LaTeX/figure output helpers
    worker_functions.R        Worker-level PSM + regression + table/figure helpers
  Main Estimates/             One script per main-text table/figure
  Appendix/
    B_Distance_Band_Analysis/
    C_Establishment_Robustness/
    E_Workers_Balancing/
    F_Exposed_Workers/
    G_Worker_Robustness/
    H_Robust_Confidence_Intervals/
```

Appendix A contains the municipal balance analysis. Appendix D consists of
static maps.

Figure 5b (`Main Estimates/10_figure5b_event_study_workers_by_skill.R`) is an extra exhibit not in the manuscript — the worker event study split by High/Low Skill.

## Run order

1. `run_main_estimates.R` — loads the data, builds the base panels, produces all main-text exhibits.
2. `run_appendix.R` — produces all appendix exhibits (does not depend on step 1).

On low-memory machines, run appendix scripts one at a time instead:
```
Rscript run_single_appendix_step.R "C_Establishment_Robustness/05_table_C5_alternative_inference.R" 1
```
(the trailing `1` loads the establishment panel; omit it for worker-only scripts).

## Software requirements

Package versions and transitive dependencies are recorded in the project-level
`renv.lock`. From the project root, run `renv::restore()` before replication.

## Input data

Reads the project-level `data/` folder via `data_path()`.

## Output

Writes to `results/analysis/` (`tables/`, `figures/`, `cache/`, `logs/`).

## Note on C.6 / G.4 ("dropping movers")

These scripts drop units ever observed with a current-year or current-job
distance band opposite their baseline classification.
