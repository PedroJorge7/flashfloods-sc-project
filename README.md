# Flash Floods, Business Adjustment, and Ripple Effects on Displaced Workers — Replication Package

**Authors:** Pedro Jorge Alves, Philipp Ehrl, and Ricardo C. A. Lima

## Overview

This repository contains the replication files for *Flash Floods, Business
Adjustment, and Ripple Effects on Displaced Workers*. The paper combines
geocoded employer–employee data with mapped flood spots from the 2008 Santa
Catarina flash floods. Establishments within 5 km form the treatment group,
and establishments between 50 and 80 km form the baseline control group.

## Structure

```text
flashfloods-sc-project/
├── README.md
├── data/
└── results/
    ├── code/
    ├── analysis/
    ├── cache/
    └── logs/
```

1. **[data](./data)** contains the databases used in the estimations.
2. **[results/code](./results/code)** contains the R replication code.
3. **[results/analysis](./results/analysis)** contains the final tables and
   figures used by the manuscript.
4. **[results/cache](./results/cache)** contains intermediate panels that
   avoid rebuilding computationally expensive samples.
5. **[results/logs](./results/logs)** contains the complete appendix
   reproduction log.

## Main Data

- `Natural Disastrer Santa Catarina - Dataset.dta`: establishment panel.
- `workers_clean_data.rds`: worker panel.
- `municipal_balance_5km_nucleo.rds`: municipal balance database.
- `bndes_desembolsos_gold.parquet`: real BNDES disbursements used in
  Appendix Table A.1.
- `firm_coordinates.dta`: coordinates used for Conley standard errors.
- `indice.xlsx`: price index used to deflate worker earnings.

The establishment and worker microdata may be subject to access restrictions.

## Replication

Open `flashfloods-sc-project.Rproj`, set the project root as the working
directory, and run:

```r
source("results/code/run_main_estimates.R")
source("results/code/run_appendix.R")
```

Both runners save final exhibits directly in `results/analysis`.

## Main Results

| Exhibit | Code | Output |
|---|---|---|
| Figure 1 — Geographical Distribution of the Affected Area | `Main Estimates/01_figure1_geographical_distribution_affected_area.R` | `map_new.png` |
| Table 1 — Summary Statistics for the Pre-Disaster Year 2007 | `Main Estimates/01_table1_summary_statistics.R` | `Tab_01_Summary_Statistics_5km_tract.tex` |
| Figure 2 — Evolution of Aggregate Outcomes (2003–2012) | `Main Estimates/02_figure2_evolution_aggregate_outcomes.R` | `graph_descritivo.png` |
| Table 2 — The Effect on Establishments’ Adjustment | `Main Estimates/03_table2_establishment_adjustment.R` | `Tab_02_Establishment_Adjustment_5km_tract.tex` |
| Figure 4 — Event Study of Establishments’ Adjustment | `Main Estimates/04_figure4_event_study_establishments.R` | `event_study.png` |
| Table 3 — Closure by Sector | `Main Estimates/05_table3_effect_by_sector.R` | `Tab_03_Effect_by_Sector_5km_tract.tex` |
| Table 4 — Closure by Business Size | `Main Estimates/06_table4_effect_by_business_size.R` | `Tab_04_Effect_by_Business_Size_5km_tract.tex` |
| Table 5 — Hazard of Establishment Closure | `Main Estimates/07_table5_hazard_establishment_closure.R` | `Tab_05_Hazard_Establishment_Closure_5km_tract.tex` |
| Table 6 — Dismissed Workers | `Main Estimates/08_table6_dismissed_workers.R` | `Tab_06_Dismissed_Workers_5km_tract.tex` |
| Figure 5 — Event Study of Dismissed Workers | `Main Estimates/09_figure5_event_study_workers.R` | `event_study_empregados.png` |

Figure 1 is generated directly as `map_new.png`, matching the filename used
by the LaTeX source. Figure 3 and the auxiliary calamity maps are stored in
`results/analysis`.

## Appendix Results

| Exhibit | Script folder | Output title |
|---|---|---|
| Table A.1 — Municipal Balance | `Appendix/A_Municipal_Balance` | `Tab_A01_Municipal_Balance_5km_Nucleo.tex` |
| Table B.1 — Distance Bands | `Appendix/B_Distance_Band_Analysis` | `Tab_B01_Distance_Band_Analysis_5km_tract.tex` |
| Figure C.1 — Alternative Control Rings | `Appendix/C_Establishment_Robustness` | `results_controles.png` |
| Figure C.2 — Alternative Treatment Radius | `Appendix/C_Establishment_Robustness` | `change_treatment.png` |
| Tables C.3–C.8 — Establishment Robustness | `Appendix/C_Establishment_Robustness` | Files beginning with `Tab_C` |
| Figure C.9 — Falsification Test | `Appendix/C_Establishment_Robustness` | `results_falso_tratamento2.png` |
| Table E.1 — Worker Balancing | `Appendix/E_Workers_Balancing` | `Tab_E01_Workers_Balancing_5km_tract.tex` |
| Table F.1 — Exposed Workers | `Appendix/F_Exposed_Workers` | `Tab_F01_Effect_Exposed_Workers_5km_tract.tex` |
| Figures G.1–G.4 — Worker Robustness | `Appendix/G_Worker_Robustness` | Worker robustness PNG files |
| Figure H.1 — HonestDiD | `Appendix/H_Robust_Confidence_Intervals` | `Fig_G01a_...png` and `Fig_G01b_...png` |

The complete appendix includes a 999-replication wild cluster bootstrap and
HonestDiD sensitivity analyses.
