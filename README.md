# Flash Floods, Business Adjustment, and Ripple Effects on Displaced Workers

## Replication package

This repository contains the replication code for *Flash Floods, Business Adjustment, and Ripple Effects on Displaced Workers*, by Pedro Jorge Alves, Philipp Ehrl, and Ricardo C. A. Lima.

The paper combines restricted employer-employee records from the Relação Anual de Informações Sociais (RAIS) with geocoded establishment locations and a polygon layer representing the areas affected by the 2008 Santa Catarina flash floods. The public repository distributes code and documentation only. It does not distribute RAIS microdata, geocoded establishment records, the derived analytical panels, or the third-party flood layer.

## From the national RAIS data to the analytical samples

The public code begins with restricted analytical files constructed from the national RAIS employer-employee records for 2002 - 2016. Construction took place in a secure environment and used complete CNPJ and CPF identifiers to follow establishments and workers anywhere in Brazil. The underlying microdata, identifiers, geocoded records, and construction scripts are not distributed.

Baseline establishments in Santa Catarina were geocoded using complete addresses and postal codes. Imprecise matches and establishments without a valid 2007 geocode were excluded. Exposure is the minimum distance from the establishment's 2007 location to the nearest polygon in the 2008 flood map supplied by Marinho et al. (2012). Treated establishments are located 0 - 5 km from a mapped flood spot; controls are located 50 - 80 km away. Treatment status remains fixed after relocation.

The establishment panel follows the baseline establishments through CNPJ. The main period is 2003 - 2012, with extensions through 2016. Closure is recorded in an establishment's last year in RAIS, and relocation indicates a change in census tract between consecutive years. The restricted input is `Natural Disastrer Santa Catarina - Dataset.dta`.

The worker sample consists of workers formally employed in 2007 at baseline establishments. This restriction is required because the propensity-score covariates are measured in 2007. Workers are subsequently followed through CPF in the national RAIS, including at other establishments and outside Santa Catarina. The restricted input is `workers_clean_data.rds`. The outcome identifies formal employment on December 31; a zero may represent either unemployment or informal employment.

## Repository structure

```text
flashfloods-sc-project/
├── Main_analysis.Rmd
├── README.md
├── data/
└── results/
    ├── code/
    │   ├── Main Estimates/
    │   ├── Appendix/
    │   └── utils/
    └── analysis/
```

`Main_analysis.Rmd` is the documented entry point for the replication.

`data/` is the local location of the restricted analytical inputs and supporting files. Git ignores this directory. Authorized users must populate it before running the replication.

`results/code/Main Estimates/` contains the scripts for the main tables and figures. The loader scripts read the establishment and worker analytical files and prepare the common estimation objects.

`results/code/Appendix/` contains the appendix and robustness analyses, organized by appendix section.

`results/code/utils/` contains shared functions for path handling, panel preparation, estimation, and output formatting.

`results/analysis/` receives generated tables and figures.

## Results and replication scripts

### Main results

| Result | Script |
| - -| - -|
| Figure 1 — Geographical distribution of the affected area | `results/code/Main Estimates/01_figure1_geographical_distribution_affected_area.R` |
| Table 1 — Summary statistics for 2007 | `results/code/Main Estimates/01_table1_summary_statistics.R` |
| Figure 2 — Evolution of aggregate outcomes | `results/code/Main Estimates/02_figure2_evolution_aggregate_outcomes.R` |
| Table 2 — Establishment adjustment | `results/code/Main Estimates/03_table2_establishment_adjustment.R` |
| Figure 4 — Event study of establishment adjustment | `results/code/Main Estimates/04_figure4_event_study_establishments.R` |
| Table 3 — Closure by sector | `results/code/Main Estimates/05_table3_effect_by_sector.R` |
| Table 4 — Closure by business size | `results/code/Main Estimates/06_table4_effect_by_business_size.R` |
| Table 5 — Hazard of establishment closure | `results/code/Main Estimates/07_table5_hazard_establishment_closure.R` |
| Table 6 — Dismissed workers | `results/code/Main Estimates/08_table6_dismissed_workers.R` |
| Figure 5 — Event study of dismissed workers | `results/code/Main Estimates/09_figure5_event_study_workers.R` |
| Figure 5, skill groups | `results/code/Main Estimates/10_figure5b_event_study_workers_by_skill.R` |

### Appendix results

| Result | Script |
| - -| - -|
| Table A.1 — Municipal balance | `results/code/Appendix/A_Municipal_Balance/01_table_A1_municipal_balance.R` |
| Table B.1 — Distance-band analysis | `results/code/Appendix/B_Distance_Band_Analysis/01_table_B1_distance_band_analysis.R` |
| Table B.3 — Dropping establishments that moved | `results/code/Appendix/B_Distance_Band_Analysis/03_table_B3_business_sorting.R` |
| Figures C.1 - C.2 — Alternative control and treatment rings | `results/code/Appendix/C_Establishment_Robustness/` |
| Tables C.3 - C.8 and Figure C.9 — Establishment robustness | `results/code/Appendix/C_Establishment_Robustness/` |
| Table E.1 — Worker balancing | `results/code/Appendix/E_Workers_Balancing/01_table_E1_workers_balancing.R` |
| Tables F.1 and F.3 — Exposed-worker estimates | `results/code/Appendix/F_Exposed_Workers/` |
| Figures G.1 - G.4 — Worker robustness | `results/code/Appendix/G_Worker_Robustness/` |
| Figure H.1 — HonestDiD sensitivity analysis | `results/code/Appendix/H_Robust_Confidence_Intervals/01_figure_H1_honestdid_sensitivity.R` |

## Data used in the article

The principal inputs are:

- `Natural Disastrer Santa Catarina - Dataset.dta`, the restricted establishment analytical panel derived from RAIS, geocoded establishment locations, and flood exposure measures;
- `workers_clean_data.rds`, the restricted worker analytical panel derived from linked RAIS employment records;
- `inundacao/inundacao_2008.shp` and its Shapefile sidecars, the third-party SAR-derived flood polygon layer;
- `municipal_balance_5km_nucleo.rds`, the municipal variables used in the municipal balance analysis;
- `firm_coordinates.dta`, the establishment coordinates used by specifications with spatially robust standard errors.

The exact filenames are part of the code interface. Restricted files must be stored locally in `data/` and must not be committed to the repository.

## Replication instructions

Open `flashfloods-sc-project.Rproj`, restore the recorded package environment with `renv::restore()`, and populate `data/` with the authorized analytical inputs before executing the code. The project root is used as the working directory.

`Main_analysis.Rmd` provides the documented replication entry point and permits the main results and appendix to be run separately.

To run the main results directly, use:

```r
source("results/code/run_main_estimates.R")
```

To run the appendix, use:

```r
source("results/code/run_appendix.R")
```

The scripts write tables and figures to `results/analysis/`. Cached estimation objects may be written to `results/cache/`, and execution logs may be written to `results/logs/`.

The public replication begins from the restricted establishment and worker analytical panels. The secure construction scripts used to produce these files from the national RAIS data do not form part of this package.

## Data Availability and Confidentiality

The data used in this project are not publicly available and are not distributed with this repository. The RAIS files and the derived analytical data contain identified or potentially identifiable information about establishments and workers. They must be handled in accordance with Brazil's General Data Protection Law, Lei No. 13,709/2018, and the confidentiality conditions governing access to RAIS.

The restricted data contain complete CNPJ and CPF identifiers as well as geocoded establishment information. The authors are not authorized to redistribute the national RAIS files, establishment addresses, coordinates, identifier crosswalks, or the derived establishment and worker panels. Researchers seeking to use RAIS must obtain authorization directly from the responsible Brazilian data custodian and comply with the applicable confidentiality and secure-use procedures.

The SAR imagery and derived 2008 flood polygons are also unavailable through this repository. They were produced and made available by the authors of Marinho et al. (2012). The project authors do not hold redistribution rights. Researchers seeking access should contact the original data producer. This third-party restriction is separate from the legal and contractual restrictions applying to RAIS.

## Reference

Marinho, Rogério Ribeiro, Waldir Renato Paradella, Camilo Daleles Rennó, and C. G. de Oliveira. 2012. “Aplicação de imagens SAR orbitais em desastres naturais: mapeamento das inundações de 2008 no Vale do Itajaí, SC.” *Revista Brasileira de Cartografia* 64(3): 317–330.
