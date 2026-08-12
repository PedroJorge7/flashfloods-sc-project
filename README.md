# Flash Floods, Business Adjustment, and Ripple Effects on Displaced Workers

## Replication package

This repository contains the replication code for *Flash Floods, Business Adjustment, and Ripple Effects on Displaced Workers*, by Pedro Jorge Alves, Philipp Ehrl, and Ricardo C. A. Lima.

The paper combines restricted employer-employee records from the Relação Anual de Informações Sociais (RAIS) with geocoded establishment locations and a polygon layer representing the areas affected by the 2008 Santa Catarina flash floods. The public repository distributes code and documentation only. It does not distribute RAIS microdata, geocoded establishment records, the derived analytical panels, or the third-party flood layer.

## From the national RAIS data to the analytical samples

The data construction was performed in a secure environment. The construction scripts do not form part of the public replication package. The public code begins with the restricted analytical establishment and worker files produced by the procedure described below.

### 1. National RAIS data

RAIS is the official Brazilian matched employer-employee database and covers the entire formal labor market. The restricted national data received for this project cover 2002 through 2016. This period includes the years required for both the main analyses and their temporal extensions. The main analyses subsequently restrict the panel to 2003 through 2012, while extended specifications follow establishments and workers through 2016.

The data received by the authors had already been standardized across years. The provider had harmonized the annual layouts and the relevant variable formats before delivery. The project therefore did not construct an additional year-by-year layout crosswalk from the original annual RAIS files.

The received data contain establishment, employment relationship, worker, address, geographic, industry, earnings, working-hours, tenure, education, gender, establishment-size, and employment-status information.

### 2. Longitudinal identification

Establishments are identified by their complete CNPJ through the variable `id_estab`. Workers are identified directly by CPF through the variable `cpf`. These identifiers allow establishments and workers to be linked across years and followed anywhere in Brazil.

The identifiers in the restricted analytical files are not anonymized. This is a central reason why neither the received RAIS data nor the derived establishment and worker files can be distributed. No CNPJ, CPF, address, coordinate, or individual record is included in the public repository or its generated outputs.

### 3. Geocoding establishment locations

The project used the Google Maps API to geocode establishments in Santa Catarina. We tested geocoding queries based on postal codes and on complete establishment addresses. The returned locations were validated to retain sufficiently precise establishment locations. Low-precision matches corresponding to municipal centroids or broad postal areas were discarded.

The procedure could not accurately geocode 20.5 percent of the initial establishments because their postal codes or addresses were misspelled, incomplete, or referred to broad geographic areas. These establishments were excluded from the analytical sample. Establishments without a valid geocode for the 2007 baseline were also excluded rather than assigned a location from another year.

High-quality geocoding was undertaken for establishments initially located in Santa Catarina. After the baseline sample was defined, the national coverage of RAIS and the CNPJ and CPF identifiers allowed establishments and workers to be followed even when they subsequently moved outside Santa Catarina.

### 4. Mapping the 2008 flood

The flood layer was made available by Marinho and is documented in Marinho et al. (2012). It combines flood classifications derived from TerraSAR-X, RADARSAT-2 ascending and descending passes, and ENVISAT ASAR imagery collected between September 2008 and January 2009.

Marinho et al. (2012) processed the SAR imagery through orthorectification, speckle-noise filtering, and conversion to the backscatter coefficient. This procedure identified 1,022 individual flood spots representing the location and spatial extent of inundation during the 2008 disaster. The authors received and used the resulting polygon layer. The source layer and its derived treatment geometry are third-party data and are not distributed with this repository.

### 5. Calculating exposure to flooding

For each establishment with a valid baseline location, the construction calculates the minimum distance from the establishment point to the boundary of the nearest flood polygon. Establishments located inside a mapped flood polygon receive a distance of zero. The resulting variable, `dist_flood`, is stored in kilometers.

The treatment group contains establishments located from 0 through 5 km from the mapped flood spots. The baseline control group contains establishments located from 50 through 80 km from the flood spots. Establishments in the intermediate area between 5 and 50 km and establishments beyond 80 km do not enter the baseline comparison.

Treatment status is fixed using the establishment's valid pre-disaster location in 2007. A later relocation does not change whether an establishment belongs to the treatment or control group.

### 6. Establishment sample and panel

The construction excludes establishments without employment relationships and establishments with zero employment. It does not exclude establishments based on public-sector status, agriculture, or CNAE activity. No sector-specific restriction is applied when the main establishment sample is constructed.

After the baseline geographic sample is selected, the complete CNPJ is used to recover the same establishments from the national RAIS data in every year. Establishments continue to be observed after relocation, including when their later locations fall outside Santa Catarina or outside the original treatment and control bands.

The main establishment panel covers 2003 through 2012. Extended specifications use observations through 2016. The restricted establishment input expected by the replication code is:

- `Natural Disastrer Santa Catarina - Dataset.dta`

The estimation code uses this file to create fixed treatment indicators, year-specific treatment interactions, post-treatment indicators, census-tract trends, and the relocation outcome.

An establishment is classified as closed in year (t) when it is present in the national RAIS registry in year (t) but absent in year (t+1). The outcome is therefore indexed by the establishment's last year in RAIS. This definition does not identify a closure when an establishment closes one CNPJ and continues its activities under a new CNPJ.

Relocation equals one in year (t) when the establishment is observed in a different census tract in year (t+1) relative to year (t). Because establishments are followed nationally through CNPJ, this measure also captures moves outside Santa Catarina.

### 7. Worker sample and panel

The worker panel is assembled from the employment relationships associated with establishments selected at baseline. CPF is then used to recover each selected worker's formal employment history in the national RAIS. Workers remain under observation when they obtain formal employment at another establishment or outside Santa Catarina.

The current worker sample does not require employment at the same establishment in both 2006 and 2007. This earlier restriction was removed so that employment has genuine variation during the pre-treatment period. The public worker loader therefore uses the complete restricted worker input without applying the former `emprego_06_07` and `mesma_empresa_06_07` filter.

The main worker analysis focuses on workers formally employed on December 31, 2008, by establishments located in the treatment area that were present in the 2008 RAIS and absent from the 2009 RAIS. The control group contains observationally similar workers associated with establishments in the control area and is selected through nearest-neighbor propensity-score matching without replacement.

The restricted worker input expected by the replication code is:

- `workers_clean_data.rds`

The public code constructs the worker-year panel, fixes treatment from the baseline employment relationship, performs propensity-score matching, and creates the employment and treatment variables used in estimation.

The main worker outcome equals one when the worker has a formal employment relationship active on December 31 of year (t), and zero otherwise. A zero does not distinguish unemployment from informal employment because RAIS covers only formal employment.

### 8. Deflating remuneration

The replication uses the Extended National Consumer Price Index, IPCA, produced by the Brazilian Institute of Geography and Statistics, IBGE. The series in `indice.xlsx` uses 2017 as its base year.

The worker code uses this index to deflate average remuneration, December remuneration, and hourly remuneration. Hourly remuneration is calculated by dividing average remuneration by contracted working hours.

## Repository structure

```text
flashfloods-sc-project/
├── Main_analysis.Rmd
├── README.md
├── flashfloods-sc-project.Rproj
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

## Data used in the article

The principal inputs are:

- `Natural Disastrer Santa Catarina - Dataset.dta`, the restricted establishment analytical panel derived from RAIS, geocoded establishment locations, and flood exposure measures;
- `workers_clean_data.rds`, the restricted worker analytical panel derived from linked RAIS employment records;
- `inundacao/inundacao_2008.shp` and its Shapefile sidecars, the third-party SAR-derived flood polygon layer;
- `indice.xlsx`, the IPCA series used to deflate remuneration;
- `municipal_balance_5km_nucleo.rds`, the municipal variables used in the municipal balance analysis;
- `firm_coordinates.dta`, the establishment coordinates used by specifications with spatially robust standard errors.

The exact filenames are part of the code interface. Restricted files must be stored locally in `data/` and must not be committed to the repository.

## Replication instructions

Open `flashfloods-sc-project.Rproj` and use the project root as the working directory. Populate `data/` with the authorized analytical inputs before executing the code.

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

No direct identifier, address record, establishment coordinate, worker record, or extract of the restricted microdata may be written to public logs or replication outputs. The repository provides the estimation code and documentation required to reproduce the results after an authorized researcher has obtained and constructed the restricted analytical inputs.

## Reference

Marinho, Rogério Ribeiro, Waldir Renato Paradella, Camilo Daleles Rennó, and C. G. de Oliveira. 2012. “Aplicação de imagens SAR orbitais em desastres naturais: mapeamento das inundações de 2008 no Vale do Itajaí, SC.” *Revista Brasileira de Cartografia* 64(3): 317–330.
