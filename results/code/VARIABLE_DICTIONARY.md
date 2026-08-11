# Variable Dictionary

Variables actually referenced by the scripts in this folder, grouped by dataset. Descriptions reflect how each variable is constructed or used in the code; unit/coding is noted where it can be determined from the code itself.

## Establishment-level data (`Natural Disastrer Santa Catarina - Dataset.dta`)

### Identifiers and raw fields

| Variable | Description |
|---|---|
| `id_estab` | Establishment identifier. |
| `year` | Calendar year of the observation. |
| `dist_flood` | Distance from the establishment to the nearest mapped flood spot, in km. Used to assign treatment/control status. |
| `code_tract` | Census tract code (raw, as read from the source file). |
| `code_tract_num` | Numeric-coerced version of `code_tract` (via `make_numeric_id()`), used for fixed effects and clustering. |
| `mun` | Municipality code as recorded in the establishment file.
| `subs_ibge` | Sector classification code (IBGE subsector). Used to build sector dummies: Construction (`== 15`), Manufacturing (`3`–`13`), Retail/Wholesale (`16`, `17`), Other Services (`21`), Transport (`20`). |
| `empregados` | Number of employees at the establishment. |
| `afil` | Establishment-level covariate used as a baseline control (Table C.3, and as `afil2` in the Cox model, Table 5).  used consistently with "number of firm branches" in the manuscript's Table C.3 note. |
| `male` | Male workforce share, used as a baseline control in Table C.3.
| `yr_abert` | Year the establishment opened. Used as a baseline covariate (`yr_abert2`) in the Cox model, Table 5. |
| `educ_d1`–`educ_d4` | Workforce-education dummy variables, used as baseline covariates in the Cox model, Table 5. |
| `massa_salarial` | Total payroll, used in Figure 2 (aggregate outcomes). |

### Constructed treatment/outcome variables (`build_establishment_panel()` in `utils_establishment.R`)

| Variable | Description |
|---|---|
| `treat_B` | 1 if the establishment falls in the treated distance band (per the script's `treated_rule`), 0 if in the control band, `NA` otherwise. Carried forward across years for establishments that later relocate outside the sampled bands (`carry_treatment_forward()`), so a once-treated establishment stays classified as treated. |
| `treat_B_agg` | Aggregated post-treatment dummy: 1 if `treat_B == 1` and `year >= 2008`, 0 if `treat_B` is non-missing and the condition doesn't hold, `NA` if `treat_B` is missing. |
| `treat_B_YYYY` (e.g. `treat_B_2008`) | Year-specific time-varying treatment dummy: equals `treat_B` in year `YYYY`, 0 in other non-missing years, `NA` otherwise. One such variable exists per year in `dummy_years`. |
| `morte` | Establishment closure indicator: 1 if the establishment is absent from RAIS in year `t+1`. |
| `closure_2yr` | Persistent-closure variant used in Table C.8: 1 only if the establishment is absent from RAIS in both `t+1` and `t+2`. |
| `reloc_tract_tminus1` | Establishment relocation indicator: 1 if the establishment's census tract in year `t+1` differs from its tract in year `t` (built via `lead(code_tract_num)`). |
| `new_firm` | New-establishment indicator, used in Figure 2 to compute the "active firms" denominator. |
| `treat_trend` / `treat_trend_f` | Post-2008 linear/factor trend indicator, interacted with `code_tract_num` to form the census-tract linear trend fixed effect used throughout. |

## Worker-level data (`workers_clean_data.rds`)

### Identifiers and raw fields

| Variable | Description |
|---|---|
| `cpf` | Worker identifier (anonymized). |
| `id_estab` | Establishment identifier the worker is linked to. |
| `year` | Calendar year of the observation. |
| `dist_flood` | Distance from the worker's linked establishment to the nearest flood spot, in km. |
| `code_tract` | Census tract code, used for clustering (`WORKER_CLUSTER_FORMULA`). |
| `codemun` | Worker's municipality code, used to detect migration (`mover_ano_mun`). |
| `ano_morte` | Year the worker's establishment closed (used to define the dismissed-worker treatment group). |
| `min_ano` | First year the worker appears in the sample. |
| `emp_31dez` | Employment status as of December 31; used to build `empregado`. |
| `grau_instr` | Worker education level, RAIS coding (1–11). Used both for the skill split (High Skill = `>= 8`, Low Skill = `< 8`) and to build `educ_d1`–`educ_d4` in the balancing table (Appendix E.1): `educ_d1` = grades 1–3, `educ_d2` = 4–5, `educ_d3` = 6–7, `educ_d4` = 8–11. |
| `genero` | Worker gender code; `male` = 1 if `genero == "1"`. |
| `idade` | Worker age. |
| `rem_med_r`, `rem_dez_r` | Average and December wage/earnings (nominal), deflated using `indice.xlsx` to produce `rendimento_medio_real` / `rendimento_real`. |
| `temp_empr` | Employment tenure. |
| `tamestab` | Employer establishment-size bracket. |
| `horas_contr` | Contracted weekly hours. |
| `tipo_sintetizado` | Synthesized worker contract-type category. Used in Appendix E.1 to build `trabalhador_fixo` (CLT/Estatutário), `trabalhador_temporario` (Trabalho Temporário), `trabalhador_outros` (Outros/Trabalho Avulso). |

### Constructed treatment/outcome variables (`output_empregados()` in `worker_functions.R`)

| Variable | Description |
|---|---|
| `treat_B` | 1 if `dist_flood` falls in `[min_treat, max_treat]`, 0 if in `[min_control, max_control]`, `NA` otherwise; filled forward/backward per worker. |
| `treat_B_agg` | Aggregated post-treatment dummy, defined relative to 2009 (not 2008 as on the establishment side): 1 if `treat_B == 1` and `year >= 2009`. |
| `treat_B_YYYY` | Year-specific time-varying treatment dummy, analogous to the establishment-side variable. |
| `empregado` | Formal-employment indicator: 1 if `emp_31dez` is non-missing. This is the outcome variable in Table 6, Table F.1, and the worker-side robustness figures ("Employment (1/0)"). |
| `mover_ano_mun` | 1 if the worker's municipality (`codemun`) changed from the prior year. Used both directly (as a robustness/migration variable) and to build `migration_geral`/`migration_interna`. |
| `rendimento_medio_real`, `rendimento_real` | Deflated average/December earnings (real terms, base period set by `indice.xlsx`). |

### Skill split (Table 6, Table F.1)

Computed from each worker's 2007 `grau_instr` value (`educ_2007` in the relevant scripts): **High Skill** = `grau_instr >= 8`, **Low Skill** = `grau_instr < 8`. The split is applied to the worker sample *before* propensity score matching — each skill subgroup gets its own independent PSM match within `output_empregados()`, rather than matching once on the combined sample and splitting afterward.

## Municipal balance data (`municipal_balance_5km_nucleo.rds`)

One row per municipality in the "nucleo" treatment/control set (12 treatment + 23 control, selected by dominant establishment count within each ring). Columns are the 24 balance variables used in Table A.1 (geographic/climatic, demographic, socioeconomic, infrastructure, and RAIS-2002 industry-composition shares) plus `codigo_municipio` (6-digit municipality code) and `grupo` (`"Tratamento"` / `"Controle"` / `"Ambos"` — the last for the one municipality present in both rings, excluded from the balance test).
