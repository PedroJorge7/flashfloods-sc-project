############################################################
## Table B.6 - Establishment-Level Estimates Dropping Units Affected by the 2011 Floods
## Robustness: Remove municipalities that declared public calamity in 2011 (ECP 2011)
## SAME STRATEGY as main result (Table 3), BUT TREND-ONLY:
##   - Outcomes: Closure (morte) and Relocation (reloc_tract_tminus1)
##   - 2 columns (TREND ONLY): (1) Closure + Census tract trend,
##                             (2) Reloc (Tract,t-1) + trend
##   - Panel A: Time-aggregated DiD (treat_B_agg)
##   - Panel B: Time-varying DiD (treat_B_2008 ... treat_B_2012)
##   - FE: id_estab + year
##   - Cluster: id_estab + year (two-way)
##   - Keep singletons: fixef.rm = "none"
## Output: ./results/analysis/Tab_B6_Removing_ECP_2011.tex
############################################################

rm(list = ls())

source('./results/code/path_utils.R')

library(dplyr)
library(haven)
library(fixest)
library(broom)

dir.create("./results/analysis", recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# 1) Load data (do NOT cut too early; keep construction identical)
# ---------------------------------------------------------
data <- haven::read_dta(data_path("Natural Disastrer Santa Catarina - Dataset.dta")) %>%
  filter(year >= 2003) %>%
  arrange(id_estab, year)

need_vars <- c("id_estab","year","dist_flood","morte","mover_ano_mun","code_tract","mun")
stopifnot(all(need_vars %in% names(data)))

# ---------------------------------------------------------
# 1.1) ECP 2011 indicator (as in Stata)
# ---------------------------------------------------------
as_num_safe <- function(x) {
  if (inherits(x, "haven_labelled")) x <- haven::zap_labels(x)
  if (is.numeric(x)) return(as.numeric(x))
  suppressWarnings(as.numeric(as.character(x)))
}

ecp_muns_2011 <- c(
  420030, 420190, 420290, 420850, 420950, 420990,
  421400, 421460, 421480, 421780, 421860
)

data <- data %>%
  mutate(
    mun_num      = as_num_safe(mun),
    ecp_2011     = if_else(!is.na(mun_num) & mun_num %in% ecp_muns_2011, 1, 0),
    ecp_2011_mun = if_else(ecp_2011 == 1, mun_num, 0)
  )

# ---------------------------------------------------------
# 2) Replicate Stata treatment construction (same as main)
# ---------------------------------------------------------
data <- data %>%
  group_by(id_estab) %>%
  mutate(
    treat_B = case_when(
      dist_flood <= 12.5 ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE ~ NA_real_
    ),
    morte_orig         = morte,
    mover_ano_mun_orig = mover_ano_mun
  ) %>%
  mutate(
    morte         = as.numeric(haven::zap_labels(morte)),
    mover_ano_mun = as.numeric(haven::zap_labels(mover_ano_mun)),
    morte = if_else(is.na(treat_B), NA_real_, morte)
  ) %>%
  mutate(
    treat_B = {
      tb  <- treat_B
      mov <- mover_ano_mun
      for (i in seq_along(tb)) {
        if (i > 1 && is.na(tb[i]) && !is.na(mov[i]) && mov[i] == 1) tb[i] <- tb[i - 1]
      }
      tb
    }
  ) %>%
  mutate(
    mover_ano_mun = if_else(is.na(treat_B), NA_real_, mover_ano_mun)
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 2.1) Relocation = Census Tract, t-1 (IGUAL Table 3)
# ---------------------------------------------------------
data <- data %>%
  group_by(id_estab) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    ct     = as.character(code_tract),
    ct_lag = lag(ct, 1),
    diff_tract = case_when(
      is.na(ct) | is.na(ct_lag) ~ NA_real_,
      ct != ct_lag              ~ 1,
      TRUE                      ~ 0
    ),
    reloc_tract_tminus1 = lead(diff_tract, 1)
  ) %>%
  ungroup() %>%
  mutate(
    reloc_tract_tminus1 = if_else(is.na(treat_B), NA_real_, reloc_tract_tminus1),
    reloc_tract_tminus1 = as.numeric(reloc_tract_tminus1)
  ) %>%
  select(-ct, -ct_lag, -diff_tract)

# 1Âº ano observado: se reloc==1 -> vira 0 (IGUAL Table 3)
data <- data %>%
  group_by(id_estab) %>%
  mutate(
    reloc_tract_tminus1 = if_else(
      year == min(year, na.rm = TRUE) & reloc_tract_tminus1 == 1,
      0, reloc_tract_tminus1
    )
  ) %>%
  ungroup()

# if Closure is NA, Relocation must be NA (IGUAL Table 3)
data <- data %>%
  mutate(
    reloc_tract_tminus1 = if_else(is.na(reloc_tract_tminus1), 0, reloc_tract_tminus1),
    reloc_tract_tminus1 = if_else(is.na(morte), NA_real_, reloc_tract_tminus1)
  )

# ---------------------------------------------------------
# 3) Create aggregated post + annual dummies + "trend control"
# ---------------------------------------------------------
make_tract_num <- function(x) {
  x_chr <- as.character(x)
  x_chr[x_chr %in% c("", "NA")] <- NA_character_
  x_num <- suppressWarnings(as.numeric(x_chr))
  bad   <- !is.na(x_chr) & is.na(x_num)
  if (sum(!is.na(x_chr)) > 0 && mean(bad) > 0.2) x_num <- as.numeric(factor(x_chr))
  x_num
}

data <- data %>%
  mutate(
    treat_B_agg = case_when(
      year >= 2008 & treat_B == 1 ~ 1,
      !is.na(treat_B)             ~ 0,
      TRUE                        ~ NA_real_
    ),
    treat_trend    = if_else(year >= 2008, 1, 0),
    code_tract_num = make_tract_num(code_tract)
  )

for (y in 2008:2012) {
  v <- paste0("treat_B_", y)
  data[[v]] <- dplyr::case_when(
    data$year == y & !is.na(data$treat_B) ~ data$treat_B,
    !is.na(data$treat_B) & data$year != y ~ 0,
    TRUE                                  ~ NA_real_
  )
}

# ---------------------------------------------------------
# 4) Restrict to 2003â€“2012 + remove ECP 2011
# ---------------------------------------------------------
data_tab <- data %>% filter(year >= 2003 & year <= 2012)
data_tab_ecp <- data_tab %>% filter(ecp_2011 == 0)

# ---------------------------------------------------------
# 5) Estimate models (TREND ONLY)
# ---------------------------------------------------------
outcomes <- c("morte", "reloc_tract_tminus1")

panelA_tr <- list()
panelB_tr <- list()

for (outcome in outcomes) {
  
  # Panel A (trend only)
  fml_A2 <- as.formula(paste0(outcome, " ~ treat_B_agg | id_estab + year + treat_trend[code_tract_num]"))
  panelA_tr[[outcome]] <- feols(
    fml_A2, data = data_tab_ecp,
    cluster = ~ id_estab + year,
    fixef.rm = "none", lean = TRUE
  )
  
  # Panel B (trend only)
  treat_vars <- paste0("treat_B_", 2008:2012, collapse = " + ")
  fml_B2 <- as.formula(paste0(outcome, " ~ ", treat_vars, " | id_estab + year + treat_trend[code_tract_num]"))
  panelB_tr[[outcome]] <- feols(
    fml_B2, data = data_tab_ecp,
    cluster = ~ id_estab + year,
    fixef.rm = "none", lean = TRUE
  )
}

# Column order (trend-only): (1) Closure+Trend, (2) Reloc+Trend
mods_A <- list(panelA_tr[["morte"]], panelA_tr[["reloc_tract_tminus1"]])
mods_B <- list(panelB_tr[["morte"]], panelB_tr[["reloc_tract_tminus1"]])

# ---------------------------------------------------------
# 6) Helpers
# ---------------------------------------------------------
get_est <- function(model, term) {
  tt <- broom::tidy(model)
  out <- tt[tt$term == term, c("estimate", "std.error", "p.value")]
  if (nrow(out) == 0) stop(paste("Termo nÃ£o encontrado:", term))
  out[1, , drop = FALSE]
}

stars <- function(p) {
  if (is.na(p)) ""
  else if (p < 0.01) "***"
  else if (p < 0.05) "**"
  else if (p < 0.1)  "*"
  else ""
}

fmt_coef <- function(est, p) sprintf("%.5f%s", est, stars(p))
fmt_se   <- function(se) sprintf("(%.5f)", se)

fmt_obs <- function(n) {
  s <- format(n, big.mark = ",", scientific = FALSE)
  gsub(",", "{,}", s, fixed = TRUE)
}

pad <- "       "
row_gap2 <- function(label, v) {
  stopifnot(length(v) == 2)
  paste0(label, " & ", v[1], " &", pad, "& ", v[2], "\\\\")
}

# ---------------------------------------------------------
# 7) Build LaTeX (TREND ONLY)
# ---------------------------------------------------------
lines <- c(
  "\\begin{table}[htb]",
  "  \\centering",
  "  \\tabcaption{Firm Estimations (Excluding ECP 2011 Municipalities) -- Trend Only}",
  "  \\label{tab: b6_removing_ecp_2011}",
  "  \\scalebox{0.80}{",
  " \\begin{threeparttable}",
  "    \\begin{tabular}{lccc}",
  "    \\toprule",
  "          & (1)   &       & (2) \\\\",
  "\\cmidrule(lr){2-2}\\cmidrule(lr){4-4}",
  "    \\multicolumn{4}{l}{\\textbf{Panel A: Time-Agregatted DiD}}\\\\",
  "    Dep. Var: & Closure & & Relocation (Tract, t-1) \\\\",
  "   \\midrule"
)

# Panel A: treat_B_agg
rowA  <- lapply(mods_A, get_est, term = "treat_B_agg")
coefA <- sapply(rowA, function(x) fmt_coef(x$estimate, x$p.value))
seA   <- sapply(rowA, function(x) fmt_se(x$std.error))

lines <- c(lines,
           row_gap2("    Flash Flood Post", coefA),
           row_gap2("                     ", seA),
           row_gap2("    Observations", sapply(sapply(mods_A, nobs), fmt_obs)),
           row_gap2("    Census Tract Trend", c("Yes","Yes")),
           "    \\midrule",
           "    \\multicolumn{4}{l}{\\textbf{Panel B: Time-Varying DiD}}\\\\",
           "    Dep. Var: & Closure & & Relocation \\\\",
           "    \\midrule"
)

# Panel B: 2008â€“2012
years_evt <- 2008:2012
terms_evt <- paste0("treat_B_", years_evt)

for (k in seq_along(years_evt)) {
  yr   <- years_evt[k]
  term <- terms_evt[k]
  
  rowB  <- lapply(mods_B, get_est, term = term)
  coefB <- sapply(rowB, function(x) fmt_coef(x$estimate, x$p.value))
  seB   <- sapply(rowB, function(x) fmt_se(x$std.error))
  
  lines <- c(lines,
             row_gap2(sprintf("    Flash Flood %d", yr), coefB),
             row_gap2("                     ", seB))
}

lines <- c(
  lines,
  row_gap2("    Observations", sapply(sapply(mods_B, nobs), fmt_obs)),
  row_gap2("    Census Tract Trend", c("Yes","Yes")),
  "    \\bottomrule",
  "    \\end{tabular}%",
  "   \t\\begin{tablenotes}[flushleft] \\item \\small Notes: This table replicates the main firm-level DiD strategy excluding establishments located in municipalities that declared public calamity in 2011 (ECP 2011). It reports only specifications including census tract trends via varying slopes (treat\\_trend[code\\_tract\\_num]). Outcomes are closure (\\texttt{morte}, column (1)) and relocation defined as \\textit{Census Tract, t-1} (\\texttt{reloc\\_tract\\_tminus1}, column (2)). Panel A uses the time-aggregated post-treatment indicator (\\texttt{treat\\_B\\_agg}); Panel B uses time-varying treatment dummies (\\texttt{treat\\_B\\_2008}--\\texttt{treat\\_B\\_2012}). Treatment radius is 0--12.5 km and control ring is 50--80 km. Establishment and year fixed effects are included in all estimations. Two-way clustered standard errors (establishment and year) are in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.",
  "   \t\\end{tablenotes}",
  "   \t\\end{threeparttable}",
  "   \t}",
  "\\end{table}%"
)

writeLines(lines, "./results/analysis/Tab_B6_Removing_ECP_2011.tex")
