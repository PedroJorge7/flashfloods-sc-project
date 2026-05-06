############################################################
## Appendix - Table B.2 (R)
## Establishment-Level Estimates Increasing the Post-Treatment Period
## - Sample window: 2003-2016
## - Trend-only specification with two columns: Closure+Trend and Relocation+Trend
## - Outcomes: morte and reloc_tract_tminus1 only
## - Relocation is defined as Census Tract, t-1 (via code_tract)
## - Two-way clustering: id_estab + year
## - Dropped terms return blank cells rather than stopping execution
## - Output:
##   ./results/analysis/Table_B2_Establishment_Level_Estimates_Increasing_Post_Treatment_Period.tex
############################################################

rm(list = ls())

source('./results/code/path_utils.R')

library(dplyr)
library(haven)
library(fixest)
library(broom)

# ---------------------------------------------------------
# 0) Set the output directory and .tex file name
# ---------------------------------------------------------
OUT_DIR <- "./results/analysis"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

OUTFILE <- file.path(
  OUT_DIR,
  "Table_B2_Establishment_Level_Estimates_Increasing_Post_Treatment_Period.tex"
)

# ---------------------------------------------------------
# 1) Load data before constructing t-1 measures
# ---------------------------------------------------------
data <- haven::read_dta(data_path("Natural Disastrer Santa Catarina - Dataset.dta")) %>%
  filter(year >= 2003) %>%
  arrange(id_estab, year)

need <- c("id_estab","year","dist_flood","morte","mover_ano_mun","code_tract")
miss <- setdiff(need, names(data))
if (length(miss) > 0) stop("Missing required columns in the dataset: ", paste(miss, collapse = ", "))

# ---------------------------------------------------------
# 2) Build treatment variables with forward carry for municipal relocations
# ---------------------------------------------------------
data <- data %>%
  group_by(id_estab) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    treat_B = case_when(
      dist_flood <= 12.5 ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE ~ NA_real_
    ),
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
# 2.1) Construct reloc_tract_tminus1 from code_tract (Census Tract, t-1)
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

# ---------------------------------------------------------
# 2.2) First-observed-year adjustment
# ---------------------------------------------------------
data <- data %>%
  group_by(id_estab) %>%
  mutate(
    reloc_tract_tminus1 = if_else(
      year == min(year, na.rm = TRUE) & reloc_tract_tminus1 == 1,
      0, reloc_tract_tminus1
    )
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 2.3) Sample-alignment rule:
#      (i) replace missing relocation values with 0
#      (ii) set relocation to NA when closure is NA
# ---------------------------------------------------------
data <- data %>%
  mutate(
    reloc_tract_tminus1 = if_else(is.na(reloc_tract_tminus1), 0, reloc_tract_tminus1),
    reloc_tract_tminus1 = if_else(is.na(morte), NA_real_, reloc_tract_tminus1)
  )

# ---------------------------------------------------------
# 3) Create treat_B_agg, year-specific dummies, and trend variables
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

for (y in 2008:2016) {
  v <- paste0("treat_B_", y)
  data[[v]] <- dplyr::case_when(
    data$year == y & !is.na(data$treat_B) ~ data$treat_B,
    !is.na(data$treat_B) & data$year != y ~ 0,
    TRUE                                  ~ NA_real_
  )
}

# ---------------------------------------------------------
# 4) Restrict 2003-2016
# ---------------------------------------------------------
data_tab <- data %>% filter(year >= 2003 & year <= 2016)

# ---------------------------------------------------------
# 5) Estimate the two trend-only models
# ---------------------------------------------------------
treat_vars_B <- paste0("treat_B_", 2008:2016, collapse = " + ")

# Panel A (trend only)
mA_cl_tr <- feols(
  morte ~ treat_B_agg | id_estab + year + treat_trend[code_tract_num],
  data = data_tab, cluster = ~ id_estab + year, fixef.rm = "none", lean = TRUE
)
mA_rl_tr <- feols(
  reloc_tract_tminus1 ~ treat_B_agg | id_estab + year + treat_trend[code_tract_num],
  data = data_tab, cluster = ~ id_estab + year, fixef.rm = "none", lean = TRUE
)

# Panel B (trend only)
mB_cl_tr <- feols(
  as.formula(paste0("morte ~ ", treat_vars_B, " | id_estab + year + treat_trend[code_tract_num]")),
  data = data_tab, cluster = ~ id_estab + year, fixef.rm = "none", lean = TRUE
)
mB_rl_tr <- feols(
  as.formula(paste0("reloc_tract_tminus1 ~ ", treat_vars_B, " | id_estab + year + treat_trend[code_tract_num]")),
  data = data_tab, cluster = ~ id_estab + year, fixef.rm = "none", lean = TRUE
)

mods_A <- list(mA_cl_tr, mA_rl_tr)
mods_B <- list(mB_cl_tr, mB_rl_tr)

# ---------------------------------------------------------
# 6) Helper functions for dropped terms
# ---------------------------------------------------------
stars <- function(p) {
  if (is.na(p)) ""
  else if (p < 0.01) "***"
  else if (p < 0.05) "**"
  else if (p < 0.1)  "*"
  else ""
}
fmt_coef <- function(est, p) if (is.na(est)) "" else sprintf("%.5f%s", est, stars(p))
fmt_se   <- function(se) if (is.na(se)) "" else sprintf("(%.5f)", se)
fmt_obs  <- function(n) gsub(",", "{,}", format(n, big.mark = ",", scientific = FALSE), fixed = TRUE)

get_est_safe <- function(model, term) {
  tt <- broom::tidy(model)
  out <- tt[tt$term == term, c("estimate","std.error","p.value")]
  if (nrow(out) == 0) return(data.frame(estimate=NA_real_, std.error=NA_real_, p.value=NA_real_))
  out[1, , drop = FALSE]
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
  "\\begin{supptable}[H]",
  "  \\centering",
  "  \\tabcaption{Establishment-Level Estimates Increasing the Post-Treatment Period (Trend Only)}",
  "  \\label{rob5:est}",
  "  \\scalebox{0.75}{",
  " \\begin{threeparttable}",
  "    \\begin{tabular}{lccc}",
  "    \\toprule",
  "          & (1)   &       & (2) \\\\",
  "\\cmidrule(lr){2-2}\\cmidrule(lr){4-4}",
  "    \\multicolumn{4}{l}{\\textbf{Panel A: Time-Agregatted DiD}}\\\\",
  "    Dep. Var: & Closure & & Relocation \\\\",
  "    \\midrule"
)

# Panel A: treat_B_agg
rowA  <- lapply(mods_A, get_est_safe, term = "treat_B_agg")
coefA <- sapply(rowA, \(x) fmt_coef(x$estimate, x$p.value))
seA   <- sapply(rowA, \(x) fmt_se(x$std.error))

lines <- c(
  lines,
  row_gap2("    Flash Flood Post", coefA),
  row_gap2("                     ", seA),
  row_gap2("    Observations", sapply(sapply(mods_A, nobs), fmt_obs)),
  row_gap2("    Census Tract Trend", c("Yes","Yes")),
  "    \\midrule",
  "    \\multicolumn{4}{l}{\\textbf{Panel B: Time-Varying DiD}}\\\\",
  "    Dep. Var: & Closure & & Relocation \\\\",
  "    \\midrule"
)

# Panel B: 2008-2016 (SAFE)
for (yr in 2008:2016) {
  term <- paste0("treat_B_", yr)
  rowB  <- lapply(mods_B, get_est_safe, term = term)
  coefB <- sapply(rowB, \(x) fmt_coef(x$estimate, x$p.value))
  seB   <- sapply(rowB, \(x) fmt_se(x$std.error))
  lines <- c(
    lines,
    row_gap2(sprintf("    Flash Flood %d", yr), coefB),
    row_gap2("                     ", seB)
  )
}

lines <- c(
  lines,
  row_gap2("    Observations", sapply(sapply(mods_B, nobs), fmt_obs)),
  row_gap2("    Census Tract Trend", c("Yes","Yes")),
  "    \\bottomrule",
  "    \\end{tabular}%",
  "   \t\\begin{tablenotes}[flushleft] \\item \\small \\textit{Notes:} This table reports specifications with the post-treatment period extended through 2016 and includes census tract trends via varying slopes (treat\\_trend[code\\_tract\\_num]). Closure is measured by \\textit{morte}. Relocation is defined as \\textit{Census Tract, t-1}: $reloc\\_tract\\_tminus1(t)=1$ if the establishment changes census tract between $t$ and $t+1$ (constructed from \\texttt{code\\_tract} using a lead of the tract-change indicator), with the first observed year forced to have $reloc\\_tract\\_tminus1=0$ when it would otherwise be 1. Panel A uses a single post-treatment dummy (treat\\_B\\_agg); Panel B uses time-varying treatment dummies (2008--2016). The treatment radius is 0--12.5 km and the control ring is 50--80 km. Establishment and year fixed effects are included in all estimations. Two-way clustered standard errors (establishment and year) are shown in parentheses. *** p$<$0.01, ** p$<$0.05, * p$<$0.1.",
  "   \t\\end{tablenotes}",
  "   \t\\end{threeparttable}",
  "   \t}",
  "\\end{supptable}%"
)

writeLines(lines, OUTFILE)
message("Saved: ", OUTFILE)
