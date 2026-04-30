############################################################
## Table 3 - The Effect of the Flash Floods on the Spatial
##           Distribution of Establishments
##
## Target output:
## - 4 columns
## - outcomes: Closure and Relocation
## - relocation = mover_ano_mun
## - sample = 367,402 observations
## - treatment rebuilt from the 0-12.5 km vs 50-80 km bands
## - NO forward fill of treat_B
############################################################

rm(list = ls())

source("./results/code/path_utils.R")

library(dplyr)
library(haven)
library(fixest)
library(broom)

dir.create("./results/analysis", recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# 1) Load data
# ---------------------------------------------------------

data <- haven::read_dta(data_path("Natural Disastrer Santa Catarina - Dataset.dta")) %>%
  filter(year >= 2003 & year <= 2012) %>%
  arrange(id_estab, year)

needed_vars <- c("id_estab", "year", "dist_flood", "morte", "mover_ano_mun", "code_tract")
stopifnot(all(needed_vars %in% names(data)))

# ---------------------------------------------------------
# 2) Rebuild the treatment exactly for the target table
#    Important: no forward fill of treat_B here.
# ---------------------------------------------------------

make_tract_num <- function(x) {
  x_chr <- as.character(x)
  x_chr[x_chr %in% c("", "NA")] <- NA_character_
  x_num <- suppressWarnings(as.numeric(x_chr))
  bad <- !is.na(x_chr) & is.na(x_num)

  if (sum(!is.na(x_chr)) > 0 && mean(bad) > 0.2) {
    x_num <- as.numeric(factor(x_chr))
  }

  x_num
}

data <- data %>%
  mutate(
    treat_B = case_when(
      dist_flood <= 12.5 ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE ~ NA_real_
    ),
    morte = as.numeric(haven::zap_labels(morte)),
    mover_ano_mun = as.numeric(haven::zap_labels(mover_ano_mun)),
    morte = if_else(is.na(treat_B), NA_real_, morte),
    mover_ano_mun = if_else(is.na(treat_B), NA_real_, mover_ano_mun),
    treat_B_agg = case_when(
      year >= 2008 & treat_B == 1 ~ 1,
      !is.na(treat_B)             ~ 0,
      TRUE                        ~ NA_real_
    ),
    treat_trend = if_else(year >= 2008, 1, 0),
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

data_tab3 <- data

# ---------------------------------------------------------
# 3) Estimate models
# ---------------------------------------------------------

fit_pair <- function(outcome) {
  treat_vars <- paste0("treat_B_", 2008:2012, collapse = " + ")

  list(
    no_trend_agg = feols(
      as.formula(paste0(outcome, " ~ treat_B_agg | id_estab + year")),
      data = data_tab3,
      cluster = ~ id_estab + year,
      lean = TRUE
    ),
    trend_agg = feols(
      as.formula(paste0(outcome, " ~ treat_B_agg | id_estab + year + treat_trend[code_tract_num]")),
      data = data_tab3,
      cluster = ~ id_estab + year,
      lean = TRUE
    ),
    no_trend_tv = feols(
      as.formula(paste0(outcome, " ~ ", treat_vars, " | id_estab + year")),
      data = data_tab3,
      cluster = ~ id_estab + year,
      lean = TRUE
    ),
    trend_tv = feols(
      as.formula(paste0(outcome, " ~ ", treat_vars, " | id_estab + year + treat_trend[code_tract_num]")),
      data = data_tab3,
      cluster = ~ id_estab + year,
      lean = TRUE
    )
  )
}

mods_closure <- fit_pair("morte")
mods_reloc <- fit_pair("mover_ano_mun")

mods_A <- list(
  mods_closure$no_trend_agg,
  mods_closure$trend_agg,
  mods_reloc$no_trend_agg,
  mods_reloc$trend_agg
)

mods_B <- list(
  mods_closure$no_trend_tv,
  mods_closure$trend_tv,
  mods_reloc$no_trend_tv,
  mods_reloc$trend_tv
)

# ---------------------------------------------------------
# 4) Helpers
# ---------------------------------------------------------

get_est <- function(model, term) {
  tt <- broom::tidy(model)
  out <- tt[tt$term == term, c("estimate", "std.error", "p.value")]
  if (nrow(out) == 0) stop(paste("Termo nao encontrado:", term))
  out
}

stars <- function(p) {
  ifelse(p < 0.01, "***",
         ifelse(p < 0.05, "**",
                ifelse(p < 0.1, "*", "")))
}

fmt_coef <- function(est, p) sprintf("%.5f%s", est, stars(p))
fmt_se <- function(se) sprintf("(%.5f)", se)

fmt_obs <- function(n) {
  s <- format(n, big.mark = ",", scientific = FALSE)
  gsub(",", "{,}", s, fixed = TRUE)
}

row_gap4 <- function(label, v) {
  stopifnot(length(v) == 4)
  sprintf("%s & %s &  & %s &  & %s &  & %s\\\\", label, v[1], v[2], v[3], v[4])
}

# ---------------------------------------------------------
# 5) Build LaTeX
# ---------------------------------------------------------

lines <- c(
  "\\begin{table}[htb]",
  "  \\centering",
  "  \\tabcaption{The Effect of the Flash Floods on the Spatial Distribution of Establishments}",
  "  \\label{tab3: main_results}",
  "  \\scalebox{0.75}{",
  " \\begin{threeparttable}",
  "    \\begin{tabular}{lccccccc}",
  "    \\toprule",
  "          & (1)   &       & (2)   &       & (3)   &       & (4) \\\\",
  "\\cmidrule(lr){2-2}\\cmidrule(lr){4-4}\\cmidrule(lr){6-6}\\cmidrule(lr){8-8}",
  "    \\multicolumn{8}{l}{\\textbf{Panel A: Time-Agregatted DiD}}\\\\",
  "    Dep. Var: & Closure & & Closure & & Relocation & & Relocation \\\\",
  "   \\midrule"
)

rowA <- lapply(mods_A, get_est, term = "treat_B_agg")
coefA <- sapply(rowA, function(x) fmt_coef(x$estimate, x$p.value))
seA <- sapply(rowA, function(x) fmt_se(x$std.error))

lines <- c(
  lines,
  row_gap4("    Flash Flood Post", coefA),
  row_gap4("                     ", seA)
)

obsA <- sapply(mods_A, nobs)
lines <- c(
  lines,
  row_gap4("    Observations", sapply(obsA, fmt_obs)),
  row_gap4("    Census Tract Trend", c("No", "Yes", "No", "Yes")),
  "    \\midrule",
  "    \\multicolumn{8}{l}{\\textbf{Panel B: Time-Varying DiD}}\\\\",
  "    Dep. Var: & Closure & & Closure & & Relocation & & Relocation \\\\",
  "    \\midrule"
)

for (yr in 2008:2012) {
  term <- paste0("treat_B_", yr)
  rowB <- lapply(mods_B, get_est, term = term)
  coefB <- sapply(rowB, function(x) fmt_coef(x$estimate, x$p.value))
  seB <- sapply(rowB, function(x) fmt_se(x$std.error))

  lines <- c(
    lines,
    row_gap4(sprintf("    Flash Flood %d", yr), coefB),
    row_gap4("                     ", seB)
  )
}

obsB <- sapply(mods_B, nobs)
lines <- c(
  lines,
  row_gap4("    Observations", sapply(obsB, fmt_obs)),
  row_gap4("    Census Tract Trend", c("No", "Yes", "No", "Yes")),
  "    \\bottomrule",
  "    \\end{tabular}%",
  "    \\begin{tablenotes}[flushleft]",
  paste0(
    "    \\item \\small Notes: This table shows difference-in-differences estimates for closure ",
    "(columns (1) and (2)) and relocation (columns (3) and (4)). Relocation is defined by ",
    "\\texttt{mover\\_ano\\_mun}. The treatment radius ranges from 0 to 12.5 km to flood spots ",
    "and the control ring is between 50--80 km. Establishment and year fixed effects are included ",
    "in all estimations. The two-way clustered-robust standard errors at the establishment and year ",
    "level are in parentheses. *** represents p $<$ 0.01, ** represents p $<$ 0.05, * represents p $<$ 0.1."
  ),
  "    \\end{tablenotes}",
  "    \\end{threeparttable}",
  "    }",
  "\\end{table}%"
)

writeLines(lines, "./results/analysis/Tab_03_Effect_Spatial_Distribution.tex")
