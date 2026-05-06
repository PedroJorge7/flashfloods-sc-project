############################################################
## Table 3 - The Effect of the Flash Floods on the Spatial
##           Distribution of Establishments
##
## Port of the baseline Stata workflow in
## script_sc_final_review.do:
## - same treatment bands (0-12.5 km vs 50-80 km)
## - same censoring / carry-forward rule for closure and municipal relocation
## - Table 3 output restricted to closure and relocation only
## - one-way clustered SEs for the aggregated DiD
## - two-way clustered SEs for the time-varying DiD
############################################################

rm(list = ls())

source("./results/code/path_utils.R")

library(broom)
library(dplyr)
library(fixest)
library(haven)

dir.create("./results/analysis", recursive = TRUE, showWarnings = FALSE)

make_numeric_id <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }

  x_unl <- haven::zap_labels(x)
  x_chr <- as.character(x_unl)
  x_chr[x_chr %in% c("", "NA")] <- NA_character_

  x_num <- suppressWarnings(as.numeric(x_chr))
  bad <- !is.na(x_chr) & is.na(x_num)

  if (sum(!is.na(x_chr)) > 0 && mean(bad) > 0.2) {
    x_num <- as.numeric(factor(x_chr))
  }

  x_num
}

carry_treatment_forward <- function(treat, mover) {
  out <- treat

  for (i in seq_along(out)) {
    if (
      i > 1 &&
      is.na(out[i]) &&
      !is.na(mover[i]) &&
      mover[i] == 1
    ) {
      out[i] <- out[i - 1]
    }
  }

  out
}

data <- haven::read_dta(data_path("Natural Disastrer Santa Catarina - Dataset.dta")) %>%
  mutate(
    morte = as.numeric(haven::zap_labels(morte)),
    mover_ano_mun = as.numeric(haven::zap_labels(mover_ano_mun))
  ) %>%
  arrange(id_estab, year) %>%
  group_by(id_estab) %>%
  mutate(
    treat_B = case_when(
      dist_flood <= 12.5 ~ 1,
      dplyr::between(dist_flood, 50, 80) ~ 0,
      TRUE ~ NA_real_
    ),
    morte_orig = morte,
    mover_ano_mun_orig = mover_ano_mun
  ) %>%
  mutate(
    morte = if_else(is.na(treat_B), NA_real_, morte_orig),
    treat_B = carry_treatment_forward(treat_B, mover_ano_mun_orig),
    mover_ano_mun = if_else(is.na(treat_B), NA_real_, mover_ano_mun_orig)
  ) %>%
  ungroup() %>%
  filter(year >= 2003 & year <= 2012) %>%
  mutate(
    # Keep relocation on the same estimation sample as closure.
    mover_ano_mun = if_else(is.na(mover_ano_mun), 0, mover_ano_mun),
    mover_ano_mun = if_else(is.na(morte), NA_real_, mover_ano_mun),
    treat_B_agg = case_when(
      year >= 2008 & treat_B == 1 ~ 1,
      !is.na(treat_B) ~ 0,
      TRUE ~ NA_real_
    ),
    treat_trend = if_else(year >= 2008, 1, 0),
    code_tract_num = make_numeric_id(code_tract)
  )

for (y in 2002:2016) {
  v <- paste0("treat_B_", y)
  data[[v]] <- dplyr::case_when(
    data$year == y & !is.na(data$treat_B) ~ data$treat_B,
    !is.na(data$treat_B) & data$year != y ~ 0,
    TRUE ~ NA_real_
  )
}

treat_vars <- paste0("treat_B_", 2008:2012, collapse = " + ")

fit_pair <- function(outcome) {
  list(
    agg_no_trend = feols(
      as.formula(paste0(outcome, " ~ treat_B_agg | id_estab + year")),
      data = data,
      cluster = ~ id_estab,
      fixef.rm = "none",
      lean = TRUE
    ),
    agg_trend = feols(
      as.formula(paste0(outcome, " ~ treat_B_agg | id_estab + year + treat_trend[code_tract_num]")),
      data = data,
      cluster = ~ id_estab,
      fixef.rm = "none",
      lean = TRUE
    ),
    tv_no_trend = feols(
      as.formula(paste0(outcome, " ~ ", treat_vars, " | id_estab + year")),
      data = data,
      cluster = ~ id_estab + year,
      fixef.rm = "none",
      lean = TRUE
    ),
    tv_trend = feols(
      as.formula(paste0(outcome, " ~ ", treat_vars, " | id_estab + year + treat_trend[code_tract_num]")),
      data = data,
      cluster = ~ id_estab + year,
      fixef.rm = "none",
      lean = TRUE
    )
  )
}

mods_closure <- fit_pair("morte")
mods_reloc <- fit_pair("mover_ano_mun")

mods_A <- list(
  mods_closure$agg_no_trend,
  mods_closure$agg_trend,
  mods_reloc$agg_no_trend,
  mods_reloc$agg_trend
)

mods_B <- list(
  mods_closure$tv_no_trend,
  mods_closure$tv_trend,
  mods_reloc$tv_no_trend,
  mods_reloc$tv_trend
)

get_est <- function(model, term) {
  tt <- broom::tidy(model)
  out <- tt[tt$term == term, c("estimate", "std.error", "p.value")]

  if (nrow(out) == 0) {
    stop(paste("Term not found:", term))
  }

  out
}

stars <- function(p) {
  ifelse(p < 0.01, "***",
         ifelse(p < 0.05, "**",
                ifelse(p < 0.1, "*", "")))
}

fmt_coef <- function(est, p) sprintf("%.5f%s", est, stars(p))
fmt_se <- function(se) sprintf("(%.5f)", se)
fmt_obs <- function(n) gsub(",", "{,}", format(n, big.mark = ",", scientific = FALSE), fixed = TRUE)

row_gap4 <- function(label, v) {
  stopifnot(length(v) == 4)
  sprintf("%s & %s &  & %s &  & %s &  & %s\\\\",
          label, v[1], v[2], v[3], v[4])
}

lines <- c(
  "\\begin{table}[htb]",
  "  \\centering",
  "  \\tabcaption{The Effect of the Flash Floods on the Spatial Distribution of Establishments}",
  "  \\label{tab3:main_results}",
  "  \\scalebox{0.80}{",
  "  \\begin{threeparttable}",
  "    \\begin{tabular}{lccccccc}",
  "    \\toprule",
  "          & (1)   &       & (2)   &       & (3)   &       & (4) \\\\",
  "\\cmidrule(lr){2-2}\\cmidrule(lr){4-4}\\cmidrule(lr){6-6}\\cmidrule(lr){8-8}",
  "    \\multicolumn{8}{l}{\\textbf{Panel A: Time-Aggregated DiD}}\\\\",
  "    Dep. Var: & Closure & & Closure & & Relocation & & Relocation \\\\",
  "    \\midrule"
)

rowA <- lapply(mods_A, get_est, term = "treat_B_agg")
coefA <- sapply(rowA, function(x) fmt_coef(x$estimate, x$p.value))
seA <- sapply(rowA, function(x) fmt_se(x$std.error))

lines <- c(
  lines,
  row_gap4("    Flash Flood Post", coefA),
  row_gap4("                     ", seA),
  row_gap4("    Observations", sapply(sapply(mods_A, nobs), fmt_obs)),
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

lines <- c(
  lines,
  "    \\midrule",
  row_gap4("    Observations", sapply(sapply(mods_B, nobs), fmt_obs)),
  row_gap4("    Census Tract Trend", c("No", "Yes", "No", "Yes")),
  "    \\bottomrule",
  "    \\end{tabular}%",
  "    \\begin{tablenotes}[flushleft]",
  paste0(
    "    \\item \\small Notes: This table follows the baseline Stata specification in ",
    "\\texttt{script\\_sc\\_final\\_review.do}. Columns (1) and (2) use establishment closure, ",
    "and columns (3) and (4) use municipal relocation (\\texttt{mover\\_ano\\_mun}). ",
    "Establishment and year fixed effects are included in all models. ",
    "Panel A uses one-way clustered standard errors at the establishment level, matching the Stata workflow. ",
    "Panel B uses two-way clustered standard errors at the establishment and year levels. ",
    "The treatment radius is 0--12.5 km and the control ring is 50--80 km. *** p $<$ 0.01, ** p $<$ 0.05, * p $<$ 0.1."
  ),
  "    \\end{tablenotes}",
  "  \\end{threeparttable}",
  "  }",
  "\\end{table}%"
)

writeLines(lines, "./results/analysis/Tab_03_Effect_Spatial_Distribution.tex")
