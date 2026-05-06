############################################################
## Table 5 - The Effect of the Flash Floods on Establishment
##           Closure by Business Size
##
## Exact port of the Stata heterogeneity block:
## - same establishment sample construction
## - same size bins based on empregados
## - one-way clustered SEs for Panel A
## - two-way clustered SEs for Panel B
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
    mover_ano_mun = as.numeric(haven::zap_labels(mover_ano_mun)),
    new_firm = as.numeric(haven::zap_labels(new_firm))
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
    new_firm_orig = new_firm,
    mover_ano_mun_orig = mover_ano_mun
  ) %>%
  mutate(
    morte = if_else(is.na(treat_B), NA_real_, morte_orig),
    new_firm = if_else(is.na(treat_B), NA_real_, new_firm_orig),
    treat_B = carry_treatment_forward(treat_B, mover_ano_mun_orig),
    mover_ano_mun = if_else(is.na(treat_B), NA_real_, mover_ano_mun_orig)
  ) %>%
  ungroup() %>%
  filter(year >= 2003 & year <= 2012) %>%
  mutate(
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

data <- data %>%
  mutate(
    size_estab1 = case_when(empregados < 10 ~ 1, TRUE ~ NA_real_),
    size_estab2 = case_when(dplyr::between(empregados, 10, 50) ~ 1, TRUE ~ NA_real_),
    size_estab3 = case_when(empregados > 50 ~ 1, TRUE ~ NA_real_)
  )

size_vars <- c("size_estab1", "size_estab2", "size_estab3")
size_labels <- c("Micro Business", "Small Business", "Medium \\& Large Business")
treat_vars <- paste0("treat_B_", 2008:2012, collapse = " + ")

fit_size_models <- function(size_var) {
  size_data <- data %>% filter(.data[[size_var]] == 1)

  list(
    agg = feols(
      morte ~ treat_B_agg | id_estab + year + treat_trend[code_tract_num],
      data = size_data,
      cluster = ~ id_estab,
      fixef.rm = "none",
      lean = TRUE
    ),
    tv = feols(
      as.formula(paste0("morte ~ ", treat_vars, " | id_estab + year + treat_trend[code_tract_num]")),
      data = size_data,
      cluster = ~ id_estab + year,
      fixef.rm = "none",
      lean = TRUE
    )
  )
}

size_models <- lapply(size_vars, fit_size_models)

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

row_gap3 <- function(label, v) {
  stopifnot(length(v) == 3)
  sprintf("%s & %s &  & %s &  & %s\\\\", label, v[1], v[2], v[3])
}

panelA_rows <- lapply(size_models, function(x) get_est(x$agg, "treat_B_agg"))
panelA_coef <- sapply(panelA_rows, function(x) fmt_coef(x$estimate, x$p.value))
panelA_se <- sapply(panelA_rows, function(x) fmt_se(x$std.error))
panelA_n <- sapply(size_models, function(x) fmt_obs(nobs(x$agg)))

lines <- c(
  "\\begin{table}[htb!]",
  "  \\centering",
  "  \\tabcaption{The Effect of the Flash Floods on Establishment Closure by Business Size}",
  "  \\label{tab5:hetero_size}",
  "  \\scalebox{0.78}{",
  "  \\begin{threeparttable}",
  "    \\begin{tabular}{lccccc}",
  "    \\toprule",
  "          & (1)   &       & (2)   &       & (3) \\\\",
  "\\cmidrule(lr){2-2}\\cmidrule(lr){4-4}\\cmidrule(lr){6-6}",
  "    \\multicolumn{6}{l}{\\textbf{Panel A: Time-Aggregated DiD}}\\\\",
  paste0("    Est. Size: & ", size_labels[1], " &       & ", size_labels[2], " &       & ", size_labels[3], " \\\\"),
  "    \\midrule",
  row_gap3("    Flash Flood Post", panelA_coef),
  row_gap3("                     ", panelA_se),
  row_gap3("    Observations", panelA_n),
  "    \\midrule",
  "    \\multicolumn{6}{l}{\\textbf{Panel B: Time-Varying DiD}}\\\\",
  paste0("    Est. Size: & ", size_labels[1], " &       & ", size_labels[2], " &       & ", size_labels[3], " \\\\"),
  "    \\midrule"
)

for (yr in 2008:2012) {
  term <- paste0("treat_B_", yr)
  rows <- lapply(size_models, function(x) get_est(x$tv, term))
  coef_vals <- sapply(rows, function(x) fmt_coef(x$estimate, x$p.value))
  se_vals <- sapply(rows, function(x) fmt_se(x$std.error))

  lines <- c(
    lines,
    row_gap3(sprintf("    Flash Flood %d", yr), coef_vals),
    row_gap3("                     ", se_vals)
  )
}

lines <- c(
  lines,
  row_gap3("    Observations", sapply(size_models, function(x) fmt_obs(nobs(x$tv)))),
  "    \\bottomrule",
  "    \\end{tabular}%",
  paste0(
    "   \\begin{tablenotes}[flushleft] \\item \\small Notes: This table follows the original Stata heterogeneity specification. ",
    "The three columns split establishments by employment size: fewer than 10 employees, 10--50 employees, and more than 50 employees. ",
    "All specifications include establishment and year fixed effects plus the legacy census-trend term ",
    "(\\texttt{treat\\_trend[code\\_tract\\_num]}). Panel A uses one-way clustered standard errors at the establishment level; ",
    "Panel B uses two-way clustered standard errors at the establishment and year levels. *** p $<$ 0.01, ** p $<$ 0.05, * p $<$ 0.1. ",
    "\\end{tablenotes}"
  ),
  "   \\end{threeparttable}",
  "   }",
  "\\end{table}%"
)

writeLines(lines, "./results/analysis/Tab_05_Effect_by_Business_Size.tex")
