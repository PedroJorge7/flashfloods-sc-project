############################################################
## Table 6 - Hazard of Establishment Closure (Cox model)
## - Exact R port of the legacy Stata specification
## - Mirrors: stset year, failure(morte) id(id_estab)
## - Mirrors: stcox ... , strata(subs_ibge year) vce(cluster id_estab)
## - Output: ./results/analysis/Tab_06_Hazard_Establishment_Closure.tex
############################################################

rm(list = ls())

source("./results/code/path_utils.R")

library(dplyr)
library(haven)
library(survival)

dir.create("./results/analysis", recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# 1) Load and prepare the establishment panel
# ---------------------------------------------------------

data <- haven::read_dta(data_path("Natural Disastrer Santa Catarina - Dataset.dta")) %>%
  filter(year >= 2003 & year <= 2012) %>%
  arrange(id_estab, year)

data <- data %>%
  group_by(id_estab) %>%
  mutate(
    treat_B = case_when(
      dist_flood <= 12.5 ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE ~ NA_real_
    ),
    morte_orig = morte,
    new_firm_orig = new_firm,
    mover_ano_mun_orig = mover_ano_mun,
    morte = if_else(is.na(treat_B), NA_real_, as.numeric(morte)),
    new_firm = if_else(is.na(treat_B), NA_real_, as.numeric(new_firm))
  ) %>%
  mutate(
    treat_B = {
      tb <- treat_B
      mov <- mover_ano_mun

      for (i in seq_along(tb)) {
        if (i > 1 && is.na(tb[i]) && !is.na(mov[i]) && mov[i] == 1) {
          tb[i] <- tb[i - 1]
        }
      }

      tb
    },
    mover_ano_mun = if_else(is.na(treat_B), NA_real_, as.numeric(mover_ano_mun))
  ) %>%
  ungroup() %>%
  mutate(
    treat_B_agg = if_else(
      year >= 2008 & treat_B == 1,
      1,
      if_else(!is.na(treat_B), 0, NA_real_)
    ),
    international_tr = if_else(
      Exportador == 1 | Importador == 1,
      1,
      0,
      missing = 0
    )
  )

# The Stata do-file builds these controls from the first observed year.
make_first_year_value <- function(df, varname) {
  tmp <- df %>%
    group_by(id_estab) %>%
    mutate(primeiro_ano = min(year, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      temp = if_else(
        year == primeiro_ano,
        as.numeric(.data[[varname]]),
        NA_real_
      )
    ) %>%
    group_by(id_estab) %>%
    mutate(first_year_value = suppressWarnings(max(temp, na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(
      first_year_value = ifelse(is.infinite(first_year_value), NA_real_, first_year_value)
    )

  tmp$first_year_value
}

for (v in c("empregados", "afil", "yr_abert", "international_tr", "educ_d2", "educ_d3", "educ_d4")) {
  data[[paste0(v, "2")]] <- make_first_year_value(data, v)
}

# ---------------------------------------------------------
# 2) Cox sample following the Stata stset/stcox workflow
# ---------------------------------------------------------

cox_data <- data %>%
  filter(
    !is.na(treat_B_agg),
    !is.na(morte),
    !is.na(subs_ibge),
    !is.na(year)
  )

model1_data <- cox_data %>%
  filter(
    complete.cases(
      treat_B_agg,
      subs_ibge,
      year,
      morte,
      id_estab
    )
  )

model2_data <- cox_data %>%
  filter(
    complete.cases(
      treat_B_agg,
      empregados2,
      afil2,
      yr_abert2,
      educ_d22,
      educ_d32,
      educ_d42,
      subs_ibge,
      year,
      morte,
      id_estab
    )
  )

# ---------------------------------------------------------
# 3) Estimate the hazard models
# ---------------------------------------------------------

cox1 <- coxph(
  Surv(year, morte) ~
    treat_B_agg +
    strata(subs_ibge, year) +
    cluster(id_estab),
  data = model1_data,
  ties = "breslow",
  na.action = na.omit
)

cox2 <- coxph(
  Surv(year, morte) ~
    treat_B_agg +
    empregados2 +
    afil2 +
    yr_abert2 +
    educ_d22 +
    educ_d32 +
    educ_d42 +
    strata(subs_ibge, year) +
    cluster(id_estab),
  data = model2_data,
  ties = "breslow",
  na.action = na.omit
)

extract_treat <- function(model) {
  s <- summary(model)
  row <- s$coefficients["treat_B_agg", , drop = TRUE]
  coef <- as.numeric(row["coef"])
  se <- as.numeric(row["robust se"])
  p <- 2 * (1 - pnorm(abs(coef / se)))

  list(
    coef = coef,
    se = se,
    p = p,
    hr = exp(coef)
  )
}

star <- function(p) {
  if (is.na(p)) {
    ""
  } else if (p < 0.01) {
    "***"
  } else if (p < 0.05) {
    "**"
  } else if (p < 0.10) {
    "*"
  } else {
    ""
  }
}

fmt_coef <- function(x, p) sprintf("%.5f%s", x, star(p))
fmt_se <- function(se) sprintf("(%.5f)", se)
fmt_hr <- function(x, p) sprintf("%.5f%s", x, star(p))
fmt_n <- function(n) {
  gsub(",", "{,}", format(n, big.mark = ",", scientific = FALSE), fixed = TRUE)
}

summarize_sample <- function(df) {
  list(
    n_obs = nrow(df),
    n_units = dplyr::n_distinct(df$id_estab),
    n_fail = sum(df$morte == 1, na.rm = TRUE)
  )
}

r1 <- extract_treat(cox1)
r2 <- extract_treat(cox2)

s1 <- summarize_sample(model1_data)
s2 <- summarize_sample(model2_data)

# ---------------------------------------------------------
# 4) Build the LaTeX table
# ---------------------------------------------------------

build_latex <- function(res1, res2, sample1, sample2, note_text) {
  c(
    "\\begin{table}[htb!]",
    "  \\centering",
    "  \\tabcaption{Effect of Flash Floods on Hazard of Establishment Closure}",
    "  \\label{tab6: hazard_closure}",
    "  \\scalebox{0.75}{",
    " \\begin{threeparttable}",
    "    \\begin{tabular}{lcccc}",
    "    \\toprule",
    "          & \\multicolumn{2}{c}{(1)} & \\multicolumn{2}{c}{(2)} \\\\",
    "          & Coefficient & Hazard Ratio & Coefficient & Hazard Ratio \\\\",
    "    \\midrule",
    paste0(
      "    Flash Flood Post & ",
      fmt_coef(res1$coef, res1$p), " & ",
      fmt_hr(res1$hr, res1$p), " & ",
      fmt_coef(res2$coef, res2$p), " & ",
      fmt_hr(res2$hr, res2$p), " \\\\"
    ),
    paste0(
      "                      & ",
      fmt_se(res1$se), " &  & ",
      fmt_se(res2$se), " &  \\\\"
    ),
    paste0(
      "    Observations & ",
      fmt_n(sample1$n_obs), " & ",
      fmt_n(sample1$n_obs), " & ",
      fmt_n(sample2$n_obs), " & ",
      fmt_n(sample2$n_obs), " \\\\"
    ),
    paste0(
      "    Number of Units & ",
      fmt_n(sample1$n_units), " & ",
      fmt_n(sample1$n_units), " & ",
      fmt_n(sample2$n_units), " & ",
      fmt_n(sample2$n_units), " \\\\"
    ),
    paste0(
      "    Number of Failures & ",
      fmt_n(sample1$n_fail), " & ",
      fmt_n(sample1$n_fail), " & ",
      fmt_n(sample2$n_fail), " & ",
      fmt_n(sample2$n_fail), " \\\\"
    ),
    "    Establishment Controls & No & No & Yes & Yes \\\\",
    "    \\bottomrule",
    "    \\end{tabular}%",
    "    \\begin{tablenotes}[flushleft]",
    paste0("    \\item \\small ", note_text),
    "    \\end{tablenotes}",
    "    \\end{threeparttable}",
    "    }",
    "\\end{table}%"
  )
}

table_note <- paste0(
  "Notes: Exact R translation of the legacy Stata specification used for Table 6. ",
  "The survival setup follows \\texttt{stset year, failure(morte) id(id\\_estab)} and the Cox model follows ",
  "\\texttt{stcox} with sector-year strata and establishment-clustered standard errors. ",
  "Column (2) adds the baseline controls generated in the Stata do-file from each establishment's first observed year: ",
  "employment (\\texttt{empregados2}), branch count (\\texttt{afil2}), opening year (\\texttt{yr\\_abert2}), and workforce education shares ",
  "(\\texttt{educ\\_d22}, \\texttt{educ\\_d32}, \\texttt{educ\\_d42}). ",
  "Observations denote establishment-year observations used by each model, the number of units corresponds to unique establishments at risk, ",
  "and the number of failures indicates establishments that experienced closure. *** p$<$0.01, ** p$<$0.05, * p$<$0.1."
)

table_latex <- build_latex(r1, r2, s1, s2, table_note)

writeLines(table_latex, "./results/analysis/Tab_06_Hazard_Establishment_Closure.tex")
message("Saved: ./results/analysis/Tab_06_Hazard_Establishment_Closure.tex")
