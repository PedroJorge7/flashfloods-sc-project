# ============================================================================
# Table A.1 - Definition of Treatment Radius:
#             Effects of Flooding on Relocation
# - Uses the same treatment-band exercise as Table 2
# - Keeps the 50-80 km control ring fixed
# - Uses municipal relocation (mover_ano_mun) as the outcome
# - Carries treatment status forward when establishments relocate
# - Uses two-way clustered standard errors and keeps FE singletons
# ============================================================================

rm(list = ls())

source("./results/code/path_utils.R")

library(dplyr)
library(haven)
library(fixest)
library(broom)

OUT_DIR <- "./results/analysis"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
OUTFILE <- file.path(OUT_DIR, "Tab_A1_Definition_Treatment_Radius_Relocation.tex")

data <- haven::read_dta(data_path("Natural Disastrer Santa Catarina - Dataset.dta")) %>%
  filter(year >= 2003 & year <= 2012) %>%
  arrange(id_estab, year) %>%
  mutate(
    mover_ano_mun_orig = mover_ano_mun
  )

stopifnot(all(c("id_estab", "year", "dist_flood", "mover_ano_mun") %in% names(data)))

bands <- list(
  "0-2.5 km"   = c(NA,   2.5),
  "2.5-5 km"   = c(2.5,  5),
  "5-7.5 km"   = c(5,    7.5),
  "7.5-10 km"  = c(7.5, 10),
  "10-12.5 km" = c(10,  12.5),
  "12.5-15 km" = c(12.5, 15),
  "15-17.5 km" = c(15,  17.5)
)

treat_years <- 2008:2012
models <- list()

for (idx in seq_along(bands)) {
  lower_b <- bands[[idx]][1]
  upper_b <- bands[[idx]][2]

  temp <- data %>%
    arrange(id_estab, year) %>%
    group_by(id_estab) %>%
    mutate(
      treat_B_temp = case_when(
        is.na(lower_b)  & dist_flood <= upper_b                        ~ 1,
        !is.na(lower_b) & dist_flood > lower_b & dist_flood <= upper_b ~ 1,
        dist_flood >= 50 & dist_flood <= 80                            ~ 0,
        TRUE                                                           ~ NA_real_
      ),
      mover_ano_mun_temp = mover_ano_mun_orig
    ) %>%
    mutate(
      treat_B_temp = {
        tb  <- treat_B_temp
        mov <- mover_ano_mun_temp
        for (i in seq_along(tb)) {
          if (i > 1 && is.na(tb[i]) && !is.na(mov[i]) && mov[i] == 1) {
            tb[i] <- tb[i - 1]
          }
        }
        tb
      }
    ) %>%
    mutate(
      mover_ano_mun_temp = if_else(
        is.na(treat_B_temp),
        NA_real_,
        as.numeric(haven::zap_labels(mover_ano_mun_temp))
      )
    ) %>%
    ungroup()

  for (y in 2003:2012) {
    v <- paste0("treat_B_temp_", y)
    temp[[v]] <- dplyr::case_when(
      temp$year == y & !is.na(temp$treat_B_temp) ~ temp$treat_B_temp,
      !is.na(temp$treat_B_temp) & temp$year != y ~ 0,
      TRUE                                       ~ NA_real_
    )
  }

  rhs <- paste0("treat_B_temp_", treat_years, collapse = " + ")
  fml <- as.formula(paste0("mover_ano_mun_temp ~ ", rhs, " | id_estab + year"))

  models[[idx]] <- feols(
    fml,
    data     = temp,
    cluster  = ~ id_estab + year,
    fixef.rm = "none",
    lean     = TRUE
  )
}

star_fun <- function(p) {
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

make_row <- function(label, vals) {
  inter <- as.vector(rbind(vals, rep("", length(vals))))
  paste0(label, " & ", paste(inter, collapse = " & "), " \\\\")
}

row_labs <- paste("Flash Flood", treat_years)
coef_mat <- matrix("", nrow = length(row_labs) * 2, ncol = length(models))
obs_vec <- numeric(length(models))

for (j in seq_along(models)) {
  tt <- broom::tidy(models[[j]])
  tt <- tt[match(paste0("treat_B_temp_", treat_years), tt$term), ]

  for (i in seq_along(treat_years)) {
    coef_mat[2 * i - 1, j] <- paste0(sprintf("%.5f", tt$estimate[i]), star_fun(tt$p.value[i]))
    coef_mat[2 * i, j]     <- paste0("(", sprintf("%.5f", tt$std.error[i]), ")")
  }

  obs_vec[j] <- nobs(models[[j]])
}

header_cols <- paste(as.vector(rbind(paste0("(", seq_along(models), ")"), rep("", length(models)))), collapse = " & ")
dep_row <- make_row("  Dep. Var:", rep("Relocation", length(models)))
treat_row <- make_row("Treatment Group", names(bands))
ctrl_row <- make_row("Control Group", rep("50-80 km", length(models)))
obs_row <- make_row("Observations", format(obs_vec, big.mark = ",", scientific = FALSE))
est_fe_row <- make_row("Establishment FE", rep("Yes", length(models)))
year_fe_row <- make_row("Year FE", rep("Yes", length(models)))

flash_rows <- c()
for (i in seq_along(treat_years)) {
  flash_rows <- c(
    flash_rows,
    make_row(paste("Flash Flood", treat_years[i]), coef_mat[2 * i - 1, ]),
    make_row("      ", coef_mat[2 * i, ])
  )
}

cmid <- "\\cmidrule{2-2}\\cmidrule{4-4}\\cmidrule{6-6}\\cmidrule{8-8}\\cmidrule{10-10}\\cmidrule{12-12}\\cmidrule{14-14}"

tex_lines <- c(
  "\\begin{table}[htb]",
  "  \\centering",
  "  \\tabcaption{Definition of Treatment Radius: Effects of Flooding on Relocation}",
  "    \\label{tab: def_treat_relocation}",
  "    \\scalebox{0.7}{",
  " \\begin{threeparttable}",
  "    \\begin{tabular}{lccccccccccccccc}",
  "\\toprule",
  paste0("          & ", header_cols, " \\\\"),
  cmid,
  dep_row,
  cmid,
  treat_row,
  ctrl_row,
  "\\midrule",
  flash_rows,
  "\\midrule",
  obs_row,
  est_fe_row,
  year_fe_row,
  "",
  "\\bottomrule",
  "    \\end{tabular}%",
  "        \\begin{tablenotes}[flushleft] \\item \\small \\textit{Notes:} This table shows the estimation of equation (\\ref{eq.eq1}) using business relocation as the outcome variable. In column 1, the treatment area is defined as the radius between 0 to 2.5 km from the flood spots. In column 2, the treatment area is defined as the ring between 2.5 km and 5 km from the flood spots. Columns 3, 4, 5, 6, and 7 use different treatment group definitions, as indicated in the second row. The control ring is fixed and defined as the area located between 50 km and 80 km from the disaster points. Relocation is measured by \\texttt{mover\\_ano\\_mun}. Robust standard errors clustered at the establishment and year level are shown in parentheses. *** represents p $<$ 0.01, ** represents p $<$ 0.05, * represents p $<$ 0.1.",
  "        \\end{tablenotes}",
  "        \\end{threeparttable}",
  "        }",
  "\\end{table}%"
)

writeLines(tex_lines, OUTFILE)
