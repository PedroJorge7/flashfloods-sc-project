# ============================================================================
# Table 2 – Definition of Treatment Radius
#           Effects of Flooding using Different Treatment Bands
#           (replicando o Stata, com formatação LaTeX customizada)
# ============================================================================

rm(list = ls())

source('./results/code/path_utils.R')

library(dplyr)
library(haven)
library(fixest)
library(broom)

# ---------------------------------------------------------------------------
# 1) Base crua + recorte 2003–2012
# ---------------------------------------------------------------------------

dados_raw <- haven::read_dta(data_path("Natural Disastrer Santa Catarina - Dataset.dta"))

dados_raw <- dados_raw %>%
  filter(year >= 2003, year <= 2012) %>%
  arrange(id_estab, year) %>%
  mutate(
    morte_orig         = morte,
    mover_ano_mun_orig = mover_ano_mun
  )

# ---------------------------------------------------------------------------
# 2) Bandas de tratamento (como na tabela do paper)
# ---------------------------------------------------------------------------

bands <- list(
  "0–2.5 km"   = c(NA,   2.5),
  "2.5–5 km"   = c(2.5,  5),
  "5–7.5 km"   = c(5,    7.5),
  "7.5–10 km"  = c(7.5, 10),
  "10–12.5 km" = c(10,  12.5),
  "12.5–15 km" = c(12.5,15),
  "15–17.5 km" = c(15, 17.5)
)

treat_years <- 2008:2012

models   <- list()
col_idx  <- 1L

# ---------------------------------------------------------------------------
# 3) Loop sobre bandas – constrói fechamento/tratamento igual Fig. 3
# ---------------------------------------------------------------------------

for (b_name in names(bands)) {
  
  lower_b <- bands[[b_name]][1]
  upper_b <- bands[[b_name]][2]
  
  temp <- dados_raw %>%
    arrange(id_estab, year) %>%
    group_by(id_estab) %>%
    mutate(
      # definição da banda de tratamento vs controle 50–80 km
      treat_B_temp = case_when(
        is.na(lower_b) & dist_flood <= upper_b ~ 1,
        !is.na(lower_b) & dist_flood > lower_b & dist_flood <= upper_b ~ 1,
        dist_flood >= 50 & dist_flood <= 80 ~ 0,
        TRUE ~ NA_real_
      ),
      morte_temp         = morte_orig,
      mover_ano_mun_temp = mover_ano_mun_orig
    ) %>%
    # fechamento = NA fora de tratamento/controle
    mutate(
      morte_temp = if_else(is.na(treat_B_temp), NA_real_, morte_temp)
    ) %>%
    # forward fill de treat_B_temp usando mover_ano_mun ORIGINAL
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
    # relocação = NA onde treat_B_temp continua NA
    mutate(
      mover_ano_mun_temp = if_else(is.na(treat_B_temp), NA_real_, mover_ano_mun_temp)
    ) %>%
    ungroup()
  
  # dummies ano-específicas de tratamento (2003–2012)
  for (y in 2003:2012) {
    var <- paste0("treat_B_temp_", y)
    temp[[var]] <- dplyr::case_when(
      temp$year == y & !is.na(temp$treat_B_temp) ~ temp$treat_B_temp,
      !is.na(temp$treat_B_temp) & temp$year != y ~ 0,
      TRUE                                       ~ NA_real_
    )
  }
  
  # regressão: Flash Flood 2008–2012, FE id_estab + year
  rhs <- paste0("treat_B_temp_", treat_years, collapse = " + ")
  fml <- as.formula(
    paste0("morte_temp ~ ", rhs, " | id_estab + year")
  )
  
  models[[col_idx]] <- feols(
    fml,
    data    = temp,
    cluster = ~ id_estab,
    lean    = TRUE
  )
  names(models)[col_idx] <- paste0("(", col_idx, ")")
  col_idx <- col_idx + 1L
}

# ---------------------------------------------------------------------------
# 4) Extrair coeficientes / SE e montar matriz coef_mat
# ---------------------------------------------------------------------------

star_fun <- function(p) {
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

row_labs <- paste("Flash Flood", treat_years)

coef_mat <- matrix("", nrow = length(row_labs) * 2, ncol = length(models))
rownames(coef_mat) <- as.vector(rbind(row_labs, paste0("(", row_labs, ")")))
colnames(coef_mat) <- names(models)

obs_vec <- numeric(length(models))

for (j in seq_along(models)) {
  m  <- models[[j]]
  tt <- broom::tidy(m)
  tt <- tt[match(paste0("treat_B_temp_", treat_years), tt$term), ]
  
  for (i in seq_along(treat_years)) {
    est <- tt$estimate[i]
    se  <- tt$std.error[i]
    p   <- tt$p.value[i]
    
    coef_mat[2*i - 1, j] <- paste0(sprintf("%.5f", est), star_fun(p))
    coef_mat[2*i,     j] <- paste0("(", sprintf("%.5f", se), ")")
  }
  
  obs_vec[j] <- nobs(m)
}

# ============================================================================
# 5) Montar LaTeX no formato da tabela do paper
# ============================================================================

# função para intercalar valores com colunas vazias
make_row <- function(label, vals) {
  inter <- as.vector(rbind(vals, rep("", length(vals))))
  paste0(label, " & ", paste(inter, collapse = " & "), " \\\\")
}

bands_labels <- names(bands)              # Treatment group labels
col_ids      <- paste0("(", 1:length(models), ")")

# cabeçalho – linha dos números das colunas
header_cols <- paste(
  as.vector(rbind(col_ids, rep("", length(col_ids)))),
  collapse = " & "
)
line_header1 <- paste0("          & ", header_cols, " \\\\")

# Dep Var row
dep_vals    <- rep("Closure", length(models))
dep_row     <- make_row("  Dep. Var:", dep_vals)

# Treatment group row
treat_row   <- make_row("Treatment Group", bands_labels)

# Control group row
ctrl_vals   <- rep("50–80 km", length(models))
ctrl_row    <- make_row("Control Group", ctrl_vals)

# linhas de coeficientes/EP por ano
flash_rows <- c()
for (i in seq_along(treat_years)) {
  coef_vals <- coef_mat[2*i - 1, ]
  se_vals   <- coef_mat[2*i, ]
  flash_rows <- c(
    flash_rows,
    make_row(paste("Flash Flood", treat_years[i]), coef_vals),
    make_row("      ", se_vals)
  )
}

# Observações e linhas fixas
obs_vals <- format(obs_vec, big.mark = ",", scientific = FALSE)
obs_row  <- make_row("Observations", obs_vals)

yes_vals <- rep("Yes", length(models))
no_vals  <- rep("No",  length(models))

est_fe_row <- make_row("Establishment FE", yes_vals)
year_fe_row <- make_row("Year FE", yes_vals)
trend_row   <- make_row("Census trend", no_vals)

# ---------------------------------------------------------------------------
# 6) Juntar tudo em um vetor de linhas LaTeX
# ---------------------------------------------------------------------------

tex_lines <- c(
  "\\begin{table}[htb]",
  "  \\centering",
  "  \\tabcaption{Definition of Treatment Radius: Effects of Flooding using Different Treatment Bands}",
  "    \\label{tab: def_treat}",
  "    \\scalebox{0.7}{",
  " \\begin{threeparttable}",
  "    \\begin{tabular}{lccccccccccccccc}",
  "\\toprule",
  line_header1,
  "\\cmidrule{2-2}\\cmidrule{4-4}\\cmidrule{6-6}\\cmidrule{8-8}\\cmidrule{10-10}\\cmidrule{12-12}\\cmidrule{14-14}",
  dep_row,
  "\\cmidrule{2-2}\\cmidrule{4-4}\\cmidrule{6-6}\\cmidrule{8-8}\\cmidrule{10-10}\\cmidrule{12-12}\\cmidrule{14-14}",
  treat_row,
  ctrl_row,
  "\\midrule",
  flash_rows,
  "\\midrule",
  obs_row,
  est_fe_row,
  year_fe_row,
  trend_row,
  "",
  "\\bottomrule",
  "    \\end{tabular}%",
  "        \\begin{tablenotes}[flushleft] \\item \\small \\textit{Notes:} This table shows the estimation of equation (\\ref{eq.eq1}) using business closure as the outcome variable. In column 1, the treatment area is defined as the radius between 0 to 2.5 km to the flood spots. In column 2, the treatment area is defined as the ring between 2.5 km and 5 km from the flood spots. Columns 3, 4, 5, 6, and 7 use different treatment group definitions, as indicated in the second row. The control ring is fixed and defined as the area located between 50 km to 80 km from the disaster points. Robust standard errors clustered at the establishment and year level are shown in parentheses. *** represents p $<$ 0.01,** represents p $<$ 0.05,* represents p $<$ 0.1.",
  "        \\end{tablenotes}",
  "        \\end{threeparttable}",
  "        }",
  "\\end{table}%"
)

writeLines(tex_lines, "./results/analysis/Tab_02_Definition_Treatment_Radius.tex")
