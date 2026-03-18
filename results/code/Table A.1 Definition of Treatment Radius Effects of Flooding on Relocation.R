# ============================================================================
# Table A.1 - Definition of Treatment Radius
#             Effects of Flooding on Relocation
#           (replicando o Stata, com formataÃ§Ã£o LaTeX customizada)
#
# AJUSTE: "nova definiÃ§Ã£o de movimentaÃ§Ã£o" = reloc_tract_tminus1 (Census Tract, t-1)
# - Igual ao seu Figure 4:
#     diff_tract(t) = 1 se code_tract(t) != code_tract(t-1)
#     reloc_tract_tminus1(t) = lead(diff_tract, 1)  # mudanÃ§a entre t e t+1
#     + A REGRA QUE VOCÃŠ EXIGIU:
#         group_by(id_estab) |> mutate(
#           reloc_tract_tminus1 = ifelse(year == min(year, na.rm=TRUE) & reloc_tract_tminus1==1, 0, reloc_tract_tminus1)
#         )
# - Usa reloc_tract_tminus1 no forward-fill do treat_B_temp (Stata-style)
# - Outcome da Tabela 2: Closure (morte_temp)
# - Cluster 2-way: id_estab + year
# - SaÃ­da: ./results/analysis/Tab_A1_Definition_Treatment_Radius_Relocation.tex
# ============================================================================

rm(list = ls())

source('./results/code/path_utils.R')

library(dplyr)
library(haven)
library(fixest)
library(broom)

# ---------------------------------------------------------------------------
# 0) Output
# ---------------------------------------------------------------------------
OUT_DIR <- "./results/analysis"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
OUTFILE <- file.path(OUT_DIR, "Tab_A1_Definition_Treatment_Radius_Relocation.tex")

# ---------------------------------------------------------------------------
# 1) Base crua + recorte 2003â€“2012
# ---------------------------------------------------------------------------
dados_raw <- haven::read_dta(data_path("Natural Disastrer Santa Catarina - Dataset.dta")) %>%
  filter(year >= 2003, year <= 2012) %>%
  arrange(id_estab, year) %>%
  mutate(
    morte_orig = as.numeric(haven::zap_labels(morte)) # garante numÃ©rico
  )

stopifnot(all(c("id_estab","year","dist_flood","code_tract","morte_orig") %in% names(dados_raw)))

# ---------------------------------------------------------------------------
# 2) Bandas de tratamento (como na tabela do paper)
# ---------------------------------------------------------------------------
bands <- list(
  "0-2.5 km"   = c(NA,   2.5),
  "2.5-5 km"   = c(2.5,  5),
  "5-7.5 km"   = c(5,    7.5),
  "7.5-10 km"  = c(7.5, 10),
  "10-12.5 km" = c(10,  12.5),
  "12.5-15 km" = c(12.5,15),
  "15-17.5 km" = c(15,  17.5)
)

treat_years <- 2008:2012

models  <- list()
col_idx <- 1L

# ---------------------------------------------------------------------------
# 3) Loop sobre bandas â€“ treat/control + reloc_tract_tminus1 + fechamento
# ---------------------------------------------------------------------------
for (b_name in names(bands)) {
  
  lower_b <- bands[[b_name]][1]
  upper_b <- bands[[b_name]][2]
  
  temp <- dados_raw %>%
    arrange(id_estab, year) %>%
    group_by(id_estab) %>%
    mutate(
      # ---------------------------------------------------
      # (A) Banda de tratamento vs controle 50â€“80 km
      # ---------------------------------------------------
      treat_B_temp = case_when(
        is.na(lower_b)  & dist_flood <= upper_b                        ~ 1,
        !is.na(lower_b) & dist_flood > lower_b & dist_flood <= upper_b ~ 1,
        dist_flood >= 50 & dist_flood <= 80                             ~ 0,
        TRUE                                                            ~ NA_real_
      ),
      
      # ---------------------------------------------------
      # (B) Outcome: Closure
      # ---------------------------------------------------
      morte_temp = morte_orig,
      
      # ---------------------------------------------------
      # (C) reloc_tract_tminus1 (igual ao seu Figure 4)
      # ---------------------------------------------------
      ct     = as.character(code_tract),
      ct_lag = dplyr::lag(ct, 1),
      
      diff_tract = dplyr::case_when(
        is.na(ct) | is.na(ct_lag) ~ NA_real_,
        ct != ct_lag              ~ 1,
        TRUE                      ~ 0
      ),
      
      reloc_tract_tminus1 = dplyr::lead(diff_tract, 1)  # NA no Ãºltimo ano do id_estab
    ) %>%
    # ---------------------------------------------------
  # (C.1) A REGRA QUE VOCÃŠ EXIGIU (exatamente):
  # group_by(id_estab) |> mutate(reloc_tract_tminus1 = ifelse(year == min(year, na.rm=TRUE) & reloc_tract_tminus1==1,0,reloc_tract_tminus1))
  # ---------------------------------------------------
  mutate(
    reloc_tract_tminus1 = ifelse(
      year == min(year, na.rm = TRUE) & reloc_tract_tminus1 == 1,
      0,
      reloc_tract_tminus1
    )
  ) %>%
    # ---------------------------------------------------
  # (D) Stata: closure = . fora treat/control
  # ---------------------------------------------------
  mutate(
    morte_temp = if_else(is.na(treat_B_temp), NA_real_, morte_temp)
  ) %>%
    # ---------------------------------------------------
  # (E) Cortes de amostra (igual seu padrÃ£o):
  # - se treat_B_temp Ã© NA => reloc_tract_tminus1 = NA
  # - se morte_temp Ã© NA   => reloc_tract_tminus1 = NA
  # ---------------------------------------------------
  mutate(
    reloc_tract_tminus1 = if_else(is.na(treat_B_temp), NA_real_, as.numeric(reloc_tract_tminus1)),
    reloc_tract_tminus1 = if_else(is.na(morte_temp),   NA_real_, reloc_tract_tminus1)
  ) %>%
    # ---------------------------------------------------
  # (F) Forward-fill do treat_B_temp usando reloc_tract_tminus1
  # ---------------------------------------------------
  mutate(
    treat_B_temp = {
      tb  <- treat_B_temp
      mov <- reloc_tract_tminus1
      for (i in seq_along(tb)) {
        if (i > 1 && is.na(tb[i]) && !is.na(mov[i]) && mov[i] == 1) {
          tb[i] <- tb[i - 1]
        }
      }
      tb
    }
  ) %>%
    # reimpÃµe NA fora do sample depois do fill
    mutate(
      morte_temp          = if_else(is.na(treat_B_temp), NA_real_, morte_temp),
      reloc_tract_tminus1 = if_else(is.na(treat_B_temp), NA_real_, reloc_tract_tminus1),
      reloc_tract_tminus1 = if_else(is.na(morte_temp),   NA_real_, reloc_tract_tminus1)
    ) %>%
    ungroup() %>%
    select(-ct, -ct_lag, -diff_tract)
  
  # dummies ano-especÃ­ficas (2003â€“2012)
  for (y in 2003:2012) {
    var <- paste0("treat_B_temp_", y)
    temp[[var]] <- dplyr::case_when(
      temp$year == y & !is.na(temp$treat_B_temp) ~ temp$treat_B_temp,
      !is.na(temp$treat_B_temp) & temp$year != y ~ 0,
      TRUE                                       ~ NA_real_
    )
  }
  
  # regressÃ£o: Flash Flood 2008â€“2012, FE id_estab + year
  rhs <- paste0("treat_B_temp_", treat_years, collapse = " + ")
  fml <- as.formula(paste0("morte_temp ~ ", rhs, " | id_estab + year"))
  
  models[[col_idx]] <- feols(
    fml,
    data    = temp,
    cluster = ~ id_estab + year,
    lean    = TRUE
  )
  names(models)[col_idx] <- paste0("(", col_idx, ")")
  col_idx <- col_idx + 1L
}

# ---------------------------------------------------------------------------
# 4) Extrair coeficientes / SE
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
make_row <- function(label, vals) {
  inter <- as.vector(rbind(vals, rep("", length(vals))))
  paste0(label, " & ", paste(inter, collapse = " & "), " \\\\")
}

bands_labels <- names(bands)
col_ids      <- paste0("(", 1:length(models), ")")

header_cols  <- paste(as.vector(rbind(col_ids, rep("", length(col_ids)))), collapse = " & ")
line_header1 <- paste0("          & ", header_cols, " \\\\")

dep_vals <- rep("Closure", length(models))
dep_row  <- make_row("  Dep. Var:", dep_vals)

treat_row <- make_row("Treatment Group", bands_labels)

ctrl_vals <- rep("50-80 km", length(models))
ctrl_row  <- make_row("Control Group", ctrl_vals)

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

obs_vals <- format(obs_vec, big.mark = ",", scientific = FALSE)
obs_row  <- make_row("Observations", obs_vals)

yes_vals <- rep("Yes", length(models))
no_vals  <- rep("No",  length(models))

est_fe_row  <- make_row("Establishment FE", yes_vals)
year_fe_row <- make_row("Year FE", yes_vals)
trend_row   <- make_row("Census trend", no_vals)

cmid <- "\\cmidrule{2-2}\\cmidrule{4-4}\\cmidrule{6-6}\\cmidrule{8-8}\\cmidrule{10-10}\\cmidrule{12-12}\\cmidrule{14-14}"

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
  trend_row,
  "",
  "\\bottomrule",
  "    \\end{tabular}%",
  "        \\begin{tablenotes}[flushleft] \\item \\small \\textit{Notes:} This table shows the estimation of equation (\\ref{eq.eq1}) using business closure as the outcome variable. Treatment groups vary by distance bands (second row) while the control ring is fixed at 50--80 km from flood spots. Treatment assignment over time follows the Stata forward-fill logic, but the movement indicator is defined as \\texttt{reloc\\_tract\\_tminus1}, constructed from changes in census tract (\\texttt{code\\_tract}) between $t$ and $t+1$ (lead of the tract-change indicator), including the adjustment that forces \\texttt{reloc\\_tract\\_tminus1}=0 if it equals 1 in the first observed year of the establishment. Robust standard errors clustered at the establishment and year level are shown in parentheses. *** p $<$ 0.01, ** p $<$ 0.05, * p $<$ 0.1.",
  "        \\end{tablenotes}",
  "        \\end{threeparttable}",
  "        }",
  "\\end{table}%"
)

writeLines(tex_lines, OUTFILE)



