# ============================================================================
# Figure B.4: Falsification Test for the Control Area in Establishment-Level Estimates
# FIX DEFINITIVO:
#  - NÃO tem Entry (só Closure e Relocation)
#  - Relocation "crua" = Census Tract, t-1: lead(1[ct(t) != ct(t-1)], 1) + correção 1º ano
#  - Regra alinhada aplicada DENTRO de cada ring: se morte_temp NA => reloc_temp NA
#  - Forward fill do treat_B_temp ANTES de mascarar outcomes
#  - Trend FE: treat_trend[code_tract_num]  (code_tract_num SEM NA!)
#  - Safe feols: se ring/outcome não identifica, não quebra
# ============================================================================

rm(list = ls())

library(dplyr)
library(tidyr)
library(haven)
library(fixest)
library(ggplot2)
library(broom)
library(ggpubr)

dir.create("./results/analysis", recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# 1) Helper: code_tract_num SEM NA (missing vira categoria)
# ---------------------------------------------------------
make_tract_num_no_na <- function(x){
  x_chr <- as.character(x)
  x_chr[x_chr %in% c("", "NA")] <- NA_character_
  x_chr[is.na(x_chr)] <- "__MISSING_TRACT__"
  as.numeric(factor(x_chr))
}

run_feols_safe <- function(fml, data, cluster_fml){
  tryCatch(
    feols(fml, data = data, cluster = cluster_fml, lean = TRUE),
    error = function(e) NULL
  )
}

tidy_term_safe <- function(model, term){
  if (is.null(model)) return(data.frame(estimate = NA_real_, std.error = NA_real_))
  tt <- broom::tidy(model)
  rr <- tt[tt$term == term, c("estimate","std.error")]
  if (nrow(rr) == 0) return(data.frame(estimate = NA_real_, std.error = NA_real_))
  rr[1, , drop = FALSE]
}

# ---------------------------------------------------------
# 2) Load data (não corta antes de construir relocation t-1)
# ---------------------------------------------------------
data <- haven::read_dta("./data/Natural Disastrer Santa Catarina - Dataset.dta") %>%
  filter(year >= 2003) %>%
  arrange(id_estab, year)

need <- c("id_estab","year","dist_flood","morte","mover_ano_mun","code_tract")
miss <- setdiff(need, names(data))
if (length(miss) > 0) stop("Faltam colunas na base: ", paste(miss, collapse = ", "))

# ---------------------------------------------------------
# 3) Variáveis "originais" (numéricas) + relocation CRUA (sem corte por treat_B)
# ---------------------------------------------------------
data <- data %>%
  group_by(id_estab) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    morte_orig         = as.numeric(haven::zap_labels(morte)),
    mover_ano_mun_orig = as.numeric(haven::zap_labels(mover_ano_mun))
  ) %>%
  ungroup()

# Relocation CRUA: Census Tract, t-1 = lead(1[ct(t) != ct(t-1)], 1)
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
    reloc_raw = lead(diff_tract, 1)
  ) %>%
  ungroup() %>%
  select(-ct, -ct_lag, -diff_tract) %>%
  mutate(reloc_raw = as.numeric(reloc_raw))

# correção 1º ano observado (igual seu padrão)
data <- data %>%
  group_by(id_estab) %>%
  mutate(
    reloc_raw = if_else(
      year == min(year, na.rm = TRUE) & reloc_raw == 1,
      0, reloc_raw
    )
  ) %>%
  ungroup()

# NA do reloc_raw vira 0 (missing de tract tratado como "não mudou")
data <- data %>%
  mutate(reloc_raw = if_else(is.na(reloc_raw), 0, reloc_raw))

# ---------------------------------------------------------
# 4) Trend FE (SEM NA em code_tract_num)
# ---------------------------------------------------------
data <- data %>%
  mutate(
    treat_trend    = if_else(year >= 2008, 1, 0),
    code_tract_num = make_tract_num_no_na(code_tract)
  )

# janela da figura
data <- data %>%
  filter(year >= 2003 & year <= 2012) %>%
  arrange(id_estab, year)

# ---------------------------------------------------------
# 5) Rings falsos
# ---------------------------------------------------------
control_specs <- list(
  "200-210 km" = list(lower = 200, upper = 210),
  "200-230 km" = list(lower = 200, upper = 230),
  "230-240 km" = list(lower = 230, upper = 240),
  "210-240 km" = list(lower = 210, upper = 240)
)

outcomes <- c("morte", "reloc_tract_tminus1")

all_results <- data.frame()

for (ring in names(control_specs)) {
  
  spec    <- control_specs[[ring]]
  lower_c <- spec$lower
  upper_c <- spec$upper
  
  temp_data <- data %>%
    mutate(
      treat_B_temp = case_when(
        dist_flood >= 50 & dist_flood <= 80 ~ 1,
        dist_flood >= lower_c & dist_flood <= upper_c ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    group_by(id_estab) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
      # forward fill do treat_B_temp usando mover_ano_mun ORIGINAL (Stata)
      treat_B_temp = {
        tb  <- treat_B_temp
        mov <- mover_ano_mun_orig
        for (i in seq_along(tb)) {
          if (i > 1 && is.na(tb[i]) && !is.na(mov[i]) && mov[i] == 1) tb[i] <- tb[i - 1]
        }
        tb
      }
    ) %>%
    # AGORA sim mascarar outcomes com treat_B_temp final
    mutate(
      morte_temp = if_else(is.na(treat_B_temp), NA_real_, morte_orig),
      
      # relocation do ring vem da reloc_raw (crua), e só depois aplica regra alinhada
      reloc_tract_tminus1_temp = if_else(is.na(treat_B_temp), NA_real_, reloc_raw),
      reloc_tract_tminus1_temp = if_else(is.na(morte_temp), NA_real_, reloc_tract_tminus1_temp)
    ) %>%
    ungroup()
  
  # dummies 2008–2012
  for (y in 2008:2012) {
    v <- paste0("treat_B_temp_", y)
    temp_data[[v]] <- dplyr::case_when(
      temp_data$year == y & !is.na(temp_data$treat_B_temp) ~ temp_data$treat_B_temp,
      !is.na(temp_data$treat_B_temp) & temp_data$year != y  ~ 0,
      TRUE                                                  ~ NA_real_
    )
  }
  
  # Post (agregado)
  temp_data <- temp_data %>%
    mutate(
      treat_B_agg_temp = case_when(
        year >= 2008 & treat_B_temp == 1 ~ 1,
        !is.na(treat_B_temp)             ~ 0,
        TRUE                             ~ NA_real_
      )
    )
  
  # -------------------------------------------------------
  # regressões (Closure + Relocation)
  # -------------------------------------------------------
  for (outcome in outcomes) {
    
    yvar <- paste0(outcome, "_temp")
    
    # (a) Post
    fml_agg <- as.formula(
      paste0(yvar, " ~ treat_B_agg_temp | id_estab + year + treat_trend[code_tract_num]")
    )
    
    m_agg <- run_feols_safe(
      fml = fml_agg,
      data = temp_data,
      cluster_fml = ~ id_estab + year
    )
    
    rr_agg <- tidy_term_safe(m_agg, "treat_B_agg_temp")
    agg_row <- data.frame(
      Control_Ring = ring,
      Outcome      = outcome,
      Period       = "Post",
      Coefficient  = rr_agg$estimate,
      SE           = rr_agg$std.error,
      CI_Low       = rr_agg$estimate - 1.96 * rr_agg$std.error,
      CI_High      = rr_agg$estimate + 1.96 * rr_agg$std.error
    )
    
    # (b) Dummies 2008–2012
    treat_vars <- paste0("treat_B_temp_", 2008:2012)
    fml_evt <- as.formula(
      paste0(yvar, " ~ ", paste(treat_vars, collapse = " + "),
             " | id_estab + year + treat_trend[code_tract_num]")
    )
    
    m_evt <- run_feols_safe(
      fml = fml_evt,
      data = temp_data,
      cluster_fml = ~ id_estab + year
    )
    
    evt_rows <- if (is.null(m_evt)) {
      data.frame()
    } else {
      broom::tidy(m_evt) %>%
        dplyr::filter(grepl("^treat_B_temp_", term)) %>%
        mutate(Period = gsub("treat_B_temp_", "", term)) %>%
        transmute(
          Control_Ring = ring,
          Outcome      = outcome,
          Period       = Period,
          Coefficient  = estimate,
          SE           = std.error,
          CI_Low       = estimate - 1.96 * std.error,
          CI_High      = estimate + 1.96 * std.error
        )
    }
    
    all_results <- bind_rows(all_results, agg_row, evt_rows)
  }
}

# ---------------------------------------------------------
# 6) Preparar dados e plot (2 painéis)
# ---------------------------------------------------------
all_results <- all_results %>%
  mutate(
    Outcome = factor(
      Outcome,
      levels = c("morte", "reloc_tract_tminus1"),
      labels = c("Closure", "Relocation (Census Tract, t-1)")
    ),
    Period = factor(Period, levels = c("Post","2008","2009","2010","2011","2012")),
    Control_Ring = factor(Control_Ring, levels = names(control_specs))
  )

ring_colors <- c(
  "200-210 km" = "#FEE0D2",
  "200-230 km" = "#FCBBA1",
  "230-240 km" = "#FB6A4A",
  "210-240 km" = "#CB181D"
)

pos_dodge <- position_dodge(width = 0.4)

make_panel <- function(df, title_label) {
  ggplot(df, aes(x = Period, y = Coefficient, color = Control_Ring)) +
    geom_point(position = pos_dodge, size = 2, na.rm = TRUE) +
    geom_errorbar(
      aes(ymin = CI_Low, ymax = CI_High),
      position = pos_dodge, width = 0.2, na.rm = TRUE
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_discrete(drop = FALSE) +
    scale_color_manual(values = ring_colors, name = "Control Radius") +
    labs(x = "Year", y = "Coefficient", title = title_label) +
    theme_bw()
}

pA <- make_panel(filter(all_results, Outcome == "Closure"),
                 "A - Establishment Closure (0/1)")

pB <- make_panel(filter(all_results, Outcome == "Relocation (Census Tract, t-1)"),
                 "B - Establishment Relocation")

fig_falsification <- ggpubr::ggarrange(
  pA, pB,
  nrow = 1, ncol = 2,
  common.legend = TRUE,
  legend = "bottom"
)

ggsave(
  filename = "./results/analysis/results_falso_tratamento2.png",
  plot     = fig_falsification,
  dpi      = 300,
  width    = 12,
  height   = 6,
  units    = "in"
)
