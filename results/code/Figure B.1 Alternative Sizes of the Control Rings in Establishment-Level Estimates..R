# ============================================================================
# Figure 5: Alternative Control Rings - Establishment Level (R version)
#   AJUSTE: Relocation = reloc_tract_tminus1 (Census Tract, t-1 via code_tract)
#   IMPORTANTE: reloc_tract_tminus1 é construído ANTES do corte 2003–2012
# ============================================================================

rm(list = ls())

library(dplyr)
library(tidyr)
library(haven)
library(fixest)
library(ggplot2)
library(broom)
library(cowplot)
library(ggpubr)

# ---------------------------------------------------------
# Helper: code_tract precisa ser numérico para varying slopes
# ---------------------------------------------------------
make_tract_num <- function(x) {
  x_chr <- as.character(x)
  x_chr[x_chr %in% c("", "NA")] <- NA_character_
  
  x_num <- suppressWarnings(as.numeric(x_chr))
  bad   <- !is.na(x_chr) & is.na(x_num)
  
  # se muita coisa não parseia, usa codificação por fator (mantém todos)
  if (sum(!is.na(x_chr)) > 0 && mean(bad) > 0.2) {
    x_num <- as.numeric(factor(x_chr))
  }
  x_num
}

# ---------------------------------------------------------
# 1) Carregar base (SEM cortar ainda)
# ---------------------------------------------------------
data <- haven::read_dta("./data/Natural Disastrer Santa Catarina - Dataset.dta") %>%
  arrange(id_estab, year)

stopifnot(all(c("id_estab","year","dist_flood","morte","mover_ano_mun","code_tract") %in% names(data)))

# ---------------------------------------------------------
# 1.1) Construir relocation correto ANTES do corte:
#      reloc_tract_tminus1(t) = 1 se muda entre t e t+1 (via code_tract)
#      (lead do indicador de mudança contemporânea)
#      + regra: no primeiro ano do estabelecimento, se der 1 -> vira 0
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
  select(-ct, -ct_lag, -diff_tract) %>%
  group_by(id_estab) %>%
  mutate(
    reloc_tract_tminus1 = if_else(
      year == min(year, na.rm = TRUE) & reloc_tract_tminus1 == 1,
      0, reloc_tract_tminus1
    )
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 2) AGORA sim: manter somente 2003–2012
# ---------------------------------------------------------
data <- data %>%
  filter(year >= 2003 & year <= 2012) %>%
  arrange(id_estab, year)

# ---------------------------------------------------------
# 3) Construção de tratamento principal (igual Tabela 3 / Fig. 3)
# ---------------------------------------------------------
data <- data %>%
  arrange(id_estab, year) %>%
  group_by(id_estab) %>%
  mutate(
    # tratamento principal (0–12.5 km vs 50–80 km)
    treat_B = case_when(
      dist_flood <= 12.5 ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE ~ NA_real_
    ),
    # guardar originais
    morte_orig              = morte,
    new_firm_orig           = new_firm,
    mover_ano_mun_orig      = mover_ano_mun,
    reloc_tract_tminus1_orig = reloc_tract_tminus1
  ) %>%
  # outcomes = NA onde treat_B é missing (como no Stata)
  mutate(
    morte    = if_else(is.na(treat_B), NA_real_, morte),
    new_firm = if_else(is.na(treat_B), NA_real_, new_firm)
  ) %>%
  # forward fill de treat_B usando relocação municipal ORIGINAL (como no teu script)
  mutate(
    treat_B = {
      tb  <- treat_B
      mov <- mover_ano_mun_orig
      for (i in seq_along(tb)) {
        if (i > 1 && is.na(tb[i]) && !is.na(mov[i]) && mov[i] == 1) {
          tb[i] <- tb[i - 1]
        }
      }
      tb
    }
  ) %>%
  # relocation (tract t-1) também vira NA onde treat_B continua NA
  mutate(
    reloc_tract_tminus1 = if_else(is.na(treat_B), NA_real_, reloc_tract_tminus1),
    reloc_tract_tminus1 = if_else(is.na(reloc_tract_tminus1),0,reloc_tract_tminus1),
    reloc_tract_tminus1 = if_else(is.na(morte), NA_real_, reloc_tract_tminus1)
  ) %>%
  ungroup()

# tendência pós-choque para o slope por tract
data <- data %>%
  mutate(
    treat_trend     = if_else(year >= 2008, 1, 0),
    code_tract_num  = make_tract_num(code_tract)
  )

# ---------------------------------------------------------
# 4) Especificações dos anéis de controle (FIG ORIGINAL)
# ---------------------------------------------------------
control_specs <- list(
  "30-50 km"  = list(lower = 30, upper = 50),
  "30-80 km"  = list(lower = 30, upper = 80),
  "60-80 km"  = list(lower = 60, upper = 80),
  "50-100 km" = list(lower = 50, upper = 100)
)

# Só dois outcomes: fechamento e relocation (TRACT t-1)
outcomes <- c("morte", "reloc_tract_tminus1")

all_results <- data.frame()

# ---------------------------------------------------------
# 5) Loop sobre anéis de controle e outcomes
#     (replicando a ordem dos replace do Stata)
# ---------------------------------------------------------
for (ring in names(control_specs)) {
  
  spec    <- control_specs[[ring]]
  lower_c <- spec$lower
  upper_c <- spec$upper
  
  temp_data <- data %>%
    mutate(
      # definição de tratamento alternativa
      treat_B_temp = case_when(
        dist_flood <= 12.5 ~ 1,
        dist_flood >= lower_c & dist_flood <= upper_c ~ 0,
        TRUE ~ NA_real_
      ),
      # sempre partir dos originais
      morte_temp               = morte_orig,
      new_firm_temp            = new_firm_orig,
      mover_ano_mun_temp       = mover_ano_mun_orig,        # só para o forward fill do treat
      reloc_tract_tminus1_temp = reloc_tract_tminus1_orig   # <- outcome correto
    ) %>%
    group_by(id_estab) %>%
    mutate(
      # 1) outcomes = NA onde treat_B_temp é NA
      morte_temp               = if_else(is.na(treat_B_temp), NA_real_, morte_temp),
      new_firm_temp            = if_else(is.na(treat_B_temp), NA_real_, new_firm_temp),
      reloc_tract_tminus1_temp = if_else(is.na(treat_B_temp), NA_real_, reloc_tract_tminus1_temp),
      
      # 2) forward fill de treat_B_temp usando mover_ano_mun_temp ORIGINAL
      treat_B_temp = {
        tb  <- treat_B_temp
        mov <- mover_ano_mun_temp
        for (i in seq_along(tb)) {
          if (i > 1 && is.na(tb[i]) && !is.na(mov[i]) && mov[i] == 1) {
            tb[i] <- tb[i - 1]
          }
        }
        tb
      },
      
      # 3) só AGORA zera relocation onde treat_B_temp continua NA
      reloc_tract_tminus1_temp = if_else(is.na(treat_B_temp), NA_real_, reloc_tract_tminus1_temp)
    ) %>%
    ungroup()
  
  # dummies 2008–2012 e dummy agregada (igual Stata)
  for (y in 2008:2012) {
    var <- paste0("treat_B_temp_", y)
    temp_data[[var]] <- dplyr::case_when(
      temp_data$year == y & !is.na(temp_data$treat_B_temp) ~ temp_data$treat_B_temp,
      !is.na(temp_data$treat_B_temp) & temp_data$year != y  ~ 0,
      TRUE                                                  ~ NA_real_
    )
  }
  
  temp_data <- temp_data %>%
    mutate(
      treat_B_agg_temp = case_when(
        year >= 2008 & treat_B_temp == 1 ~ 1,
        !is.na(treat_B_temp)             ~ 0,
        TRUE                             ~ NA_real_
      )
    )
  
  # regressões por outcome
  for (outcome in outcomes) {
    
    yvar <- paste0(outcome, "_temp")
    
    # (1) modelo agregado (Post)
    fml_agg <- as.formula(
      paste0(yvar, " ~ treat_B_agg_temp | id_estab + year + treat_trend[code_tract_num]")
    )
    m_agg <- feols(
      fml_agg,
      data    = temp_data,
      cluster = ~ id_estab,
      lean    = TRUE
    )
    agg_row <- broom::tidy(m_agg) %>%
      dplyr::filter(term == "treat_B_agg_temp") %>%
      transmute(
        Control_Ring = ring,
        Outcome      = outcome,
        Period       = "Post",
        Coefficient  = estimate,
        SE           = std.error,
        CI_Low       = estimate - 1.96 * std.error,
        CI_High      = estimate + 1.96 * std.error
      )
    
    # (2) modelo com dummies 2008–2012
    treat_vars <- paste0("treat_B_temp_", 2008:2012)
    fml_evt <- as.formula(
      paste0(yvar, " ~ ", paste(treat_vars, collapse = " + "),
             " | id_estab + year + treat_trend[code_tract_num]")
    )
    m_evt <- feols(
      fml_evt,
      data    = temp_data,
      cluster = ~ id_estab + year,
      lean    = TRUE
    )
    evt_rows <- broom::tidy(m_evt) %>%
      dplyr::filter(grepl("^treat_B_temp_", term)) %>%
      mutate(
        Period = gsub("treat_B_temp_", "", term)
      ) %>%
      transmute(
        Control_Ring = ring,
        Outcome      = outcome,
        Period       = Period,
        Coefficient  = estimate,
        SE           = std.error,
        CI_Low       = estimate - 1.96 * std.error,
        CI_High      = estimate + 1.96 * std.error
      )
    
    all_results <- bind_rows(all_results, agg_row, evt_rows)
  }
}

# ---------------------------------------------------------
# 6) Preparar dados e painéis A/B
# ---------------------------------------------------------
all_results <- all_results %>%
  mutate(
    Outcome = factor(
      Outcome,
      levels = c("morte", "reloc_tract_tminus1"),
      labels = c("Closure", "Relocation")
    ),
    Period  = factor(
      Period,
      levels = c("Post", "2008", "2009", "2010", "2011", "2012")
    ),
    Control_Ring = factor(
      Control_Ring,
      levels = c("30-50 km", "30-80 km", "60-80 km", "50-100 km")
    )
  )

ring_colors <- c(
  "30-50 km"  = "#FEE0D2",
  "30-80 km"  = "#FCBBA1",
  "60-80 km"  = "#FB6A4A",
  "50-100 km" = "#CB181D"
)

pos_dodge <- position_dodge(width = 0.4)

make_panel <- function(df, title_label) {
  ggplot(df, aes(x = Period, y = Coefficient, color = Control_Ring)) +
    geom_point(position = pos_dodge, size = 2) +
    geom_errorbar(aes(ymin = CI_Low, ymax = CI_High),
                  position = pos_dodge, width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_discrete(drop = FALSE) +
    scale_color_manual(values = ring_colors, name = "Control Radius") +
    labs(x = "Year", y = "Coefficient", title = title_label) +
    theme_bw()
}

pA <- make_panel(
  df = dplyr::filter(all_results, Outcome == "Closure"),
  title_label = "A - Establishment Closure (0/1)"
) + theme(legend.position = "bottom")

pB <- make_panel(
  df = dplyr::filter(all_results, Outcome == "Relocation"),
  title_label = "B - Establishment Relocation (0/1)"
) + theme(legend.position = "bottom")

fig_ctrl <- ggpubr::ggarrange(
  pA, pB,
  ncol  = 2,
  common.legend = TRUE,
  legend = "bottom"
)

# ---------------------------------------------------------
# 7) Salvar figura
# ---------------------------------------------------------

ggsave(
  filename = "results/analysis/results_controles.png",
  plot     = fig_ctrl,
  dpi      = 300,
  width    = 12,
  height   = 6,
  units    = "in"
)
