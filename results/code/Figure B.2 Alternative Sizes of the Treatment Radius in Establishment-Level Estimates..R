# ============================================================================
# Figure 6: Alternative Sizes of the Treatment Radius – Establishment Level
# ============================================================================

rm(list = ls())

library(dplyr)
library(tidyr)
library(haven)
library(fixest)
library(ggplot2)
library(ggpubr)
library(broom)

# ---------------------------------------------------------
# 1) Carregar base
# ---------------------------------------------------------

data <- haven::read_dta("./data/Natural Disastrer Santa Catarina - Dataset.dta")

# Manter somente 2003–2012
data <- data %>%
  filter(year >= 2003 & year <= 2012) %>%
  arrange(id_estab, year)

# ---------------------------------------------------------
# 2) Construção de tratamento principal (igual Tabela 3 / Fig. 5)
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
    morte_orig         = morte,
    new_firm_orig      = new_firm,
    mover_ano_mun_orig = mover_ano_mun
  ) %>%
  # outcomes = NA onde treat_B é missing (como no Stata)
  mutate(
    morte    = if_else(is.na(treat_B), NA_real_, morte),
    new_firm = if_else(is.na(treat_B), NA_real_, new_firm)
  ) %>%
  # forward fill de treat_B usando relocação
  mutate(
    treat_B = {
      tb  <- treat_B
      mov <- mover_ano_mun
      for (i in seq_along(tb)) {
        if (i > 1 && is.na(tb[i]) && !is.na(mov[i]) && mov[i] == 1) {
          tb[i] <- tb[i - 1]
        }
      }
      tb
    }
  ) %>%
  # relocação também vira NA onde treat_B continua NA
  mutate(
    mover_ano_mun = if_else(is.na(treat_B), NA_real_, mover_ano_mun)
  ) %>%
  ungroup()

if ("mover_ano_tract" %in% names(data)) {
  data <- data %>%
    mutate(mover_ano_tract = if_else(is.na(treat_B), NA_real_, mover_ano_tract))
}

if ("mover_ano_cep" %in% names(data)) {
  data <- data %>%
    mutate(mover_ano_cep = if_else(is.na(treat_B), NA_real_, mover_ano_cep))
}

# tendência pós-choque para o slope por tract
data <- data %>%
  mutate(
    treat_trend = if_else(year >= 2008, 1, 0)
  )

# ---------------------------------------------------------
# 3) Especificações de raios de tratamento (Figura B.2 original)
# ---------------------------------------------------------

radius_specs <- list(
  "0-2.5 km"  = 2.5,
  "0-7.5 km"  = 7.5,
  "0-17.5 km" = 17.5,
  "0-22.5 km" = 22.5
)

# Só dois outcomes: fechamento e relocation
outcomes <- c("morte", "mover_ano_mun")

all_results <- data.frame()

# ---------------------------------------------------------
# 4) Loop sobre raios e outcomes – Post + dummies 2008–2012
# ---------------------------------------------------------

for (rad_lab in names(radius_specs)) {
  
  radius <- radius_specs[[rad_lab]]
  
  temp_data <- data %>%
    mutate(
      # tratamento alternativo: 0–radius vs 50–80 km
      treat_B_temp = case_when(
        dist_flood <= radius ~ 1,
        dist_flood >= 50 & dist_flood <= 80 ~ 0,
        TRUE ~ NA_real_
      ),
      morte_temp         = morte_orig,
      new_firm_temp      = new_firm_orig,
      mover_ano_mun_temp = mover_ano_mun_orig
    ) %>%
    group_by(id_estab) %>%
    mutate(
      # 1) outcomes = NA onde treat_B_temp é NA
      morte_temp    = if_else(is.na(treat_B_temp), NA_real_, morte_temp),
      new_firm_temp = if_else(is.na(treat_B_temp), NA_real_, new_firm_temp),
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
      # 3) relocação vira NA onde treat_B_temp continua NA
      mover_ano_mun_temp = if_else(is.na(treat_B_temp), NA_real_, mover_ano_mun_temp)
    ) %>%
    ungroup()
  
  # dummies 2008–2012 e dummy agregada (Post)
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
      paste0(yvar, " ~ treat_B_agg_temp | id_estab + year + treat_trend[code_tract]")
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
        Treatment_Radius = rad_lab,
        Outcome          = outcome,
        Period           = "Post",
        Coefficient      = estimate,
        SE               = std.error,
        CI_Low           = estimate - 1.96 * std.error,
        CI_High          = estimate + 1.96 * std.error
      )
    
    # (2) modelo com dummies 2008–2012
    treat_vars <- paste0("treat_B_temp_", 2008:2012)
    fml_evt <- as.formula(
      paste0(yvar, " ~ ", paste(treat_vars, collapse = " + "),
             " | id_estab + year + treat_trend[code_tract]")
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
        Treatment_Radius = rad_lab,
        Outcome          = outcome,
        Period           = Period,
        Coefficient      = estimate,
        SE               = std.error,
        CI_Low           = estimate - 1.96 * std.error,
        CI_High          = estimate + 1.96 * std.error
      )
    
    all_results <- bind_rows(all_results, agg_row, evt_rows)
  }
}

# ---------------------------------------------------------
# 5) Preparar dados, paleta e painéis A/B
# ---------------------------------------------------------

all_results <- all_results %>%
  mutate(
    Outcome = factor(Outcome,
                     levels = c("morte", "mover_ano_mun"),
                     labels = c("Closure", "Relocation")),
    Period  = factor(
      Period,
      levels = c("Post", "2008", "2009", "2010", "2011", "2012")
    ),
    Treatment_Radius = factor(
      Treatment_Radius,
      levels = c("0-2.5 km", "0-7.5 km", "0-17.5 km", "0-22.5 km")
    )
  )

rad_colors <- c(
  "0-2.5 km"  = "#FEE0D2",
  "0-7.5 km"  = "#FCBBA1",
  "0-17.5 km" = "#FB6A4A",
  "0-22.5 km" = "#CB181D"
)

pos_dodge <- position_dodge(width = 0.4)

make_panel <- function(df, title_label) {
  ggplot(df, aes(x = Period, y = Coefficient, color = Treatment_Radius)) +
    geom_point(position = pos_dodge, size = 2) +
    geom_errorbar(
      aes(ymin = CI_Low, ymax = CI_High),
      position = pos_dodge, width = 0.2
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_discrete(drop = FALSE) +
    scale_color_manual(values = rad_colors, name = "Treatment Radius") +
    labs(
      x = "Year",
      y = "Coefficient",
      title = title_label
    ) +
    theme_bw()
}

# Painel A - Closure
pA <- make_panel(
  df = dplyr::filter(all_results, Outcome == "Closure"),
  title_label = "A - Establishment Closure (0/1)"
)

# Painel B - Relocation
pB <- make_panel(
  df = dplyr::filter(all_results, Outcome == "Relocation"),
  title_label = "B - Establishment Relocation (0/1)"
)

# ---------------------------------------------------------
# 6) Figura final com legenda única embaixo (ggpubr)
# ---------------------------------------------------------

fig_rad <- ggpubr::ggarrange(
  pA,
  pB,
  ncol          = 2,
  common.legend = TRUE,
  legend        = "bottom"
)

# ---------------------------------------------------------
# 7) Salvar figura e resultados
# ---------------------------------------------------------

ggsave(
  filename = "establishments/analysis/Fig_06_Alternative_Treatment_Radius.png",
  plot     = fig_rad,
  dpi      = 300,
  width    = 12,
  height   = 6,
  units    = "in"
)

