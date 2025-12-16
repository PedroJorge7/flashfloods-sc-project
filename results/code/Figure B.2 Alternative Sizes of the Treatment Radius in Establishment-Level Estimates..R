# ============================================================================
# Figure 6: Alternative Sizes of the Treatment Radius – Establishment Level
#   AJUSTE: Relocation = reloc_tract_tminus1 (Census Tract, t-1 via code_tract)
#   IMPORTANTE: reloc_tract_tminus1 é construído ANTES do corte 2003–2012
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
# Helper: code_tract precisa ser numérico p/ varying slopes (fixest)
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
    
    reloc_tract_tminus1 = lead(diff_tract, 1)  # NA no último ano do estab
  ) %>%
  ungroup() %>%
  select(-ct, -ct_lag, -diff_tract) %>%
  group_by(id_estab) %>%
  mutate(
    reloc_tract_tminus1 = if_else(
      year == min(year, na.rm = TRUE) & reloc_tract_tminus1 == 1,
      0, reloc_tract_tminus1
    ),
  ) %>%
  ungroup() |> 
  mutate(reloc_tract_tminus1 = if_else(is.na(reloc_tract_tminus1),0,reloc_tract_tminus1),
         reloc_tract_tminus1 = if_else(is.na(morte), NA_real_, reloc_tract_tminus1))

# ---------------------------------------------------------
# 2) AGORA sim: manter somente 2003–2012
# ---------------------------------------------------------
data <- data %>%
  filter(year >= 2003 & year <= 2012) %>%
  arrange(id_estab, year)

# ---------------------------------------------------------
# 3) Construção de tratamento principal (igual Tabela 3 / Fig. 5)
# ---------------------------------------------------------
has_new_firm <- "new_firm" %in% names(data)

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
    mover_ano_mun_orig      = mover_ano_mun,
    reloc_tract_tminus1_orig = reloc_tract_tminus1,
    new_firm_orig           = if (has_new_firm) new_firm else NA_real_
  ) %>%
  # outcomes = NA onde treat_B é missing (como no Stata)
  mutate(
    morte    = if_else(is.na(treat_B), NA_real_, morte),
    new_firm = if (has_new_firm) if_else(is.na(treat_B), NA_real_, new_firm) else NA_real_
  ) %>%
  # forward fill de treat_B usando relocação municipal ORIGINAL (como no Stata)
  mutate(
    treat_B = {
      tb  <- treat_B
      mov <- mover_ano_mun  # aqui ainda é o "bruto"
      for (i in seq_along(tb)) {
        if (i > 1 && is.na(tb[i]) && !is.na(mov[i]) && mov[i] == 1) {
          tb[i] <- tb[i - 1]
        }
      }
      tb
    }
  ) %>%
  # relocation (tract t-1) vira NA onde treat_B continua NA
  mutate(
    reloc_tract_tminus1 = if_else(is.na(treat_B), NA_real_, reloc_tract_tminus1),
    # sua regra (amostra alinhada): se closure é NA, relocation também vira NA
    reloc_tract_tminus1 = if_else(is.na(reloc_tract_tminus1),0,reloc_tract_tminus1),
    reloc_tract_tminus1 = if_else(is.na(morte), NA_real_, reloc_tract_tminus1),
    # (mantém igual ao teu padrão)
    mover_ano_mun = if_else(is.na(treat_B), NA_real_, mover_ano_mun)
  ) %>%
  ungroup()

# tendência pós-choque para o slope por tract
data <- data %>%
  mutate(
    treat_trend    = if_else(year >= 2008, 1, 0),
    code_tract_num = make_tract_num(code_tract)
  )

# ---------------------------------------------------------
# 4) Especificações de raios de tratamento
# ---------------------------------------------------------
radius_specs <- list(
  "0-2.5 km"  = 2.5,
  "0-7.5 km"  = 7.5,
  "0-17.5 km" = 17.5,
  "0-22.5 km" = 22.5
)

# Só dois outcomes: fechamento e relocation (TRACT t-1)
outcomes <- c("morte", "reloc_tract_tminus1")

all_results <- data.frame()

# ---------------------------------------------------------
# 5) Loop sobre raios e outcomes – Post + dummies 2008–2012
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
      # sempre partir dos originais
      morte_temp               = morte_orig,
      mover_ano_mun_temp       = mover_ano_mun_orig,         # só p/ forward fill do treat
      reloc_tract_tminus1_temp = reloc_tract_tminus1_orig
    ) %>%
    group_by(id_estab) %>%
    mutate(
      # 1) outcomes = NA onde treat_B_temp é NA
      morte_temp               = if_else(is.na(treat_B_temp), NA_real_, morte_temp),
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
      
      # 3) só AGORA: relocation vira NA onde treat_B_temp continua NA
      reloc_tract_tminus1_temp = if_else(is.na(treat_B_temp), NA_real_, reloc_tract_tminus1_temp),
      
      # (alinhamento: se closure é NA, relocation também NA)
      reloc_tract_tminus1_temp = if_else(is.na(morte_temp), NA_real_, reloc_tract_tminus1_temp)
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
      mutate(Period = gsub("treat_B_temp_", "", term)) %>%
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
# 6) Preparar dados, paleta e painéis A/B
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
    geom_errorbar(aes(ymin = CI_Low, ymax = CI_High),
                  position = pos_dodge, width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_discrete(drop = FALSE) +
    scale_color_manual(values = rad_colors, name = "Treatment Radius") +
    labs(x = "Year", y = "Coefficient", title = title_label) +
    theme_bw()
}

pA <- make_panel(
  df = dplyr::filter(all_results, Outcome == "Closure"),
  title_label = "A - Establishment Closure (0/1)"
)

pB <- make_panel(
  df = dplyr::filter(all_results, Outcome == "Relocation"),
  title_label = "B - Establishment Relocation (0/1) [Census Tract, t-1]"
)

# ---------------------------------------------------------
# 7) Figura final + salvar
# ---------------------------------------------------------
fig_rad <- ggpubr::ggarrange(
  pA, pB,
  ncol          = 2,
  common.legend = TRUE,
  legend        = "bottom"
)

dir.create("results/analysis", recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = "results/analysis/change_treatment.png",
  plot     = fig_rad,
  dpi      = 300,
  width    = 12,
  height   = 6,
  units    = "in"
)
