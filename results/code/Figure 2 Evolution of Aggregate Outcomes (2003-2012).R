# ============================================================================
# Figure 2: Evolution of Aggregate Outcomes (2003-2012) – 4 panels
#   UPDATED: Relocation measure = Census Tract, t-1 (via code_tract)
# ============================================================================

rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(haven)

# ============================================================================
# 1) Carregar base bruta (NÃO cortar em 2012 antes de construir t-1)
# ============================================================================

data <- haven::read_dta("./data/Natural Disastrer Santa Catarina - Dataset.dta") %>%
  filter(year >= 2003) %>%   # deixa anos posteriores entrarem para construir lead()
  arrange(id_estab, year)

# ============================================================================
# 2) Replicar o tratamento do Stata (treat_B, morte, mover_ano_mun, etc.)
#    antes do collapse
# ============================================================================

data <- data %>%
  arrange(id_estab, year) %>%
  group_by(id_estab) %>%
  mutate(
    # drop treat_* ; gera treat_B exatamente como no Stata
    treat_B = case_when(
      dist_flood <= 12.5 ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE ~ NA_real_
    ),
    # guardar originais (Stata: gen *_orig = *)
    morte_orig         = morte,
    new_firm_orig      = new_firm,
    mover_ano_mun_orig = mover_ano_mun
  ) %>%
  # replace morte = .  if treat_B == .
  mutate(
    morte    = if_else(is.na(treat_B), NA_real_, morte),
    new_firm = if_else(is.na(treat_B), NA_real_, new_firm)
  ) %>%
  # bysort id_estab (year): replace treat_B = treat_B[_n-1] if treat_B == . & mover_ano_mun == 1
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
  # replace mover_ano_mun = . if treat_B == .
  mutate(
    mover_ano_mun = if_else(is.na(treat_B), NA_real_, mover_ano_mun)
  ) %>%
  ungroup()

# (se existirem, replicaria o corte para mover_ano_tract / mover_ano_cep aqui)
if ("mover_ano_tract" %in% names(data)) {
  data <- data %>% mutate(mover_ano_tract = if_else(is.na(treat_B), NA_real_, mover_ano_tract))
}
if ("mover_ano_cep" %in% names(data)) {
  data <- data %>% mutate(mover_ano_cep = if_else(is.na(treat_B), NA_real_, mover_ano_cep))
}

# ============================================================================
# 3) NOVO: Relocation (Census Tract, t-1) a partir de code_tract
#    Lógica por estabelecimento:
#      diff_tract = 1 se code_tract != lag(code_tract)
#      reloc_tract_tminus1 = lead(diff_tract, 1)
# ============================================================================

if (!("code_tract" %in% names(data))) {
  stop("Variável 'code_tract' não existe na base. Não dá para construir relocation por census tract.")
}

data <- data %>%
  group_by(id_estab) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    ct      = as.character(code_tract),
    ct_lag  = dplyr::lag(ct, 1),
    diff_tract = if_else(!is.na(ct) & !is.na(ct_lag) & ct != ct_lag, 1, 0),
    reloc_tract_tminus1 = dplyr::lead(diff_tract, 1, default = 0)
  ) %>%
  ungroup() %>%
  mutate(
    reloc_tract_tminus1 = if_else(is.na(treat_B), NA_real_, as.numeric(reloc_tract_tminus1))
  ) %>%
  select(-ct, -ct_lag, -diff_tract)

# ============================================================================
# 4) Collapse igual ao Stata
#    preserve
#      gen firm = 1 if morte == 0
#      gen massa_salarial_medio = massa_salarial
#      collapse ..., by(treat_B year)
#      keep if inrange(year,2003,2012)
#      gen prop_* = */firm
#    restore
# ============================================================================

base_agregado <- data %>%
  mutate(
    firm                 = if_else(morte == 0, 1, NA_real_),
    massa_salarial_medio = massa_salarial
  ) %>%
  group_by(treat_B, year) %>%
  summarise(
    new_firm             = sum(new_firm,            na.rm = TRUE),
    firm                 = sum(firm,                na.rm = TRUE),
    morte                = sum(morte,               na.rm = TRUE),
    reloc_tract_tminus1  = sum(reloc_tract_tminus1, na.rm = TRUE),
    empregados           = sum(empregados,          na.rm = TRUE),
    massa_salarial       = sum(massa_salarial,      na.rm = TRUE),
    massa_salarial_medio = mean(massa_salarial_medio, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(dplyr::between(year, 2003, 2012)) %>%
  mutate(
    prop_morte = morte / firm,
    prop_new   = new_firm / firm,
    prop_mov   = reloc_tract_tminus1 / firm
  ) %>%
  # ajusta para os quatro outcomes que vamos plotar
  mutate(
    morte               = prop_morte * 100,  # Closure rate (%)
    reloc_tract_tminus1 = prop_mov   * 100,  # Relocation rate (%)
    empregados          = empregados / firm, # Mean employees
    massa_salarial      = massa_salarial / firm  # Mean payroll
  ) %>%
  tidyr::pivot_longer(
    cols = c(morte, reloc_tract_tminus1, empregados, massa_salarial),
    names_to  = "variable",
    values_to = "valor"
  ) %>%
  mutate(
    # ATENÇÃO: cores pedidas -> Control vermelho, Treat azul
    treat = case_when(
      treat_B == 1 ~ "Treat",
      treat_B == 0 ~ "Control",
      TRUE         ~ "Others"
    ),
    variable = case_when(
      variable == "morte"               ~ "A - Closure Rate (%)",
      variable == "reloc_tract_tminus1" ~ "B - Relocation Rate (%)",
      variable == "empregados"          ~ "C - Mean number of Employees",
      variable == "massa_salarial"      ~ "D - Mean Payroll (in BRL)",
      TRUE                              ~ NA_character_
    )
  ) %>%
  filter(treat %in% c("Treat", "Control"),
         !is.na(variable)) %>%
  arrange(variable)

# ordem fixa dos painéis
vars_ordem <- c(
  "A - Closure Rate (%)",
  "B - Relocation Rate (Census Tract, t-1) (%)",
  "C - Mean number of Employees",
  "D - Mean Payroll (in BRL)"
)

# ============================================================================
# 5) Função de plot: todos os anos no eixo X
# ============================================================================

create_plot <- function(data, var_name) {
  data %>%
    filter(variable == var_name) %>%
    ggplot() +
    geom_rect(
      aes(xmin = 2007 - 0.0001, xmax = 2007 + 0.0001,
          ymin = -Inf, ymax = Inf),
      fill = "grey60", alpha = 0.25
    ) +
    geom_vline(xintercept = 2007 - 0.0001,
               linetype = "dashed", color = "firebrick") +
    geom_vline(xintercept = 2007 + 0.0001,
               linetype = "dashed", color = "firebrick") +
    geom_line(aes(x = year, y = valor, colour = treat)) +
    geom_point(aes(x = year, y = valor, colour = treat),
               shape = 23, fill = "white") +
    # aqui eu INVERTO as cores: Control vermelho, Treat azul
    scale_color_manual(
      values = c("Control" = "coral3", "Treat" = "cornflowerblue"),
      breaks = c("Control", "Treat"),
      name   = ""
    ) +
    scale_x_continuous(
      breaks = 2003:2012,
      limits = c(2003, 2012)
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(
      title = var_name,
      x     = "Year",
      y     = "Coefficient"
    )
}

plots <- lapply(vars_ordem, function(v) create_plot(base_agregado, v))

# ============================================================================
# 6) Combinar em 2x2 com legenda única embaixo
# ============================================================================

combined_plot <- ggpubr::ggarrange(
  plotlist      = plots,
  nrow          = 2,
  ncol          = 2,
  common.legend = TRUE,
  legend        = "bottom"
)


ggsave(
  "./results/analysis/Fig_02_Evolution_of_Aggregate_Outcomes_4panels_Tract_tminus1.png",
  combined_plot,
  dpi    = 300,
  width  = 14,
  height = 9,
  units  = "in"
)
