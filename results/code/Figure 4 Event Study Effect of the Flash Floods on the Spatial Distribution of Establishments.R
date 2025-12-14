############################################################
## Figure 4 – Event Study: Effect of the Flash Floods
##             on the Spatial Distribution of Establishments
##
## UPDATED: Relocation = Census Tract, t-1 (via code_tract)
## - Replica o tratamento (treat_B) do Figure 3 original
## - Constrói reloc_tract_tminus1 via lag/lead em code_tract
## - Usa FE: id_estab + year + treat_trend[code_tract]
## - Eixo Y definido pelos ICs (min/max) + folga
############################################################

rm(list = ls())

library(dplyr)
library(tidyr)
library(haven)
library(fixest)
library(ggplot2)
library(ggpubr)
library(broom)

# ---------------------------------------------------------
# 1) Carregar base – NÃO cortar em 2012 antes de construir t-1
# ---------------------------------------------------------

data <- haven::read_dta("./data/Natural Disastrer Santa Catarina - Dataset.dta") %>%
  filter(year >= 2003) %>%
  arrange(id_estab, year)

# ---------------------------------------------------------
# 2) Replicar construção de tratamento (igual ao Figure 3 / Stata)
# ---------------------------------------------------------

data <- data %>%
  arrange(id_estab, year) %>%
  group_by(id_estab) %>%
  mutate(
    treat_B = dplyr::case_when(
      dist_flood <= 12.5 ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE ~ NA_real_
    ),
    morte_orig         = morte,
    new_firm_orig      = new_firm,
    mover_ano_mun_orig = mover_ano_mun
  ) %>%
  mutate(
    morte    = if_else(is.na(treat_B), NA_real_, morte),
    new_firm = if_else(is.na(treat_B), NA_real_, new_firm)
  ) %>%
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
  mutate(
    mover_ano_mun = if_else(is.na(treat_B), NA_real_, mover_ano_mun)
  ) %>%
  ungroup()

# Se existirem essas variáveis na base, aplica o mesmo corte
if ("mover_ano_tract" %in% names(data)) {
  data <- data %>%
    mutate(mover_ano_tract = if_else(is.na(treat_B), NA_real_, mover_ano_tract))
}
if ("mover_ano_cep" %in% names(data)) {
  data <- data %>%
    mutate(mover_ano_cep = if_else(is.na(treat_B), NA_real_, mover_ano_cep))
}

# Tendência pós-choque (igual ao Stata: year >= 2008)
data <- data %>%
  mutate(treat_trend = if_else(year >= 2008, 1, 0))

# ---------------------------------------------------------
# 3) Construir reloc_tract_tminus1 via code_tract (Census Tract, t-1)
# ---------------------------------------------------------

if (!("code_tract" %in% names(data))) {
  stop("Variável 'code_tract' não existe na base. Não dá para construir relocation por census tract.")
}

data <- data %>%
  group_by(id_estab) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    ct         = as.character(code_tract),
    ct_lag     = dplyr::lag(ct, 1),
    diff_tract = if_else(!is.na(ct) & !is.na(ct_lag) & ct != ct_lag, 1, 0),
    reloc_tract_tminus1 = dplyr::lead(diff_tract, 1, default = 0)
  ) %>%
  ungroup() %>%
  mutate(
    reloc_tract_tminus1 = if_else(is.na(treat_B), NA_real_, as.numeric(reloc_tract_tminus1))
  ) %>%
  select(-ct, -ct_lag, -diff_tract)

# ---------------------------------------------------------
# 4) Amostra 2003–2012 com treat_B definido
# ---------------------------------------------------------

data_es <- data %>%
  filter(year >= 2003 & year <= 2012, !is.na(treat_B))

# Opcional: alinhar relocation com o suporte de morte (igual ao seu script novo)
data_es$reloc_tract_tminus1 <- ifelse(is.na(data_es$morte),
                                      NA,
                                      data_es$reloc_tract_tminus1)

# ---------------------------------------------------------
# 5) Dummies ano-específicas de tratamento (baseline = 2007)
# ---------------------------------------------------------

for (y in 2003:2012) {
  var <- paste0("treat_B_", y)
  data_es[[var]] <- dplyr::case_when(
    data_es$year == y & !is.na(data_es$treat_B) ~ data_es$treat_B,
    !is.na(data_es$treat_B) & data_es$year != y ~ 0,
    TRUE                                        ~ NA_real_
  )
}

treat_years <- c(2003:2006, 2008:2012)

# ---------------------------------------------------------
# 6) Estimar event study (Closure + Relocation Tract t-1)
# ---------------------------------------------------------

outcomes <- c("morte", "reloc_tract_tminus1")
outcome_labels <- c(
  "morte"               = "A - Establishment Closure (0/1)",
  "reloc_tract_tminus1" = "B - Establishment Relocation (0/1)"
)

all_coefs <- data.frame()

for (outcome in outcomes) {
  
  lab <- outcome_labels[[outcome]]
  y   <- data_es[[outcome]]
  y_non_na <- y[!is.na(y)]
  
  if (length(y_non_na) < 2 || length(unique(y_non_na)) <= 1) {
    message("Outcome '", outcome, "' é constante ou quase; pulando.")
    next
  }
  
  treat_vars_str <- paste0("treat_B_", treat_years, collapse = " + ")
  formula_str    <- paste(outcome, "~", treat_vars_str)
  
  fml <- as.formula(paste0(
    formula_str,
    " | id_estab + year + treat_trend[code_tract]"
  ))
  
  model <- feols(
    fml,
    data    = data_es,
    cluster = ~ id_estab + year,
    lean    = TRUE
  )
  
  coef_summary <- broom::tidy(model, conf.int = TRUE)
  
  event_coefs <- coef_summary %>%
    dplyr::filter(grepl("^treat_B_", term)) %>%
    mutate(
      year       = as.numeric(gsub("treat_B_", "", term)),
      parmseq    = year - 2007,
      Regression = lab,
      IC         = "95%"
    ) %>%
    dplyr::select(Regression, parmseq, estimate,
                  min = conf.low, max = conf.high, IC)
  
  baseline_row <- data.frame(
    Regression = lab,
    parmseq    = 0,
    estimate   = 0,
    min        = 0,
    max        = 0,
    IC         = "95%",
    stringsAsFactors = FALSE
  )
  
  event_coefs <- dplyr::bind_rows(event_coefs, baseline_row)
  all_coefs   <- dplyr::bind_rows(all_coefs, event_coefs)
}

if (nrow(all_coefs) == 0) {
  stop("Nenhum outcome com variação suficiente para estimar o event study.")
}

output <- all_coefs %>% arrange(Regression, parmseq)

# ---------------------------------------------------------
# 7) Limites do eixo Y a partir dos ICs (min/max) + folga
# ---------------------------------------------------------

y_min   <- min(output$min, na.rm = TRUE)
y_max   <- max(output$max, na.rm = TRUE)
y_range <- y_max - y_min

pad <- if (is.finite(y_range) && y_range > 0) 0.05 * y_range else 0
y_limits <- c(y_min - pad, y_max + pad)

# ---------------------------------------------------------
# 8) Plot com a estética do Figure 3 + y dinâmico
# ---------------------------------------------------------

event_study_plot <- function(reg_name) {
  output %>%
    dplyr::filter(Regression == reg_name) %>%
    ggplot(aes(x = parmseq, y = estimate)) +
    geom_ribbon(aes(ymax = max, ymin = min),
                fill = "grey90", alpha = 0.5) +
    geom_line(aes(parmseq, max), color = "grey30", size = 0.1) +
    geom_line(aes(parmseq, min), color = "grey30", size = 0.1) +
    geom_point(size = 2, color = "firebrick", fill = "black") +
    geom_line(color = "firebrick", size = 1) +
    scale_x_continuous(breaks = -4:5, labels = 2003:2012) +
    scale_y_continuous(limits = y_limits) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    labs(
      x = "Years",
      y = "Coefficient",
      title = reg_name
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

reg_names  <- unique(output$Regression)
plots_list <- lapply(reg_names, event_study_plot)

fig_event <- ggpubr::ggarrange(
  plotlist      = plots_list,
  nrow          = 1,
  ncol          = length(plots_list),
  common.legend = FALSE
)

ggsave(
  filename = "./establishments/analysis/Fig_04_Event_Study_Relocation_Tract_tminus1_dynamicY.png",
  plot     = fig_event,
  dpi      = 300,
  width    = 30 * length(plots_list) / 2,
  height   = 15,
  units    = "cm"
)
