# ============================================================================
# Figure 7: Alternative Inference Procedures – Establishment Level (R version)
# ============================================================================

rm(list = ls())

library(dplyr)
library(tidyr)
library(haven)
library(fixest)
library(ggplot2)
library(broom)
library(ggpubr)

# ---------------------------------------------------------
# 1) Carregar base correta (com coordenadas)
# ---------------------------------------------------------

data <- haven::read_dta("./data/firm_coordinates.dta") %>%
  mutate(
    # garantir nomes lat/lon que o fixest reconhece
    lat = Lat,
    lon = Lon
  ) %>%
  filter(year >= 2003 & year <= 2012) %>%
  arrange(id_estab, year)

# ---------------------------------------------------------
# 2) Garantir estrutura de tratamento (igual tabela/figuras anteriores)
#    (se já estiver na base, isso só sobrescreve de forma consistente)
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
    morte_orig         = morte,
    new_firm_orig      = new_firm,
    mover_ano_mun_orig = mover_ano_mun
  ) %>%
  # outcomes = NA onde treat_B é missing
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
  # relocação = NA onde treat_B segue NA
  mutate(
    mover_ano_mun = if_else(is.na(treat_B), NA_real_, mover_ano_mun)
  ) %>%
  ungroup() %>%
  mutate(
    treat_trend = if_else(year >= 2008, 1, 0)
  )

# dummies anuais 2008–2012 (se já existirem, serão refeitas de forma idêntica)
for (y in 2008:2012) {
  var <- paste0("treat_B_", y)
  data[[var]] <- dplyr::case_when(
    data$year == y & !is.na(data$treat_B) ~ data$treat_B,
    !is.na(data$treat_B) & data$year != y ~ 0,
    TRUE                                  ~ NA_real_
  )
}

# dummy agregada pós-choque
data <- data %>%
  mutate(
    treat_B_agg = case_when(
      year >= 2008 & treat_B == 1 ~ 1,
      !is.na(treat_B)             ~ 0,
      TRUE                        ~ NA_real_
    )
  )

# ---------------------------------------------------------
# 3) Especificações de inferência
# ---------------------------------------------------------

# Só dois outcomes
outcomes <- c("morte", "mover_ano_mun")

inference_specs <- list(
  "Code tract"      = list(type = "cluster", cluster = ~ code_tract),
  "Conley: 10 km"   = list(type = "conley",  dist = 10),
  "Conley: 15 km"   = list(type = "conley",  dist = 15),
  "Conley: 20 km"   = list(type = "conley",  dist = 20)
)

# ---------------------------------------------------------
# 4) Rodar regressões (Post + dummies anuais) para cada inferência
# ---------------------------------------------------------

all_results <- data.frame()

for (inf_name in names(inference_specs)) {
  
  spec <- inference_specs[[inf_name]]
  
  for (outcome in outcomes) {
    
    # ---------- (a) Efeito agregado: Post ----------
    fml_agg <- as.formula(
      paste0(outcome,
             " ~ treat_B_agg | id_estab + year + treat_trend[code_tract]")
    )
    
    if (spec$type == "cluster") {
      m_agg <- feols(
        fml_agg,
        data    = data,
        cluster = spec$cluster,
        lean    = TRUE
      )
    } else if (spec$type == "conley") {
      m_agg <- feols(
        fml_agg,
        data = data,
        vcov = conley(spec$dist),
        lean = TRUE
      )
    }
    
    agg_row <- broom::tidy(m_agg) %>%
      dplyr::filter(term == "treat_B_agg") %>%
      transmute(
        Inference   = inf_name,
        Outcome     = outcome,
        Period      = "Post",
        Coefficient = estimate,
        SE          = std.error,
        CI_Low      = estimate - 1.96 * std.error,
        CI_High     = estimate + 1.96 * std.error
      )
    
    # ---------- (b) Dummies 2008–2012 ----------
    treat_vars <- paste0("treat_B_", 2008:2012)
    fml_evt <- as.formula(
      paste0(outcome, " ~ ",
             paste(treat_vars, collapse = " + "),
             " | id_estab + year + treat_trend[code_tract]")
    )
    
    if (spec$type == "cluster") {
      m_evt <- feols(
        fml_evt,
        data    = data,
        cluster = spec$cluster,
        lean    = TRUE
      )
    } else if (spec$type == "conley") {
      m_evt <- feols(
        fml_evt,
        data = data,
        vcov = conley(spec$dist),
        lean = TRUE
      )
    }
    
    evt_rows <- broom::tidy(m_evt) %>%
      dplyr::filter(grepl("^treat_B_", term)) %>%
      mutate(
        Period = gsub("treat_B_", "", term)
      ) %>%
      transmute(
        Inference   = inf_name,
        Outcome     = outcome,
        Period      = Period,
        Coefficient = estimate,
        SE          = std.error,
        CI_Low      = estimate - 1.96 * std.error,
        CI_High     = estimate + 1.96 * std.error
      )
    
    all_results <- bind_rows(all_results, agg_row, evt_rows)
  }
}

# ---------------------------------------------------------
# 5) Preparar dados e painéis A/B
# ---------------------------------------------------------

all_results <- all_results %>%
  mutate(
    Outcome      = factor(Outcome,
                          levels = c("morte", "mover_ano_mun"),
                          labels = c("Closure", "Relocation")),
    Period       = factor(Period,
                          levels = c("Post", "2008", "2009", "2010", "2011", "2012")),
    Inference    = factor(Inference,
                          levels = c("Code tract",
                                     "Conley: 10 km",
                                     "Conley: 15 km",
                                     "Conley: 20 km"))
  )

inf_colors <- c(
  "Code tract"      = "#FEE0D2",
  "Conley: 10 km"   = "#FCBBA1",
  "Conley: 15 km"   = "#FB6A4A",
  "Conley: 20 km"   = "#CB181D"
)

pos_dodge <- position_dodge(width = 0.4)

make_panel <- function(df, title_label) {
  ggplot(df, aes(x = Period, y = Coefficient, color = Inference)) +
    geom_point(position = pos_dodge, size = 2) +
    geom_errorbar(
      aes(ymin = CI_Low, ymax = CI_High),
      position = pos_dodge,
      width = 0.2
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_discrete(drop = FALSE) +
    scale_color_manual(values = inf_colors, name = "Inference Procedure") +
    labs(
      x = "Year",
      y = "Coefficient",
      title = title_label
    ) +
    theme_bw()
}

pA <- make_panel(
  df = dplyr::filter(all_results, Outcome == "Closure"),
  title_label = "A - Establishment Closure (0/1)"
)

pB <- make_panel(
  df = dplyr::filter(all_results, Outcome == "Relocation"),
  title_label = "B - Establishment Relocation (0/1)"
)

# ---------------------------------------------------------
# 6) Figura final com legenda única embaixo
# ---------------------------------------------------------

fig_inf <- ggpubr::ggarrange(
  pA, pB,
  ncol          = 2,
  common.legend = TRUE,
  legend        = "bottom"
)

# ---------------------------------------------------------
# 7) Salvar figura e coeficientes
# ---------------------------------------------------------

ggsave(
  filename = "establishments/analysis/Fig_07_Alternative_Inference_Procedures.png",
  plot     = fig_inf,
  dpi      = 300,
  width    = 12,
  height   = 6,
  units    = "in"
)

