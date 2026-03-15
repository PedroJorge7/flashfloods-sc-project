# ============================================================================
# Figure B.3: Establishment-Level Estimates Using Alternative Inference Procedures
# NOVA DEFINIÃ‡ÃƒO: Relocation = Census Tract, t-1 (reloc_tract_tminus1 via code_tract)
# - inclui lead()
# - inclui correÃ§Ã£o do 1Âº ano do id_estab
# - inclui regra: se morte Ã© NA => relocation NA
# - FE tendÃªncia: treat_trend_f[code_tract_num]  (ordem correta no fixest)
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
# 0) Output dir
# ---------------------------------------------------------
dir.create("./results/analysis", recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# 1) Carregar base correta (com coordenadas)
# ---------------------------------------------------------
data <- haven::read_dta("./data/firm_coordinates.dta") %>%
  mutate(
    lat = Lat,
    lon = Lon
  ) %>%
  filter(year >= 2003 & year <= 2012) %>%
  arrange(id_estab, year)

stopifnot(all(c("id_estab","year","dist_flood","morte","mover_ano_mun","code_tract","lat","lon") %in% names(data)))

# se tiver coordenadas faltando, Conley pode falhar -> corta
data <- data %>%
  filter(!is.na(lat), !is.na(lon))

# ---------------------------------------------------------
# 2) Tratamento (igual Table 3 / Stata) + variÃ¡veis base
# ---------------------------------------------------------
data <- data %>%
  arrange(id_estab, year) %>%
  group_by(id_estab) %>%
  mutate(
    # tratamento principal (0â€“12.5 km vs 50â€“80 km)
    treat_B = case_when(
      dist_flood <= 12.5 ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE ~ NA_real_
    ),
    # guardar originais
    morte_orig         = morte,
    mover_ano_mun_orig = mover_ano_mun
  ) %>%
  mutate(
    # garantir numÃ©rico (evita haven_labelled)
    morte         = as.numeric(haven::zap_labels(morte)),
    mover_ano_mun = as.numeric(haven::zap_labels(mover_ano_mun)),
    mover_raw     = mover_ano_mun,  # usado no forward fill do treat_B (como Stata)
    # outcomes = NA onde treat_B Ã© missing
    morte = if_else(is.na(treat_B), NA_real_, morte)
  ) %>%
  # forward fill do treat_B usando mover_ano_mun "bruto" (como Stata)
  mutate(
    treat_B = {
      tb  <- treat_B
      mov <- mover_raw
      for (i in seq_along(tb)) {
        if (i > 1 && is.na(tb[i]) && !is.na(mov[i]) && mov[i] == 1) {
          tb[i] <- tb[i - 1]
        }
      }
      tb
    }
  ) %>%
  # mover_ano_mun = NA onde treat_B segue NA (como Stata)
  mutate(
    mover_ano_mun = if_else(is.na(treat_B), NA_real_, mover_ano_mun)
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 2.1) NOVA relocation: Census Tract, t-1 (via code_tract)
#      reloc_tract_tminus1(t) = lead( 1[ct(t) != ct(t-1)], 1 )
#      + seu ajuste obrigatÃ³rio no 1Âº ano do id_estab
#      + regra alinhada: se morte NA => relocation NA
# ---------------------------------------------------------
data <- data %>%
  group_by(id_estab) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    ct     = as.character(code_tract),
    ct_lag = dplyr::lag(ct, 1),
    diff_tract = dplyr::case_when(
      is.na(ct) | is.na(ct_lag) ~ NA_real_,
      ct != ct_lag              ~ 1,
      TRUE                      ~ 0
    ),
    reloc_tract_tminus1 = dplyr::lead(diff_tract, 1)
  ) %>%
  ungroup() %>%
  select(-ct, -ct_lag, -diff_tract)

# aplica corte por treat_B (igual teu padrÃ£o) + correÃ§Ã£o 1Âº ano + regra morte->NA
data <- data %>%
  mutate(
    reloc_tract_tminus1 = if_else(is.na(treat_B), NA_real_, as.numeric(reloc_tract_tminus1))
  ) %>%
  group_by(id_estab) %>%
  mutate(
    reloc_tract_tminus1 = ifelse(
      year == min(year, na.rm = TRUE) & reloc_tract_tminus1 == 1,
      0,
      reloc_tract_tminus1
    )
  ) %>%
  ungroup() %>%
  mutate(
    reloc_tract_tminus1 = if_else(is.na(reloc_tract_tminus1),0,reloc_tract_tminus1),
    reloc_tract_tminus1 = if_else(is.na(morte), NA_real_, reloc_tract_tminus1),
  )

# ---------------------------------------------------------
# 2.2) Trend FE (ordem correta no fixest): treat_trend_f[code_tract_num]
# ---------------------------------------------------------
data <- data %>%
  mutate(
    treat_trend    = if_else(year >= 2008, 1, 0),
    treat_trend_f  = factor(treat_trend),
    code_tract_num = suppressWarnings(as.numeric(as.character(code_tract)))
  )

# ---------------------------------------------------------
# 2.3) Dummies anuais 2008â€“2012 + dummy agregada pÃ³s-choque
# ---------------------------------------------------------
for (y in 2008:2012) {
  var <- paste0("treat_B_", y)
  data[[var]] <- dplyr::case_when(
    data$year == y & !is.na(data$treat_B) ~ data$treat_B,
    !is.na(data$treat_B) & data$year != y ~ 0,
    TRUE                                  ~ NA_real_
  )
}

data <- data %>%
  mutate(
    treat_B_agg = case_when(
      year >= 2008 & treat_B == 1 ~ 1,
      !is.na(treat_B)             ~ 0,
      TRUE                        ~ NA_real_
    )
  )

# ---------------------------------------------------------
# 3) EspecificaÃ§Ãµes de inferÃªncia
# ---------------------------------------------------------
outcomes <- c("morte", "reloc_tract_tminus1")

inference_specs <- list(
  "Code tract"      = list(type = "cluster", cluster = ~ code_tract),
  "Conley: 10 km"   = list(type = "conley",  dist = 10),
  "Conley: 15 km"   = list(type = "conley",  dist = 15),
  "Conley: 20 km"   = list(type = "conley",  dist = 20)
)

# ---------------------------------------------------------
# 4) Rodar regressÃµes (Post + dummies anuais) para cada inferÃªncia
#    FE: id_estab + year + treat_trend_f[code_tract_num]
# ---------------------------------------------------------
all_results <- data.frame()

for (inf_name in names(inference_specs)) {
  
  spec <- inference_specs[[inf_name]]
  
  for (outcome in outcomes) {
    
    # ---------- (a) Efeito agregado: Post ----------
    fml_agg <- as.formula(
      paste0(outcome,
             " ~ treat_B_agg | id_estab + year + treat_trend_f[code_tract_num]")
    )
    
    if (spec$type == "cluster") {
      m_agg <- feols(
        fml_agg,
        data    = data,
        cluster = spec$cluster,
        lean    = TRUE
      )
    } else {
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
    
    # ---------- (b) Dummies 2008â€“2012 ----------
    treat_vars <- paste0("treat_B_", 2008:2012)
    fml_evt <- as.formula(
      paste0(outcome, " ~ ",
             paste(treat_vars, collapse = " + "),
             " | id_estab + year + treat_trend_f[code_tract_num]")
    )
    
    if (spec$type == "cluster") {
      m_evt <- feols(
        fml_evt,
        data    = data,
        cluster = spec$cluster,
        lean    = TRUE
      )
    } else {
      m_evt <- feols(
        fml_evt,
        data = data,
        vcov = conley(spec$dist),
        lean = TRUE
      )
    }
    
    evt_rows <- broom::tidy(m_evt) %>%
      dplyr::filter(grepl("^treat_B_", term)) %>%
      mutate(Period = gsub("treat_B_", "", term)) %>%
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
# 5) Preparar dados e painÃ©is A/B
# ---------------------------------------------------------
all_results <- all_results %>%
  mutate(
    Outcome = factor(
      Outcome,
      levels = c("morte", "reloc_tract_tminus1"),
      labels = c("Closure", "Relocation (Census Tract, t-1)")
    ),
    Period = factor(
      Period,
      levels = c("Post", "2008", "2009", "2010", "2011", "2012")
    ),
    Inference = factor(
      Inference,
      levels = c("Code tract", "Conley: 10 km", "Conley: 15 km", "Conley: 20 km")
    )
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
  df = dplyr::filter(all_results, Outcome == "Relocation (Census Tract, t-1)"),
  title_label = "B - Establishment Relocation"
)

# ---------------------------------------------------------
# 6) Figura final com legenda Ãºnica embaixo
# ---------------------------------------------------------
fig_inf <- ggpubr::ggarrange(
  pA, pB,
  ncol          = 2,
  common.legend = TRUE,
  legend        = "bottom"
)

# ---------------------------------------------------------
# 7) Salvar figura
# ---------------------------------------------------------
ggsave(
  filename = "./results/analysis/change_standard_deviation.png",
  plot     = fig_inf,
  dpi      = 300,
  width    = 12,
  height   = 6,
  units    = "in"
)
