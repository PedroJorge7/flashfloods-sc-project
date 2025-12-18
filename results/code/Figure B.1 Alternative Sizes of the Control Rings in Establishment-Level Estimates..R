# ============================================================================
# Figure 5: Alternative Control Rings - Establishment Level (R version)
#   AJUSTE: Relocation = reloc_tract_tminus1 (Census Tract, t-1 via code_tract)
#   IMPORTANTE: reloc_tract_tminus1 é construído ANTES do corte 2003–2012
#   CONTROL RINGS (fixos):
#     40-70, 50-80, 30-80, 50-100
#   TESTES: diagnóstico por anel (N obs, N firmas, shares, etc.)
#   CLUSTER: 2-way (id_estab + year) em TODOS os modelos
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
# Output dir
# ---------------------------------------------------------
dir.create("results/analysis", recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------
# Helper: code_tract -> num (p/ varying slopes)
# ---------------------------------------------------------
make_tract_num <- function(x) {
  x_chr <- as.character(x)
  x_chr[x_chr %in% c("", "NA")] <- NA_character_
  
  x_num <- suppressWarnings(as.numeric(x_chr))
  bad   <- !is.na(x_chr) & is.na(x_num)
  
  if (sum(!is.na(x_chr)) > 0 && mean(bad) > 0.2) {
    x_num <- as.numeric(factor(x_chr))
  }
  x_num
}

# ---------------------------------------------------------
# TEST helpers
# ---------------------------------------------------------
assert_has_cols <- function(df, cols) {
  miss <- setdiff(cols, names(df))
  if (length(miss) > 0) stop("FALTAM COLUNAS NA BASE: ", paste(miss, collapse = ", "))
  invisible(TRUE)
}

quick_binary_check <- function(x, name) {
  ok <- all(is.na(x) | x %in% c(0, 1))
  if (!ok) stop("VARIÁVEL NÃO BINÁRIA: ", name, " (esperado 0/1/NA)")
  invisible(TRUE)
}

validate_rings <- function(control_specs) {
  nm <- names(control_specs)
  if (anyDuplicated(nm)) stop("RINGS DUPLICADOS EM control_specs.")
  for (k in nm) {
    lo <- control_specs[[k]]$lower
    up <- control_specs[[k]]$upper
    if (is.null(lo) || is.null(up)) stop("Ring sem lower/upper: ", k)
    if (!is.numeric(lo) || !is.numeric(up)) stop("Ring lower/upper não numérico: ", k)
    if (!(lo < up)) stop("Ring inválido (lower >= upper): ", k)
  }
  invisible(TRUE)
}

ring_diagnostics <- function(df, ring_label) {
  df2 <- df %>% filter(!is.na(treat_B_temp))
  tibble(
    Control_Ring = ring_label,
    n_obs        = nrow(df2),
    n_firms      = n_distinct(df2$id_estab),
    share_treat  = ifelse(nrow(df2) > 0, mean(df2$treat_B_temp == 1), NA_real_),
    share_ctrl   = ifelse(nrow(df2) > 0, mean(df2$treat_B_temp == 0), NA_real_),
    years_min    = ifelse(nrow(df2) > 0, min(df2$year), NA_real_),
    years_max    = ifelse(nrow(df2) > 0, max(df2$year), NA_real_)
  )
}

# ---------------------------------------------------------
# 0) CONTROL RINGS (AJUSTADO: 60-80 -> 30-80)
# ---------------------------------------------------------
control_specs <- list(
  "40-70 km"  = list(lower = 40, upper = 70),
  "30-80 km"  = list(lower = 30, upper = 80),
  "50-100 km" = list(lower = 50, upper = 100)
)
validate_rings(control_specs)

outcomes <- c("morte", "reloc_tract_tminus1")

# thresholds para não rodar anel sem massa
MIN_FIRMS <- 200
MIN_OBS   <- 2000

# ---------------------------------------------------------
# 1) Carregar base (SEM cortar ainda)
# ---------------------------------------------------------
data <- haven::read_dta("./data/Natural Disastrer Santa Catarina - Dataset.dta") %>%
  arrange(id_estab, year)

assert_has_cols(data, c("id_estab","year","dist_flood","morte","mover_ano_mun","code_tract"))

# ---------------------------------------------------------
# 1.1) Construir relocation correto ANTES do corte:
#      reloc_tract_tminus1(t) = 1 se muda entre t e t+1 (via code_tract)
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

quick_binary_check(data$reloc_tract_tminus1, "reloc_tract_tminus1 (raw, pre-cut)")

# ---------------------------------------------------------
# 2) AGORA sim: manter somente 2003–2012
# ---------------------------------------------------------
data <- data %>%
  filter(year >= 2003 & year <= 2012) %>%
  arrange(id_estab, year)

# ---------------------------------------------------------
# 3) Construção-base (guardar originais + tendência + tract num)
# ---------------------------------------------------------
data <- data %>%
  group_by(id_estab) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    treat_B = case_when(
      dist_flood <= 12.5 ~ 1,
      dist_flood >= 50 & dist_flood <= 80 ~ 0,
      TRUE ~ NA_real_
    ),
    
    morte_orig               = morte,
    mover_ano_mun_orig       = mover_ano_mun,
    reloc_tract_tminus1_orig = reloc_tract_tminus1
  ) %>%
  mutate(
    morte = if_else(is.na(treat_B), NA_real_, morte)
  ) %>%
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
  mutate(
    reloc_tract_tminus1 = if_else(is.na(treat_B), NA_real_, reloc_tract_tminus1),
    reloc_tract_tminus1 = if_else(!is.na(treat_B) & is.na(reloc_tract_tminus1), 0, reloc_tract_tminus1),
    reloc_tract_tminus1 = if_else(is.na(morte), NA_real_, reloc_tract_tminus1)
  ) %>%
  ungroup() %>%
  mutate(
    treat_trend    = if_else(year >= 2008, 1, 0),
    code_tract_num = make_tract_num(code_tract)
  )

quick_binary_check(data$morte, "morte (in-sample)")
quick_binary_check(data$reloc_tract_tminus1, "reloc_tract_tminus1 (in-sample)")

# ---------------------------------------------------------
# 4) Loop sobre anéis e outcomes
# ---------------------------------------------------------
all_results <- data.frame()
diag_table  <- data.frame()

for (ring in names(control_specs)) {
  
  spec    <- control_specs[[ring]]
  lower_c <- spec$lower
  upper_c <- spec$upper
  
  temp_data <- data %>%
    mutate(
      treat_B_temp = case_when(
        dist_flood <= 12.5 ~ 1,
        dist_flood >= lower_c & dist_flood <= upper_c ~ 0,
        TRUE ~ NA_real_
      ),
      morte_temp               = morte_orig,
      mover_ano_mun_temp       = mover_ano_mun_orig,
      reloc_tract_tminus1_temp = reloc_tract_tminus1_orig
    ) %>%
    group_by(id_estab) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
      morte_temp               = if_else(is.na(treat_B_temp), NA_real_, morte_temp),
      reloc_tract_tminus1_temp = if_else(is.na(treat_B_temp), NA_real_, reloc_tract_tminus1_temp),
      
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
      
      morte_temp               = if_else(is.na(treat_B_temp), NA_real_, morte_temp),
      reloc_tract_tminus1_temp = if_else(is.na(treat_B_temp), NA_real_, reloc_tract_tminus1_temp)
    ) %>%
    ungroup() %>%
    mutate(
      reloc_tract_tminus1_temp = if_else(
        !is.na(treat_B_temp) & is.na(reloc_tract_tminus1_temp),
        0, reloc_tract_tminus1_temp
      ),
      reloc_tract_tminus1_temp = if_else(is.na(morte_temp), NA_real_, reloc_tract_tminus1_temp)
    )
  
  d0 <- ring_diagnostics(temp_data, ring)
  diag_table <- bind_rows(diag_table, d0)
  
  if (is.na(d0$n_firms) || d0$n_firms < MIN_FIRMS || d0$n_obs < MIN_OBS ||
      is.na(d0$share_treat) || is.na(d0$share_ctrl) ||
      d0$share_treat == 0 || d0$share_ctrl == 0) {
    message(sprintf(
      "[SKIP] %s: n_firms=%s, n_obs=%s, share_treat=%s, share_ctrl=%s",
      ring, d0$n_firms, d0$n_obs,
      round(d0$share_treat, 4), round(d0$share_ctrl, 4)
    ))
    next
  } else {
    message(sprintf(
      "[OK] %s: n_firms=%s, n_obs=%s, share_treat=%.3f, share_ctrl=%.3f",
      ring, d0$n_firms, d0$n_obs, d0$share_treat, d0$share_ctrl
    ))
  }
  
  # dummies 2008–2012 + agregado
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
  
  for (outcome in outcomes) {
    
    yvar <- paste0(outcome, "_temp")
    
    # (1) Post agregado — cluster 2-way
    fml_agg <- as.formula(
      paste0(yvar, " ~ treat_B_agg_temp | id_estab + year + treat_trend[code_tract_num]")
    )
    m_agg <- feols(
      fml_agg,
      data    = temp_data,
      cluster = ~ id_estab + year,
      lean    = TRUE
    )
    
    agg_row <- broom::tidy(m_agg) %>%
      filter(term == "treat_B_agg_temp") %>%
      transmute(
        Control_Ring = ring,
        Outcome      = outcome,
        Period       = "Post",
        Coefficient  = estimate,
        SE           = std.error,
        CI_Low       = estimate - 1.96 * std.error,
        CI_High      = estimate + 1.96 * std.error,
        nobs         = nobs(m_agg)
      )
    
    # (2) Evento 2008–2012 — cluster 2-way
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
      filter(grepl("^treat_B_temp_", term)) %>%
      mutate(Period = gsub("treat_B_temp_", "", term)) %>%
      transmute(
        Control_Ring = ring,
        Outcome      = outcome,
        Period       = Period,
        Coefficient  = estimate,
        SE           = std.error,
        CI_Low       = estimate - 1.96 * std.error,
        CI_High      = estimate + 1.96 * std.error,
        nobs         = nobs(m_evt)
      )
    
    all_results <- bind_rows(all_results, agg_row, evt_rows)
  }
}

# ---------------------------------------------------------
# 5) Salvar diagnósticos
# ---------------------------------------------------------
# write.csv(diag_table,  "results/analysis/control_rings_diagnostics.csv", row.names = FALSE)
# write.csv(all_results, "results/analysis/control_rings_coeffs.csv",      row.names = FALSE)

if (nrow(all_results) == 0) {
  stop("Nenhuma regressão rodou (todos os anéis caíram nos testes MIN_FIRMS/MIN_OBS). Veja diagnostics CSV.")
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
      levels = c("40-70 km", "30-80 km", "50-100 km")
    )
  )

ring_colors <- c(
  "40-70 km"  = "#FEE0D2",
  "30-80 km"  = "#FB6A4A",
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
  df = filter(all_results, Outcome == "Closure"),
  title_label = "A - Establishment Closure (0/1)"
) + theme(legend.position = "bottom")

pB <- make_panel(
  df = filter(all_results, Outcome == "Relocation"),
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

