# ============================================================================
# [New_Results, CORRECTED] Figure H.1 — Sensitivity Analysis of the
# HonestDiD Approach
# (Rambachan & Roth, 2023, relative-magnitudes bound). Tests how robust the
# average post-treatment effect is to violations of parallel trends, for
# (a) establishment closure and (b) worker employment. Treatment: 0-5 km vs.
# 50-80 km control throughout; clustering: census tract. Two panel images
# (closure, employment).
#
# Closure: refits the event-study model directly on `main_panel` (corrected,
# full-life panel).
#
# Employment: replicates the CORRECTED worker PSM + panel-construction
# pipeline (fixed 2007 baseline treat_B, no mover-year outcome nulling)
# independently, to get a raw fixest object with a full covariance matrix,
# since output_empregados() itself only returns tidied point estimates/SEs.
# Depends on `main_panel` (establishment) and `dados_main` + WORKER_MIN_TREAT
# etc. (worker) already being in scope.
# ============================================================================

library(HonestDiD)

log_msg("=== 01_figure_H1_honestdid_sensitivity.R: start ===")

if (!exists("dados_main", envir = .GlobalEnv)) {
  log_msg("dados_main not in scope; loading worker data...")
  library(MatchIt)
  source(file.path(script_dir, "utils", "worker_functions.R"))
  worker_path_h1 <- data_path("workers_clean_data.rds")
  dados_raw_h1 <- readRDS(worker_path_h1)
  dados_main             <<- dados_raw_h1 %>% filter(between(year, 2002, 2012))
  WORKER_MIN_TREAT       <<- 0
  WORKER_MAX_TREAT       <<- 5
  WORKER_MIN_CTRL        <<- 50
  WORKER_MAX_CTRL        <<- 80
  WORKER_CLUSTER_FORMULA <<- ~code_tract
  rm(dados_raw_h1)
}

year_from_term <- function(nm) as.numeric(gsub(".*?(\\d{4}).*", "\\1", nm))

make_honest_plot <- function(beta, sigma, numPrePeriods, numPostPeriods, context) {
  l_vec   <- rep(1 / numPostPeriods, numPostPeriods)
  Mbarvec <- seq(0.1, 1, by = 0.1)

  delta_rm <- createSensitivityResults_relativeMagnitudes(
    betahat = beta, sigma = sigma,
    numPrePeriods = numPrePeriods, numPostPeriods = numPostPeriods,
    Mbarvec = Mbarvec, l_vec = l_vec
  )
  original <- constructOriginalCS(
    betahat = beta, sigma = sigma,
    numPrePeriods = numPrePeriods, numPostPeriods = numPostPeriods,
    l_vec = l_vec
  )
  log_msg("HonestDiD (%s): original CS = [%.5f, %.5f]", context, original$lb, original$ub)
  robust_sig <- delta_rm[sign(delta_rm$lb) == sign(delta_rm$ub), ]
  breakdown_m <- if (nrow(robust_sig) > 0) max(robust_sig$Mbar) else NA_real_
  log_msg("HonestDiD (%s): largest Mbar with CI excluding zero = %s", context,
          if (is.na(breakdown_m)) "none (already crosses zero at smallest Mbar tested)" else sprintf("%.2f", breakdown_m))

  p <- createSensitivityPlot_relativeMagnitudes(delta_rm, original) +
    ggplot2::scale_color_manual(values = c("C-LF" = "steelblue", "Original" = "black")) +
    ggplot2::scale_x_continuous(breaks = c(0, Mbarvec)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none", plot.title = ggplot2::element_blank())
  list(plot = p, delta_rm = delta_rm, original = original)
}

# --------------------------------------------------------------------------
# (a) Establishment Closure -- pure consumer of the corrected main_panel /
# fit_establishment_models(), no changes needed here.
# --------------------------------------------------------------------------
res_h1_closure <- fit_establishment_models(main_panel, "morte", context = "H.1 / Closure")
m_close <- res_h1_closure$event

terms_close <- names(coef(m_close))
years_close <- year_from_term(terms_close)
ord_close   <- order(years_close)
terms_close <- terms_close[ord_close]; years_close <- years_close[ord_close]

beta_close  <- coef(m_close)[terms_close]
sigma_close <- vcov(m_close)[terms_close, terms_close]
numPre_close  <- sum(years_close < 2007)
numPost_close <- sum(years_close > 2007)
log_msg("Closure event-study: %d pre-periods (%s), %d post-periods (%s)",
        numPre_close, paste(years_close[years_close < 2007], collapse = ","),
        numPost_close, paste(years_close[years_close > 2007], collapse = ","))

honest_closure <- make_honest_plot(beta_close, sigma_close, numPre_close, numPost_close, "Closure")

out_path_a <- file.path(figures_dir, "Fig_H01a_HonestDID_Closure_5km_tract.png")
ggsave(filename = out_path_a, plot = honest_closure$plot, dpi = 300, width = 7, height = 7, units = "in")
log_msg("Saved figure: %s", out_path_a)

# --------------------------------------------------------------------------
# (b) Worker Employment -- replicates the CORRECTED output_empregados() PSM +
# panel pipeline (fixed 2007 baseline treat_B, no mover-year outcome
# nulling), inline, since we need the raw fixest object with a full
# covariance matrix (output_empregados() only returns tidied estimates).
# --------------------------------------------------------------------------
w2 <- dados_main %>% dplyr::filter(year == 2007)
w3 <- w2 %>%
  dplyr::filter(dplyr::between(dist_flood, WORKER_MIN_TREAT, WORKER_MAX_TREAT) |
                  dplyr::between(dist_flood, WORKER_MIN_CTRL, WORKER_MAX_CTRL)) %>%
  dplyr::mutate(
    treat   = ifelse(ano_morte == 2008 & dplyr::between(dist_flood, WORKER_MIN_TREAT, WORKER_MAX_TREAT), 1, 0),
    control = ifelse(dplyr::between(dist_flood, WORKER_MIN_CTRL, WORKER_MAX_CTRL), 1, 0)
  ) %>%
  dplyr::filter(treat == 1 | control == 1)

w4 <- w3 %>%
  dplyr::mutate(
    educ_d1 = ifelse(grau_instr %in% c("1", "2", "3"), 1, 0),
    educ_d2 = ifelse(grau_instr %in% c("4", "5"), 1, 0),
    educ_d3 = ifelse(grau_instr %in% c("6", "7"), 1, 0),
    educ_d4 = ifelse(grau_instr %in% c("8", "9", "10", "11"), 1, 0),
    male    = ifelse(genero == "1", 1, 0),
    idade = as.numeric(idade), grau_instr = as.numeric(grau_instr),
    rem_med_r = as.numeric(rem_med_r), rem_dez_r = as.numeric(rem_dez_r),
    codemun = as.numeric(codemun), temp_empr = as.numeric(temp_empr),
    tamestab = as.numeric(tamestab), subs_ibge = as.numeric(subs_ibge),
    Construcao  = ifelse(subs_ibge == 15, 1, 0),
    Industria   = ifelse(subs_ibge %in% 3:13, 1, 0),
    Comercio    = ifelse(subs_ibge %in% c(16, 17), 1, 0),
    servicos    = ifelse(subs_ibge == 21, 1, 0),
    transporte  = ifelse(subs_ibge == 20, 1, 0),
    trabalhador_fixo       = ifelse(tipo_sintetizado %in% c("CLT", "Estatutário"), 1, 0),
    trabalhador_temporario = ifelse(tipo_sintetizado == "Trabalho Temporário", 1, 0),
    trabalhador_outros     = ifelse(tipo_sintetizado %in% c("Outros", "Trabalho Avulso"), 1, 0)
  )

psm_h1 <- MatchIt::matchit(
  treat ~ idade + rem_med_r + temp_empr + tamestab,
  method = "nearest",
  exact  = c("educ_d1", "educ_d2", "educ_d3", "educ_d4", "male",
             "Construcao", "Industria", "Comercio", "servicos", "transporte",
             "trabalhador_fixo", "trabalhador_temporario", "trabalhador_outros"),
  replace = FALSE,
  data    = w4
)
m_data_h1 <- MatchIt::match.data(psm_h1)
# Carry `treat` (baseline classification) forward too, not just `weights` --
# this is what fixes treatment to each worker's 2007 job for the rest of
# their panel below.
baseline_h1 <- m_data_h1[, c("cpf", "weights", "treat")]
names(baseline_h1)[names(baseline_h1) == "treat"] <- "treat_base"
w2m <- merge(dados_main, baseline_h1, by = "cpf")

w3b <- expand.grid(dplyr::distinct(w2m, cpf)$cpf, 2002:max(dados_main$year))
names(w3b) <- c("cpf", "year")

w4b <- dplyr::left_join(w3b, w2m, by = c("cpf", "year")) %>%
  dplyr::arrange(cpf, year) %>%
  dplyr::mutate(ano_nascimento = year - idade, code_tract = as.numeric(code_tract)) %>%
  dplyr::group_by(cpf) %>%
  tidyr::fill(codemun, id_estab, code_tract, pis, min_ano, genero,
              ano_nascimento, grau_instr, treat_base, weights, .direction = "downup") %>%
  dplyr::group_by(id_estab) %>%
  tidyr::fill(ano_morte, dist_flood, code_tract, .direction = "downup") %>%
  dplyr::group_by(cpf) %>%
  dplyr::mutate(
    idade         = ifelse(is.na(idade), year - ano_nascimento, idade),
    # Pure RAIS-presence indicator, not nulled based on distance to the flood
    # spots -- tracks employment anywhere, including outside Santa Catarina.
    empregado     = ifelse(is.na(as.numeric(emp_31dez)), 0, 1)
  ) %>%
  dplyr::filter(year >= 2003 & year <= max(dados_main$year), year >= min_ano) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    treat_B    = treat_base,  # fixed baseline, not recomputed from that year's dist_flood
    code_tract = ifelse(is.na(code_tract), codemun, code_tract),
    code_tract = as.numeric(code_tract),
    pre_pos    = ifelse(year > 2008, 1, 0),
    codemun    = as.numeric(codemun),
    year       = as.numeric(year)
  )

reg_emp <- feols(empregado ~ i(year, treat_B, 2007) | year + cpf + pre_pos[code_tract],
                  data = w4b, cluster = ~code_tract, lean = FALSE)
log_msg("Employment event-study fit: N=%d", nobs(reg_emp))

terms_emp <- names(coef(reg_emp))
years_emp <- year_from_term(terms_emp)
ord_emp   <- order(years_emp)
terms_emp <- terms_emp[ord_emp]; years_emp <- years_emp[ord_emp]

beta_emp  <- coef(reg_emp)[terms_emp]
sigma_emp <- vcov(reg_emp)[terms_emp, terms_emp]
numPre_emp  <- sum(years_emp < 2007)
numPost_emp <- sum(years_emp > 2007)
log_msg("Employment event-study: %d pre-periods (%s), %d post-periods (%s)",
        numPre_emp, paste(years_emp[years_emp < 2007], collapse = ","),
        numPost_emp, paste(years_emp[years_emp > 2007], collapse = ","))

honest_emp <- make_honest_plot(beta_emp, sigma_emp, numPre_emp, numPost_emp, "Employment")

out_path_b <- file.path(figures_dir, "Fig_H01b_HonestDID_Employment_5km_tract.png")
ggsave(filename = out_path_b, plot = honest_emp$plot, dpi = 300, width = 7, height = 7, units = "in")
log_msg("Saved figure: %s", out_path_b)

log_msg("=== 01_figure_H1_honestdid_sensitivity.R: done ===")
