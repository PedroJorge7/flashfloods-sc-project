# =============================================================================
# worker_functions.R
#
# Shared worker-level helper functions: output_empregados() (PSM + DiD
# estimation pipeline), gen_table(), and event_study_plot(). Sourced by every
# worker-level script.
# =============================================================================

source(file.path(script_dir, "utils", "path_utils.R"))

library(fixest)
library(tidyr)

# -----------------------------------------------------------------------------
# output_empregados()
# Runs the full PSM + DiD pipeline for dismissed workers.
#
# Arguments:
#   dados              - worker panel (workers_clean_data.rds)
#   min_treat/max_treat - inner radius bounds (km)
#   min_control/max_control - outer ring bounds (km)
#   remove_treat_control_mob - drop workers who switch rings across the panel
#   expand_outcomes    - use expanded outcome set (migration reference)
#   exposed_workers    - use all treated-area workers, not just dismissed
#   trend              - add census-tract pre/post trend (pre_pos[code_tract])
#   PSM                - run propensity score matching (default TRUE)
#   se_type            - "twoway" (default) or "cluster"
#   cluster_formula    - formula used when se_type = "cluster"
# -----------------------------------------------------------------------------
output_empregados <- function(dados, min_treat, max_treat,
                              min_control, max_control,
                              remove_treat_control_mob = FALSE,
                              expand_outcomes = FALSE,
                              exposed_workers = FALSE,
                              trend = FALSE,
                              PSM = TRUE,
                              se_type = "twoway",
                              cluster_formula = ~ year + cpf) {

  se_type <- match.arg(se_type, c("twoway", "cluster"))

  run_worker_model <- function(formula, data, fixef_rm = NULL) {
    model_args <- list(fml = formula, data = data)
    if (!is.null(fixef_rm)) model_args$fixef.rm <- fixef_rm
    if (identical(se_type, "cluster")) model_args$cluster <- cluster_formula
    model <- do.call(feols, model_args)
    # Diagnostic only (does not change the model or its SEs): log how many
    # distinct clusters back the cluster-robust variance, since cluster-
    # robust SEs are known to be downward-biased (artificially tight CIs)
    # when the number of clusters is small (rule of thumb: fewer than ~30-40
    # is a concern). Added to investigate a suspected pre-trend/SE issue in
    # the worker-level event study.
    if (identical(se_type, "cluster") && exists("log_msg", mode = "function")) {
      cl_vars <- all.vars(cluster_formula)
      cl_vars <- cl_vars[cl_vars %in% names(data)]
      if (length(cl_vars) > 0) {
        counts <- sapply(cl_vars, function(v) length(unique(data[[v]][!is.na(data[[v]])])))
        # For two-or-more-way clustering also report the number of unique
        # intersection groups (relevant since CGM's variance formula
        # subtracts the intersection-cluster variance; if one dimension is
        # nearly nested within another, the intersection count will be close
        # to the finer dimension's count, and two-way clustering will behave
        # similarly to clustering on that finer dimension alone).
        inter_note <- ""
        if (length(cl_vars) > 1) {
          inter_key <- do.call(paste, c(lapply(cl_vars, function(v) data[[v]]), sep = "__"))
          n_inter <- length(unique(inter_key[!is.na(inter_key)]))
          inter_note <- sprintf(", n_intersection_groups = %d", n_inter)
        }
        log_msg("  run_worker_model: cluster variable(s) = %s, n_clusters = %s, n_obs = %d%s",
                paste(cl_vars, collapse = " x "), paste(counts, collapse = " / "), nobs(model), inter_note)
      }
    }
    # BUGFIX (found while investigating a suspected pre-trend/SE issue):
    # summary(model, se = "cluster") WITHOUT an explicit `cluster=` argument
    # to summary() does NOT reuse the `cluster=cluster_formula` set at
    # feols() estimation time -- it silently re-derives clustering from the
    # model's fixed effects instead (empirically confirmed to default to
    # the `year` FE here, i.e. ~10 clusters, regardless of what
    # cluster_formula was). This made every cluster_formula choice
    # (code_tract / id_estab / id_estab+code_tract) produce IDENTICAL SEs,
    # since `year` never changed across those comparisons. Fixed by passing
    # `cluster = cluster_formula` explicitly to summary() as well.
    if (identical(se_type, "cluster")) {
      summary(model, cluster = cluster_formula)
    } else {
      summary(model, se = "twoway")
    }
  }

  # ---------------------------------------------------------------------------
  # Step 1: PSM — match 2007 cross-section within treatment/control bands
  # ---------------------------------------------------------------------------
  if (PSM) {
    dados2 <- dados %>% filter(year == 2007)
    dados3 <- dados2 %>%
      filter(between(dist_flood, min_treat, max_treat) |
               between(dist_flood, min_control, max_control))

    if (exposed_workers) {
      dados3 <- dados3 %>%
        mutate(treat  = ifelse(between(dist_flood, min_treat, max_treat), 1, 0),
               control = ifelse(between(dist_flood, min_control, max_control), 1, 0)) %>%
        filter(treat == 1 | control == 1)
    } else {
      dados3 <- dados3 %>%
        mutate(treat  = ifelse(ano_morte == 2008 & between(dist_flood, min_treat, max_treat), 1, 0),
               control = ifelse(between(dist_flood, min_control, max_control), 1, 0)) %>%
        filter(treat == 1 | control == 1)
    }

    dados4 <- dados3 %>%
      mutate(
        educ_d1 = ifelse(grau_instr %in% c("1", "2", "3"), 1, 0),
        educ_d2 = ifelse(grau_instr %in% c("4", "5"), 1, 0),
        educ_d3 = ifelse(grau_instr %in% c("6", "7"), 1, 0),
        educ_d4 = ifelse(grau_instr %in% c("8", "9", "10", "11"), 1, 0),
        male    = ifelse(genero == "1", 1, 0),
        idade = as.numeric(idade), grau_instr = as.numeric(grau_instr),
        rem_med_r = as.numeric(rem_med_r), rem_dez_r = as.numeric(rem_dez_r),
        codemun = as.numeric(codemun), temp_empr = as.numeric(temp_empr),
        tamestab = as.numeric(tamestab), subs_ibge = as.numeric(subs_ibge),
        Agricultura = ifelse(subs_ibge == 25, 1, 0),
        Construcao  = ifelse(subs_ibge == 15, 1, 0),
        Industria   = ifelse(subs_ibge %in% 3:13, 1, 0),
        Comercio    = ifelse(subs_ibge %in% c(16, 17), 1, 0),
        servicos    = ifelse(subs_ibge == 21, 1, 0),
        transporte  = ifelse(subs_ibge == 20, 1, 0),
        trabalhador_fixo      = ifelse(tipo_sintetizado %in% c("CLT", "Estatutário"), 1, 0),
        trabalhador_temporario = ifelse(tipo_sintetizado == "Trabalho Temporário", 1, 0),
        trabalhador_outros    = ifelse(tipo_sintetizado %in% c("Outros", "Trabalho Avulso"), 1, 0),
        peq    = ifelse(tamestab <= 2, 1, 0),
        media  = ifelse(tamestab %in% c(3, 4, 5), 1, 0),
        grande = ifelse(tamestab >= 5, 1, 0)
      )

    psm <- matchit(
      treat ~ idade + rem_med_r + temp_empr + tamestab,
      method = "nearest",
      exact  = c("educ_d1", "educ_d2", "educ_d3", "educ_d4", "male",
                 "Construcao", "Industria", "Comercio", "servicos", "transporte",
                 "trabalhador_fixo", "trabalhador_temporario", "trabalhador_outros"),
      replace = FALSE,
      data    = dados4
    )

    m.data <- match.data(psm)
    dados2 <- merge(dados, m.data[, c("cpf", "weights")], by = "cpf")
  } else {
    dados2 <- dados
  }

  # ---------------------------------------------------------------------------
  # Step 2: Expand to a balanced panel and compute variables
  # ---------------------------------------------------------------------------
  dados3 <- expand.grid(distinct(dados2, cpf)$cpf, 2002:max(dados$year))
  names(dados3) <- c('cpf', 'year')

  dados4 <- left_join(dados3, dados2) %>%
    arrange(cpf, year) %>%
    mutate(ano_nascimento = year - idade,
           code_tract = as.numeric(code_tract)) %>%
    group_by(cpf) %>%
    tidyr::fill(codemun, id_estab, code_tract, pis, min_ano, genero,
                ano_nascimento, grau_instr, .direction = 'downup') %>%
    group_by(id_estab) %>%
    tidyr::fill(ano_morte, dist_flood, code_tract, .direction = 'downup') %>%
    group_by(cpf) %>%
    mutate(
      idade        = ifelse(is.na(idade), year - ano_nascimento, idade),
      mover_ano_mun = ifelse(lag(codemun, 1) != codemun, 1, 0),
      empregado    = ifelse(is.na(as.numeric(emp_31dez)), 0, 1)
    ) %>%
    filter(year >= 2003 & year <= max(dados$year), year >= min_ano)

  # Deflate wages using the price index
  indice <- readxl::read_excel(data_path("indice.xlsx")) %>%
    select(c(year = Data, indice))
  dados4 <- left_join(dados4, indice)

  dados4$salario_hora          <- dados4$rem_med_r / dados4$horas_contr
  dados4$salario_hora_real     <- BETS::deflate(dados4$salario_hora, dados4$indice, type = 'index')
  dados4$rendimento_real       <- BETS::deflate(dados4$rem_dez_r,    dados4$indice, type = 'index')
  dados4$rendimento_medio_real <- BETS::deflate(dados4$rem_med_r,    dados4$indice, type = 'index')

  # ---------------------------------------------------------------------------
  # Step 3: Build treatment variables
  # ---------------------------------------------------------------------------
  if (expand_outcomes) {
    dados4 <- dados4 %>%
      mutate(
        treat_B = ifelse(year == 2008 & between(dist_flood, min_treat, max_treat), 1,
                  ifelse(year == 2008 & between(dist_flood, min_control, max_control), 0, NA)),
        codemun_ref = ifelse(min_ano == year, codemun, NA)
      ) %>%
      tidyr::fill(treat_B, codemun_ref, .direction = 'downup') %>%
      mutate(
        treat_B_agg         = ifelse(year < 2008, 0, treat_B),
        migration_geral_ref = ifelse(codemun == codemun_ref, 0, 1),
        migration_geral_ref = ifelse(empregado == 0, NA, migration_geral_ref),
        migration_geral     = ifelse(is.na(treat_B), NA, mover_ano_mun),
        migration_geral     = ifelse(empregado == 0, NA, migration_geral),
        rendimento_medio_real = ifelse(is.na(treat_B), NA, rendimento_medio_real),
        migration_geral     = ifelse(rendimento_medio_real == 0, NA, migration_geral)
      )
  } else {
    dados4 <- dados4 %>%
      mutate(
        treat_B = ifelse(between(dist_flood, min_treat, max_treat), 1,
                  ifelse(between(dist_flood, min_control, max_control), 0, NA)),
        empregado             = ifelse(is.na(treat_B), NA, empregado),
        rendimento_medio_real = ifelse(is.na(treat_B), NA, rendimento_medio_real),
        rendimento_real       = ifelse(is.na(treat_B), NA, rendimento_real),
        codemun_ref = ifelse(min_ano == year, codemun, NA)
      ) %>%
      arrange(cpf, year) %>%
      group_by(cpf) %>%
      tidyr::fill(codemun_ref, .direction = 'downup') %>%
      mutate(
        migration_interna_ref = ifelse(codemun == codemun_ref, 0, 1),
        migration_interna_ref = ifelse(is.na(treat_B), NA, migration_interna_ref),
        migration_interna     = mover_ano_mun,
        migration_interna     = ifelse(is.na(treat_B), NA, migration_interna),
        treat_B               = ifelse(is.na(treat_B) & lag(treat_B, default = NA) == 1, 1, treat_B),
        migration_geral       = ifelse(empregado == 0, NA, mover_ano_mun),
        migration_geral       = ifelse(is.na(treat_B), NA, migration_geral),
        migration_geral_ref   = ifelse(codemun == codemun_ref, 0, 1),
        migration_geral_ref   = ifelse(empregado == 0, NA, migration_geral_ref),
        migration_geral_ref   = ifelse(is.na(treat_B), NA, migration_geral_ref),
        treat_max = max(treat_B),
        treat_min = min(treat_B)
      )
  }

  dados4$migration_geral <- ifelse(dados4$empregado == 0, NA, dados4$migration_geral)

  # Aggregated post-treatment indicator (post-2009 for workers)
  dados4$treat_B_agg <- ifelse(dados4$year >= 2009 & dados4$treat_B == 1, 1, NA)
  dados4$treat_B_agg <- ifelse(!is.na(dados4$treat_B) & is.na(dados4$treat_B_agg), 0, dados4$treat_B_agg)

  # Year-specific treatment dummies
  for (y in 2002:2016) {
    dados4[[paste0("treat_B_", y)]] <- ifelse(dados4$year == y, dados4$treat_B, 0)
    dados4[[paste0("treat_B_", y)]] <- ifelse(!is.na(dados4$treat_B) & dados4$year != y, 0,
                                               dados4[[paste0("treat_B_", y)]])
  }

  if (remove_treat_control_mob) dados4 <- dados4 %>% filter(treat_max == treat_min)

  # ---------------------------------------------------------------------------
  # Step 4: Estimate employment and wage models
  # ---------------------------------------------------------------------------
  if (trend) {
    dados4$code_tract <- ifelse(is.na(dados4$code_tract), dados4$codemun, dados4$code_tract)
    dados4$code_tract <- as.numeric(dados4$code_tract)
    dados4$pre_pos    <- ifelse(dados4$year > 2008, 1, 0)
    dados4$codemun    <- as.numeric(dados4$codemun)
    dados4$year       <- as.numeric(dados4$year)

    reg1 <- run_worker_model(empregado ~ i(year, treat_B, 2007) | year + cpf + pre_pos[code_tract], data = dados4)
    reg2 <- run_worker_model(log(rendimento_medio_real) ~ i(year, treat_B, 2007) | year + cpf + pre_pos[code_tract], data = dados4, fixef_rm = "none")

    output1 <- rbind(
      data.frame(Regression = "A - Employment (1/0)", parmseq = as.numeric(gsub(".*::(\\d+):.*", "\\1", names(coef(reg1)))), estimate = coef(reg1), se = reg1$se, min = coef(reg1) - 1.96 * reg1$se, max = coef(reg1) + 1.96 * reg1$se, p.value = broom::tidy(reg1)$p.value, nobs = reg1$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control)),
      data.frame(Regression = "B - Wage Value",       parmseq = as.numeric(gsub(".*::(\\d+):.*", "\\1", names(coef(reg2)))), estimate = coef(reg2), se = reg2$se, min = coef(reg2) - 1.96 * reg2$se, max = coef(reg2) + 1.96 * reg2$se, p.value = broom::tidy(reg2)$p.value, nobs = reg2$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control))
    )
    output1 <- rbind(output1,
                     c("A - Employment (1/0)", 2007, 0, 0, 0, 0, 0, 0, paste0(min_treat, "-", max_treat), paste0(min_control, "-", max_control)),
                     c("B - Wage Value",       2007, 0, 0, 0, 0, 0, 0, paste0(min_treat, "-", max_treat), paste0(min_control, "-", max_control)))
    output1 <- output1 %>% arrange(Regression, parmseq) %>%
      mutate(estimate = as.numeric(estimate), min = as.numeric(min), max = as.numeric(max),
             parmseq = as.numeric(parmseq))
    output1$type <- 'event_study'

    reg1 <- run_worker_model(empregado ~ treat_B_agg | year + cpf + pre_pos[code_tract], data = dados4)
    reg2 <- run_worker_model(log(rendimento_medio_real) ~ treat_B_agg | year + cpf + pre_pos[code_tract], data = dados4, fixef_rm = "none")

    output2 <- rbind(
      data.frame(Regression = "A - Employment (1/0)", parmseq = "Flash Flood Post", estimate = coef(reg1), se = reg1$se, min = coef(reg1) - 1.96 * reg1$se, max = coef(reg1) + 1.96 * reg1$se, p.value = broom::tidy(reg1)$p.value, nobs = reg1$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control)),
      data.frame(Regression = "B - Wage Value",       parmseq = "Flash Flood Post", estimate = coef(reg2), se = reg2$se, min = coef(reg2) - 1.96 * reg2$se, max = coef(reg2) + 1.96 * reg2$se, p.value = broom::tidy(reg2)$p.value, nobs = reg2$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control))
    )
    output2$type <- 'type_treatment'

    reg1 <- run_worker_model(empregado ~ treat_B_2008 + treat_B_2009 + treat_B_2010 + treat_B_2011 + treat_B_2012 + treat_B_2013 + treat_B_2014 + treat_B_2015 + treat_B_2016 | year + cpf + pre_pos[code_tract], data = dados4)
    reg2 <- run_worker_model(log(rendimento_medio_real) ~ treat_B_2008 + treat_B_2009 + treat_B_2010 + treat_B_2011 + treat_B_2012 + treat_B_2013 + treat_B_2014 + treat_B_2015 + treat_B_2016 | year + cpf + pre_pos[code_tract], data = dados4, fixef_rm = "none")

    output3 <- rbind(
      data.frame(Regression = "A - Employment (1/0)", parmseq = gsub("treat_B_", "Flash Flood ", names(coef(reg1))), estimate = coef(reg1), se = reg1$se, min = coef(reg1) - 1.96 * reg1$se, max = coef(reg1) + 1.96 * reg1$se, p.value = broom::tidy(reg1)$p.value, nobs = reg1$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control)),
      data.frame(Regression = "B - Wage Value",       parmseq = gsub("treat_B_", "Flash Flood ", names(coef(reg2))), estimate = coef(reg2), se = reg2$se, min = coef(reg2) - 1.96 * reg2$se, max = coef(reg2) + 1.96 * reg2$se, p.value = broom::tidy(reg2)$p.value, nobs = reg2$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control))
    )
    output3$type <- 'type_treatment'

  } else {
    reg1 <- run_worker_model(empregado ~ i(year, treat_B, 2007) | year + cpf, data = dados4)
    reg2 <- run_worker_model(log(rendimento_medio_real) ~ i(year, treat_B, 2007) | year + cpf, data = dados4, fixef_rm = "none")

    output1 <- rbind(
      data.frame(Regression = "A - Employment (1/0)", parmseq = as.numeric(gsub(".*::(\\d+):.*", "\\1", names(coef(reg1)))), estimate = coef(reg1), se = reg1$se, min = coef(reg1) - 1.96 * reg1$se, max = coef(reg1) + 1.96 * reg1$se, p.value = broom::tidy(reg1)$p.value, nobs = reg1$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control)),
      data.frame(Regression = "B - Wage Value",       parmseq = as.numeric(gsub(".*::(\\d+):.*", "\\1", names(coef(reg2)))), estimate = coef(reg2), se = reg2$se, min = coef(reg2) - 1.96 * reg2$se, max = coef(reg2) + 1.96 * reg2$se, p.value = broom::tidy(reg2)$p.value, nobs = reg2$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control))
    )
    output1 <- rbind(output1,
                     c("A - Employment (1/0)", 2007, 0, 0, 0, 0, 0, 0, paste0(min_treat, "-", max_treat), paste0(min_control, "-", max_control)),
                     c("B - Wage Value",       2007, 0, 0, 0, 0, 0, 0, paste0(min_treat, "-", max_treat), paste0(min_control, "-", max_control)))
    output1 <- output1 %>% arrange(Regression, parmseq) %>%
      mutate(estimate = as.numeric(estimate), min = as.numeric(min), max = as.numeric(max),
             parmseq = as.numeric(parmseq))
    output1$type <- 'event_study'

    reg1 <- run_worker_model(empregado ~ treat_B_agg | year + cpf, data = dados4)
    reg2 <- run_worker_model(log(rendimento_medio_real) ~ treat_B_agg | year + cpf, data = dados4, fixef_rm = "none")

    output2 <- rbind(
      data.frame(Regression = "A - Employment (1/0)", parmseq = "Flash Flood Post", estimate = coef(reg1), se = reg1$se, min = coef(reg1) - 1.96 * reg1$se, max = coef(reg1) + 1.96 * reg1$se, p.value = broom::tidy(reg1)$p.value, nobs = reg1$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control)),
      data.frame(Regression = "B - Wage Value",       parmseq = "Flash Flood Post", estimate = coef(reg2), se = reg2$se, min = coef(reg2) - 1.96 * reg2$se, max = coef(reg2) + 1.96 * reg2$se, p.value = broom::tidy(reg2)$p.value, nobs = reg2$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control))
    )
    output2$type <- 'type_treatment'

    reg1 <- run_worker_model(empregado ~ treat_B_2008 + treat_B_2009 + treat_B_2010 + treat_B_2011 + treat_B_2012 + treat_B_2013 + treat_B_2014 + treat_B_2015 + treat_B_2016 | year + cpf, data = dados4)
    reg2 <- run_worker_model(log(rendimento_medio_real) ~ treat_B_2008 + treat_B_2009 + treat_B_2010 + treat_B_2011 + treat_B_2012 + treat_B_2013 + treat_B_2014 + treat_B_2015 + treat_B_2016 | year + cpf, data = dados4, fixef_rm = "none")

    output3 <- rbind(
      data.frame(Regression = "A - Employment (1/0)", parmseq = gsub("treat_B_", "Flash Flood ", names(coef(reg1))), estimate = coef(reg1), se = reg1$se, min = coef(reg1) - 1.96 * reg1$se, max = coef(reg1) + 1.96 * reg1$se, p.value = broom::tidy(reg1)$p.value, nobs = reg1$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control)),
      data.frame(Regression = "B - Wage Value",       parmseq = gsub("treat_B_", "Flash Flood ", names(coef(reg2))), estimate = coef(reg2), se = reg2$se, min = coef(reg2) - 1.96 * reg2$se, max = coef(reg2) + 1.96 * reg2$se, p.value = broom::tidy(reg2)$p.value, nobs = reg2$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control))
    )
    output3$type <- 'type_treatment'
  }

  output <- plyr::rbind.fill(output1, output2, output3)
  row.names(output) <- 1:nrow(output)
  return(output)
}

# -----------------------------------------------------------------------------
# gen_table()
# Formats the output of output_empregados() into a printable coefficient table.
# -----------------------------------------------------------------------------
gen_table <- function(df) {
  table <- df %>%
    filter(type == 'type_treatment') %>%
    rename(std.error = se, term = parmseq, output = Regression)

  nobs <- table %>% select(output, nobs) %>% distinct(output, .keep_all = TRUE)
  nobs <- nobs$nobs

  table <- table %>%
    mutate(
      std.error = as.numeric(std.error),
      estimate  = as.numeric(estimate),
      estimate  = sprintf("%.5f", round(estimate, 5)),
      std.error = sprintf("%.5f", round(std.error, 5)),
      p.value   = as.numeric(p.value),
      estimate  = ifelse(p.value <= 0.01, paste0(estimate, "***"),
                  ifelse(p.value <= 0.05, paste0(estimate, "**"),
                  ifelse(p.value <= 0.10, paste0(estimate, "*"), estimate))),
      std.error = paste0("(", std.error, ")")
    ) %>%
    select(output, term, Coef = estimate, std.error)

  table <- table %>% pivot_wider(names_from = 'output', values_from = c('Coef', 'std.error'))

  table <- do.call(rbind, lapply(1:nrow(table), function(i) {
    rbind(
      data.frame(term = table$term[i], Employment = table$`Coef_A - Employment (1/0)`[i], `log Wage` = table$`Coef_B - Wage Value`[i], stringsAsFactors = FALSE),
      data.frame(term = "",            Employment = table$`std.error_A - Employment (1/0)`[i], `log Wage` = table$`std.error_B - Wage Value`[i], stringsAsFactors = FALSE)
    )
  }))

  rbind(table, c("", nobs))
}

# -----------------------------------------------------------------------------
# event_study_plot()
# Builds a ggplot event-study panel from the output of output_empregados().
#
# Arguments:
#   reg_name - value of the `Regression` column to plot
#   data     - data frame returned by output_empregados(); must contain columns
#              Regression, parmseq (year), estimate, min, max, type
# -----------------------------------------------------------------------------
event_study_plot <- function(reg_name, data) {
  dd <- data %>%
    filter(Regression == reg_name, type == "event_study") %>%
    mutate(
      parmseq  = as.numeric(parmseq),
      estimate = as.numeric(estimate),
      min      = as.numeric(min),
      max      = as.numeric(max)
    ) %>%
    filter(is.finite(estimate)) %>%
    arrange(parmseq)

  ggplot2::ggplot(dd, ggplot2::aes(x = parmseq, y = estimate)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = min, ymax = max), fill = "grey90", alpha = 0.5) +
    ggplot2::geom_line(ggplot2::aes(y = max), color = "grey30", linewidth = 0.1) +
    ggplot2::geom_line(ggplot2::aes(y = min), color = "grey30", linewidth = 0.1) +
    ggplot2::geom_line(color = "firebrick", linewidth = 1) +
    ggplot2::geom_point(size = 2, color = "firebrick") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 2007.5, linetype = "dashed") +
    ggplot2::scale_x_continuous(breaks = sort(unique(dd$parmseq))) +
    ggplot2::labs(x = "Year", y = "Coefficient", title = reg_name) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}
