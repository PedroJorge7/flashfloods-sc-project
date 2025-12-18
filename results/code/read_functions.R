

output_empregados <- function(dados, min_treat, max_treat, 
                              min_control, max_control, 
                              remove_treat_control_mob = FALSE,
                              expand_outcomes = FALSE,
                              exposed_workers = FALSE,
                              trend = FALSE,
                              PSM = TRUE) {
  
  if(PSM == TRUE){
    
    # Filtrar para o ano de 2008 e para a área de estudo
    dados2 <- dados %>% filter(year == 2007)
    
    # Filtrar para as distâncias definidas para tratamento e controle
    dados3 <- dados2 %>% filter(between(dist_flood, min_treat, max_treat) | between(dist_flood, min_control, max_control))
    
    # Definir variáveis de tratamento e controle
    if(exposed_workers ==  TRUE){
      dados3 <- dados3 %>%
        mutate(
          treat = ifelse(between(dist_flood, min_treat, max_treat), 1, 0),
          control = ifelse(between(dist_flood, min_control, max_control), 1, 0)
        ) %>%
        filter(treat == 1 | control == 1)
    } else {
      dados3 <- dados3 %>%
        mutate(
          treat = ifelse(ano_morte == 2008 & between(dist_flood, min_treat, max_treat), 1, 0),
          control = ifelse(between(dist_flood, min_control, max_control), 1, 0)
        ) %>%
        filter(treat == 1 | control == 1)
    }
    
    # Criar variáveis adicionais
    dados4 <- dados3 %>%
      mutate(
        educ_d1 = ifelse(grau_instr %in% c("1", "2", "3"), 1, 0),
        educ_d2 = ifelse(grau_instr %in% c("4", "5"), 1, 0),
        educ_d3 = ifelse(grau_instr %in% c("6", "7"), 1, 0),
        educ_d4 = ifelse(grau_instr %in% c("8", "9", "10", "11"), 1, 0),
        male = ifelse(genero == "1", 1, 0),
        idade = as.numeric(idade),
        grau_instr = as.numeric(grau_instr),
        rem_med_r = as.numeric(rem_med_r),
        rem_dez_r = as.numeric(rem_dez_r),
        codemun = as.numeric(codemun),
        temp_empr = as.numeric(temp_empr),
        tamestab = as.numeric(tamestab),
        subs_ibge = as.numeric(subs_ibge),
        Agricultura = ifelse(subs_ibge == 25, 1, 0),
        Construcao = ifelse(subs_ibge == 15, 1, 0),
        Industria = ifelse(subs_ibge %in% 3:13, 1, 0),
        Comercio = ifelse(subs_ibge %in% c(16, 17), 1, 0),
        servicos = ifelse(subs_ibge == 21, 1, 0),
        transporte = ifelse(subs_ibge == 20, 1, 0),
        others = ifelse(!Agricultura & !Construcao & !Industria & !Comercio, 1, 0),
        trabalhador_fixo = ifelse(tipo_sintetizado %in% c("CLT", "Estatutário"), 1, 0),
        trabalhador_temporario = ifelse(tipo_sintetizado == "Trabalho Temporário", 1, 0),
        trabalhador_outros = ifelse(tipo_sintetizado %in% c("Outros", "Trabalho Avulso"), 1, 0),
        peq = ifelse(tamestab <= 2, 1, 0),
        media = ifelse(tamestab %in% c(3, 4, 5), 1, 0),
        grande = ifelse(tamestab >= 5, 1, 0)
      )
    
    # Realizar o matching
    psm <- matchit(treat ~ idade + rem_med_r + temp_empr + tamestab, method = "nearest",
                   exact = c("educ_d1", "educ_d2", "educ_d3", "educ_d4", "male", 
                             "Construcao", "Industria", "Comercio", "servicos", "transporte",
                             "trabalhador_fixo", "trabalhador_temporario", "trabalhador_outros"), 
                   replace = FALSE, data = dados4)
    
    # Obter dados pareados e salvar
    m.data <- match.data(psm)
    #saveRDS(m.data, file = "restricted_PSM_database.rds")
    
    # Carregar e processar dados pareados
    # dados_PSM <- readRDS("restricted_PSM_database.rds")
    dados_PSM <- m.data
    dados2 <- merge(dados, dados_PSM[, c("cpf", "weights")], by = "cpf")
    
  } else {
    dados2 <- dados
  }
  
  
  dados3 <- expand.grid(distinct(dados2, cpf)$cpf, 2002:max(dados$year))
  names(dados3) <- c('cpf', 'year')
  dados4 <- left_join(dados3, dados2) %>%
    arrange(cpf, year) %>%
    mutate(ano_nascimento = year - idade,
           code_tract = as.numeric(code_tract)) %>%
    group_by(cpf) %>%
    tidyr::fill(codemun, id_estab, code_tract,pis, min_ano, genero, ano_nascimento, grau_instr, .direction = 'downup') %>%
    group_by(id_estab) %>%
    tidyr::fill(ano_morte, dist_flood,code_tract, .direction = 'downup') %>%
    group_by(cpf) %>%
    mutate(idade = ifelse(is.na(idade), year - ano_nascimento, idade),
           mover_ano_mun = ifelse(lag(codemun, 1) != codemun, 1, 0),
           empregado = ifelse(is.na(as.numeric(emp_31dez)), 0, 1)) %>%
    filter(year >= 2003 & year <= max(dados$year), year >= min_ano)
  
  # Carregar índice e ajustar rendimentos
  indice <- readxl::read_excel("./data/indice.xlsx") %>%
    select(c(year = Data, indice))
  dados4 <- left_join(dados4, indice) 
  
  dados4$salario_hora = dados4$rem_med_r / dados4$horas_contr
  dados4$salario_hora_real = BETS::deflate(dados4$salario_hora, dados4$indice, type = 'index')
  dados4$rendimento_real = BETS::deflate(dados4$rem_dez_r, dados4$indice, type = 'index')
  dados4$rendimento_medio_real = BETS::deflate(dados4$rem_med_r, dados4$indice, type = 'index')
  
  # dados4$treat1 <- ifelse(dados4$dist_flood <= 12.5, 1, 0)
  
  
  if(expand_outcomes == TRUE){
    
    # Definir variáveis de tratamento
    # dados4 <- dados4 %>%
    #   arrange(cpf, year) %>%
    #   group_by(cpf) %>%
    #   mutate(
    #     treat_B = ifelse(between(dist_flood, min_treat, max_treat), 1,
    #               ifelse(between(dist_flood, min_control, max_control),0, NA)),
    #     treat_B   = ifelse(is.na(treat_B) & lag(treat_B, default = NA) == 1, 1, treat_B),
    #     empregado             = ifelse(is.na(treat_B), NA, empregado),
    #     rendimento_medio_real = ifelse(is.na(treat_B), NA, rendimento_medio_real),
    #     rendimento_real       = ifelse(is.na(treat_B), NA, rendimento_real),
    #     codemun_ref = ifelse(min_ano == year,codemun,NA)
    #   ) %>%
    #   tidyr::fill(codemun_ref, .direction = 'downup') %>% 
    #   mutate(migration_interna_ref = ifelse(codemun == codemun_ref,0,1),
    #          migration_interna_ref = ifelse(is.na(treat_B),NA, migration_interna_ref),
    #          migration_interna = mover_ano_mun,
    #          migration_interna = ifelse(is.na(treat_B),NA, migration_interna),
    #          migration_geral     = ifelse(is.na(treat_B), NA, mover_ano_mun),
    #          migration_geral_ref = ifelse(codemun == codemun_ref,0,1),
    #          migration_geral_ref = ifelse(empregado == 0,NA,migration_geral_ref),
    #          migration_geral_ref = ifelse(is.na(treat_B),NA, migration_geral_ref),
    #          treat_max = max(treat_B),
    #          treat_min = min(treat_B))
    
    dados4 <- dados4 %>%
      mutate(treat_B  = ifelse(year == 2008 & between(dist_flood, min_treat, max_treat), 1,
                               ifelse(year == 2008 & between(dist_flood, min_control, max_control),0, NA)),
             codemun_ref = ifelse(min_ano == year,codemun,NA)) %>%
      tidyr::fill(treat_B,codemun_ref, .direction = 'downup') %>%
      mutate(treat_B_agg  = ifelse(year < 2008,0,treat_B),
             migration_geral_ref = ifelse(codemun == codemun_ref,0,1),
             migration_geral_ref = ifelse(empregado == 0,NA,migration_geral_ref),
             migration_geral     = ifelse(is.na(treat_B), NA, mover_ano_mun),
             migration_geral     = ifelse(empregado == 0, NA, migration_geral),
             #empregado           = ifelse(empregado == 0, NA, empregado),
             rendimento_medio_real     = ifelse(is.na(treat_B), NA, rendimento_medio_real),
             rendimento_medio_real     = ifelse(is.na(treat_B), NA, rendimento_medio_real),
             migration_geral     = ifelse(rendimento_medio_real == 0, NA, migration_geral))
    
  } else {
    
    # Definir variáveis de tratamento
    dados4 <- dados4 %>%
      mutate(
        treat_B = ifelse(between(dist_flood, min_treat, max_treat), 1,
                         ifelse(between(dist_flood, min_control, max_control),0, NA)),
        empregado             = ifelse(is.na(treat_B), NA, empregado),
        rendimento_medio_real = ifelse(is.na(treat_B), NA, rendimento_medio_real),
        rendimento_real       = ifelse(is.na(treat_B), NA, rendimento_real),
        codemun_ref = ifelse(min_ano == year,codemun,NA)
      ) %>%
      arrange(cpf, year) %>%
      group_by(cpf) %>%
      tidyr::fill(codemun_ref, .direction = 'downup') %>% 
      mutate(
        migration_interna_ref = ifelse(codemun == codemun_ref,0,1),
        migration_interna_ref = ifelse(is.na(treat_B),NA, migration_interna_ref),
        migration_interna = mover_ano_mun,
        migration_interna = ifelse(is.na(treat_B),NA, migration_interna),
        treat_B   = ifelse(is.na(treat_B) & lag(treat_B, default = NA) == 1, 1, treat_B),
        migration_geral     = ifelse(empregado == 0, NA, mover_ano_mun),
        migration_geral     = ifelse(is.na(treat_B), NA, migration_geral),
        migration_geral_ref = ifelse(codemun == codemun_ref,0,1),
        migration_geral_ref = ifelse(empregado == 0,NA,migration_geral_ref),
        migration_geral_ref = ifelse(is.na(treat_B),NA, migration_geral_ref),
        treat_max = max(treat_B),
        treat_min = min(treat_B))
  }
  
  
  # c <- dados4 %>% filter(cpf == 169243044) %>%
  #   select(c(starts_with('migration'),empregado, rendimento_real, codemun,codemun_ref,treat_B,dist_flood))
  
  if(remove_treat_control_mob == TRUE){
    dados4 <- dados4 %>% filter(treat_max == treat_min)  
  }
  
  
  
  
  # dados4 <- dados4 %>%
  #   mutate(treat_B  = ifelse(year == 2008 & between(dist_flood, min_treat, max_treat), 1,
  #                     ifelse(year == 2008 & between(dist_flood, min_control, max_control),0, NA))) %>%
  #   tidyr::fill(treat_B, .direction = 'downup') %>%
  #   mutate(treat_B_agg  = ifelse(year < 2008,0,treat_B),
  #          migration = ifelse(is.na(treat_B), NA, mover_ano_mun))
  
  dados4$migration_geral <- ifelse(dados4$empregado == 0,NA,dados4$migration_geral)
  # Definição de treat_B_agg e treat_A_agg
  dados4$treat_B_agg <- ifelse(dados4$year >= 2009 & dados4$treat_B == 1, 1, NA)
  dados4$treat_B_agg <- ifelse(!is.na(dados4$treat_B) & is.na(dados4$treat_B_agg), 0, dados4$treat_B_agg)
  
  for (y in 2002:2016) {
    dados4[[paste0("treat_B_", y)]] <- ifelse(dados4$year == y, dados4$treat_B, 0)
    dados4[[paste0("treat_B_", y)]] <- ifelse(!is.na(dados4$treat_B) & dados4$year != y, 0, dados4[[paste0("treat_B_", y)]])
  }
  
  # reg1 <- summary(feols(migration_interna_ref ~ i(year, treat_B, 2007) | year + cpf, data = dados4), se = "twoway")
  # reg2 <- summary(feols(migration_geral_ref ~ i(year, treat_B, 2007) | year + cpf, data = dados4), se = "twoway")
  # reg3 <- summary(feols(migration_interna ~ i(year, treat_B, 2007) | year + cpf, data = dados4), se = "twoway")
  # reg4 <- summary(feols(migration_geral ~ i(year, treat_B, 2007) | year + cpf, data = dados4), se = "twoway")
  # 
  # 
  # output <- rbind(
  #   data.frame(Regression = "migration_interna_ref", parmseq = as.numeric(gsub(".*::(\\d+):.*", "\\1", names(coef(reg1)))), estimate = coef(reg1), min = coef(reg1) - 1.96 * reg1$se, max = coef(reg1) + 1.96 * reg1$se),
  #   data.frame(Regression = "migration_geral_ref", parmseq = as.numeric(gsub(".*::(\\d+):.*", "\\1", names(coef(reg2)))), estimate = coef(reg2), min = coef(reg2) - 1.96 * reg2$se, max = coef(reg2) + 1.96 * reg2$se),
  #   data.frame(Regression = "migration_interna", parmseq = as.numeric(gsub(".*::(\\d+):.*", "\\1", names(coef(reg3)))), estimate = coef(reg3), min = coef(reg3) - 1.96 * reg3$se, max = coef(reg3) + 1.96 * reg3$se),
  #   data.frame(Regression = "migration_geral", parmseq = as.numeric(gsub(".*::(\\d+):.*", "\\1", names(coef(reg4)))), estimate = coef(reg4), min = coef(reg4) - 1.96 * reg4$se, max = coef(reg4) + 1.96 * reg4$se)
  # )
  # 
  # output <- rbind(output,c("migration_interna_ref",2008,0,0,0),
  #                        c("migration_geral_ref",2008,0,0,0),
  #                        c("migration_interna",2008,0,0,0),
  #                        c("migration_geral",2008,0,0,0))
  # 
  # output$parmseq <- as.numeric(output$parmseq)
  # output <- output %>%
  #   arrange(Regression, parmseq) %>%
  #   mutate(estimate = as.numeric(estimate), min = as.numeric(min), max = as.numeric(max), parmseq = as.numeric(parmseq), treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control))
  # 
  # 
  # 
  # plot <- function(x){
  #   output %>% 
  #     filter(Regression == x) %>% 
  #     mutate(parmseq = as.numeric(parmseq)) %>% 
  #     ggplot(aes(x = parmseq, y = estimate, group = 1)) +
  #     geom_ribbon(aes(ymax = max, ymin = min), fill = "grey90", alpha = 0.5) +
  #     geom_line(aes(parmseq, max), color = "grey30", size = 0.1) +
  #     geom_line(aes(parmseq, min), color = "grey30", size = 0.1) + 
  #     geom_point( size = 2, color = "firebrick", fill = "black") +
  #     geom_line(color = "firebrick", size = 1) +
  #     scale_x_continuous(breaks = c(2003:2012),labels = c(2003:2012)) +
  #     geom_hline(yintercept=0) +
  #     geom_vline(xintercept=2007) +
  #     labs( x='Years', y='Coefficient', title = paste0(x)) +
  #     theme_bw() + 
  #     theme(legend.position = "bottom")
  # }
  # 
  # # Usando cowplot para plotar várias regressões
  # library(cowplot)
  # lapply(unique(output$Regression), plot) %>% 
  #   plot_grid(plotlist = ., nrow = 2)
  # 
  # reg1 <- summary(feols(migration_interna_ref ~ treat_B_agg | year + cpf, data = dados4), se = "twoway")
  # reg2 <- summary(feols(migration_geral_ref ~ treat_B_agg | year + cpf, data = dados4), se = "twoway")
  # reg3 <- summary(feols(migration_interna ~ treat_B_agg | year + cpf, data = dados4), se = "twoway")
  # reg4 <- summary(feols(migration_geral ~ treat_B_agg | year + cpf, data = dados4), se = "twoway")
  # 
  # output2 <- rbind(
  #   data.frame(Regression = "migration_interna_ref", parmseq = "Flash Flood Post", estimate = coef(reg1), min = coef(reg1) - 1.96 * reg1$se, max = coef(reg1) + 1.96 * reg1$se,  treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control)),
  #   data.frame(Regression = "migration_geral_ref", parmseq = "Flash Flood Post", estimate = coef(reg2), min = coef(reg2) - 1.96 * reg2$se, max = coef(reg2) + 1.96 * reg2$se,  treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control)),
  #   data.frame(Regression = "migration_interna", parmseq = "Flash Flood Post", estimate = coef(reg3), min = coef(reg3) - 1.96 * reg3$se, max = coef(reg3) + 1.96 * reg3$se,  treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control)),
  #   data.frame(Regression = "migration_geral", parmseq = "Flash Flood Post", estimate = coef(reg4), min = coef(reg4) - 1.96 * reg4$se, max = coef(reg3) + 1.96 * reg4$se,  treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control))
  # )
  # 
  # 
  # reg1 <- summary(feols(migration_interna_ref ~ treat_B_2008 + treat_B_2009 + treat_B_2010 + treat_B_2011 + treat_B_2012 | year + cpf, data = dados4), se = "twoway")
  # reg2 <- summary(feols(migration_geral_ref ~  treat_B_2008 + treat_B_2009 + treat_B_2010 + treat_B_2011 + treat_B_2012 | year + cpf, data = dados4), se = "twoway")
  # reg3 <- summary(feols(migration_interna ~  treat_B_2008 + treat_B_2009 + treat_B_2010 + treat_B_2011 + treat_B_2012 | year + cpf, data = dados4), se = "twoway")
  # reg4 <- summary(feols(migration_geral ~  treat_B_2008 + treat_B_2009 + treat_B_2010 + treat_B_2011 + treat_B_2012 | year + cpf, data = dados4), se = "twoway")
  # 
  # output3 <- rbind(
  #   data.frame(Regression = "migration_interna_ref", parmseq = gsub("treat_B_", "Flash Flood ", names(coef(reg1))), estimate = coef(reg1), min = coef(reg1) - 1.96 * reg1$se, max = coef(reg1) + 1.96 * reg1$se, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control)),
  #   data.frame(Regression = "migration_geral_ref", parmseq = gsub("treat_B_", "Flash Flood ", names(coef(reg2))), estimate = coef(reg2), min = coef(reg2) - 1.96 * reg2$se, max = coef(reg2) + 1.96 * reg2$se, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control)),
  #   data.frame(Regression = "migration_interna", parmseq = gsub("treat_B_", "Flash Flood ", names(coef(reg3))), estimate = coef(reg3), min = coef(reg3) - 1.96 * reg3$se, max = coef(reg3) + 1.96 * reg3$se, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control)),
  #   data.frame(Regression = "migration_geral", parmseq = gsub("treat_B_", "Flash Flood ", names(coef(reg4))), estimate = coef(reg4), min = coef(reg4) - 1.96 * reg4$se, max = coef(reg4) + 1.96 * reg4$se, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control))
  # )
  # 
  # output <- rbind(output2,output3)
  # output$parmseq <- gsub("Flash Flood ","",output$parmseq)
  # 
  # # Função para plotar os gráficos
  # plot <- function(x){
  #   output %>% 
  #     filter(Regression == x) %>% 
  #     ggplot(aes(y = parmseq, x = estimate)) +
  #     geom_pointrange(
  #       aes(xmax = max, xmin = min),
  #       size = 0.5 , position = position_dodge(width=0.7)
  #     ) +
  #     geom_vline(xintercept = 0, linetype = "dashed") +
  #     scale_y_discrete(limits = c("Post",  "2008","2009", "2010", "2011", "2012")) + # Incluindo o "ATT" no eixo y
  #     labs(x = 'Coefficient', y = 'Year', title = paste0(x)) +
  #     scale_color_brewer(palette = "Reds") +
  #     coord_flip() +
  #     theme_bw() + 
  #     theme(legend.position = "bottom")
  # }
  # 
  # # Usando cowplot para arranjar os gráficos
  # lapply(unique(output$Regression), plot) %>% 
  #   ggpubr::ggarrange(plotlist = ., nrow = 2, ncol = 2,
  #                     common.legend = TRUE, legend = "bottom")
  
  if(trend == TRUE){
    
    dados4$code_tract <- ifelse(is.na(dados4$code_tract),dados4$codemun,dados4$code_tract)
    dados4$code_tract <- as.numeric(dados4$code_tract)  # ou as.character(dados4$code_tract)
    dados4$pre_pos <- ifelse(dados4$year > 2008,1,0)
    dados4$codemun    <- as.numeric(dados4$codemun)
    dados4$year <- as.numeric(dados4$year)  # ou as.factor(dados4$year)
    dados4$trend <- interaction(dados4$code_tract, dados4$year)
    
    # Estimações e construção do data frame de output
    reg1 <- summary(feols(empregado ~ i(year, treat_B, 2007) | year + cpf + pre_pos[code_tract] , data = dados4), se = "twoway")
    reg2 <- summary(feols(log(rendimento_medio_real) ~ i(year, treat_B, 2007) | year + cpf + pre_pos[code_tract], data = dados4), se = "twoway")
    reg3 <- summary(feols(migration_geral ~ i(year, treat_B, 2007) | year + cpf + pre_pos[code_tract], data = dados4), se = "twoway")
    
    output1 <- rbind(
      data.frame(Regression = "A - Employment (1/0)", parmseq = as.numeric(gsub(".*::(\\d+):.*", "\\1", names(coef(reg1)))), estimate = coef(reg1), se = reg3$se, min = coef(reg1) - 1.96 * reg1$se, max = coef(reg1) + 1.96 * reg1$se, p.value = broom::tidy(reg1)$p.value, nobs = reg1$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control)),
      data.frame(Regression = "B - Wage Value", parmseq = as.numeric(gsub(".*::(\\d+):.*", "\\1", names(coef(reg2)))), estimate = coef(reg2), se = reg3$se, min = coef(reg2) - 1.96 * reg2$se, max = coef(reg2) + 1.96 * reg2$se, p.value = broom::tidy(reg2)$p.value, nobs = reg2$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control)),
      data.frame(Regression = "C - Migration (0/1)", parmseq = as.numeric(gsub(".*::(\\d+):.*", "\\1", names(coef(reg3)))), estimate = coef(reg3), se = reg3$se, min = coef(reg3) - 1.96 * reg3$se, max = coef(reg3) + 1.96 * reg3$se, p.value = broom::tidy(reg3)$p.value, nobs = reg3$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control))
    )
    
    output1 <- rbind(output1,c("A - Employment (1/0)",2007,0,0,0,0,0,0, paste0(min_treat, "-", max_treat), paste0(min_control, "-", max_control)),
                     c("B - Wage Value",2007,0,0,0,0,0,0, paste0(min_treat, "-", max_treat), paste0(min_control, "-", max_control)),
                     c("C - Migration (0/1)",2007,0,0,0,0,0,0,  paste0(min_treat, "-", max_treat), paste0(min_control, "-", max_control)))
    
    output1 <- output1 %>%
      arrange(Regression, parmseq) %>%
      mutate(estimate = as.numeric(estimate), min = as.numeric(min), max = as.numeric(max),
             parmseq = as.numeric(parmseq), treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control))
    
    output1$type <- 'event_study'
    
    
    reg1 <- summary(feols(empregado ~ treat_B_agg | year + cpf + pre_pos[code_tract], data = dados4), se = "twoway")
    reg2 <- summary(feols(log(rendimento_medio_real) ~ treat_B_agg | year + cpf + pre_pos[code_tract], data = dados4), se = "twoway")
    reg3 <- summary(feols(migration_geral ~ treat_B_agg | year + cpf + pre_pos[code_tract], data = dados4), se = "twoway")
    
    output2 <- rbind(
      data.frame(Regression = "A - Employment (1/0)", parmseq = "Flash Flood Post", estimate = coef(reg1), se = reg3$se, min = coef(reg1) - 1.96 * reg1$se, max = coef(reg1) + 1.96 * reg1$se, p.value = broom::tidy(reg1)$p.value,nobs = reg1$nobs,  treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control)),
      data.frame(Regression = "B - Wage Value", parmseq = "Flash Flood Post", estimate = coef(reg2), se = reg3$se, min = coef(reg2) - 1.96 * reg2$se, max = coef(reg2) + 1.96 * reg2$se,p.value = broom::tidy(reg2)$p.value,nobs = reg2$nobs,  treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control)),
      data.frame(Regression = "C - Migration (0/1)", parmseq = "Flash Flood Post", estimate = coef(reg3), se = reg3$se, min = coef(reg3) - 1.96 * reg3$se, max = coef(reg3) + 1.96 * reg3$se,p.value = broom::tidy(reg3)$p.value, nobs = reg3$nobs,  treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control))
    )
    
    output2$type <- 'type_treatment'
    
    
    reg1 <- summary(feols(empregado ~ treat_B_2008 + treat_B_2009 + treat_B_2010 + treat_B_2011 + treat_B_2012 + treat_B_2013 + treat_B_2014 + treat_B_2015 + treat_B_2016 | year + cpf + pre_pos[code_tract], data = dados4), se = "twoway")
    reg2 <- summary(feols(log(rendimento_medio_real) ~  treat_B_2008 + treat_B_2009 + treat_B_2010 + treat_B_2011 + treat_B_2012 + treat_B_2013 + treat_B_2014 + treat_B_2015 + treat_B_2016 | year + cpf + pre_pos[code_tract], data = dados4), se = "twoway")
    reg3 <- summary(feols(migration_geral ~  treat_B_2008 + treat_B_2009 + treat_B_2010 + treat_B_2011 + treat_B_2012 + treat_B_2013 + treat_B_2014 + treat_B_2015 + treat_B_2016 | year + cpf + pre_pos[code_tract], data = dados4), se = "twoway")
    
    
    
    
    output3 <- rbind(
      data.frame(Regression = "A - Employment (1/0)", parmseq = gsub("treat_B_", "Flash Flood ", names(coef(reg1))), estimate = coef(reg1), se = reg3$se, min = coef(reg1) - 1.96 * reg1$se, max = coef(reg1) + 1.96 * reg1$se,p.value = broom::tidy(reg1)$p.value, nobs = reg1$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control)),
      data.frame(Regression = "B - Wage Value", parmseq = gsub("treat_B_", "Flash Flood ", names(coef(reg2))), estimate = coef(reg2), se = reg3$se, min = coef(reg2) - 1.96 * reg2$se, max = coef(reg2) + 1.96 * reg2$se,p.value = broom::tidy(reg2)$p.value, nobs = reg2$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control)),
      data.frame(Regression = "C - Migration (0/1)", parmseq = gsub("treat_B_", "Flash Flood ", names(coef(reg3))), estimate = coef(reg3), se = reg3$se, min = coef(reg3) - 1.96 * reg3$se, max = coef(reg3) + 1.96 * reg3$se,p.value = broom::tidy(reg3)$p.value, nobs = reg3$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control))
    )
    
    
    
    
    
    output3$type <- 'type_treatment'
    
    output <- plyr::rbind.fill(output1,output2,output3)
    
  } else {
    # Estimações e construção do data frame de output
    reg1 <- summary(feols(empregado ~ i(year, treat_B, 2007) | year + cpf, data = dados4), se = "twoway")
    reg2 <- summary(feols(log(rendimento_medio_real) ~ i(year, treat_B, 2007) | year + cpf, data = dados4), se = "twoway")
    reg3 <- summary(feols(migration_geral ~ i(year, treat_B, 2007) | year + cpf, data = dados4), se = "twoway")
    
    output1 <- rbind(
      data.frame(Regression = "A - Employment (1/0)", parmseq = as.numeric(gsub(".*::(\\d+):.*", "\\1", names(coef(reg1)))), estimate = coef(reg1), se = reg3$se, min = coef(reg1) - 1.96 * reg1$se, max = coef(reg1) + 1.96 * reg1$se, p.value = broom::tidy(reg1)$p.value, nobs = reg1$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control)),
      data.frame(Regression = "B - Wage Value", parmseq = as.numeric(gsub(".*::(\\d+):.*", "\\1", names(coef(reg2)))), estimate = coef(reg2), se = reg3$se, min = coef(reg2) - 1.96 * reg2$se, max = coef(reg2) + 1.96 * reg2$se, p.value = broom::tidy(reg2)$p.value, nobs = reg2$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control)),
      data.frame(Regression = "C - Migration (0/1)", parmseq = as.numeric(gsub(".*::(\\d+):.*", "\\1", names(coef(reg3)))), estimate = coef(reg3), se = reg3$se, min = coef(reg3) - 1.96 * reg3$se, max = coef(reg3) + 1.96 * reg3$se, p.value = broom::tidy(reg3)$p.value, nobs = reg3$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control))
    )
    
    output1 <- rbind(output1,c("A - Employment (1/0)",2007,0,0,0,0,0,0, paste0(min_treat, "-", max_treat), paste0(min_control, "-", max_control)),
                     c("B - Wage Value",2007,0,0,0,0,0,0, paste0(min_treat, "-", max_treat), paste0(min_control, "-", max_control)),
                     c("C - Migration (0/1)",2007,0,0,0,0,0,0,  paste0(min_treat, "-", max_treat), paste0(min_control, "-", max_control)))
    
    output1 <- output1 %>%
      arrange(Regression, parmseq) %>%
      mutate(estimate = as.numeric(estimate), min = as.numeric(min), max = as.numeric(max),
             parmseq = as.numeric(parmseq), treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control))
    
    output1$type <- 'event_study'
    
    
    reg1 <- summary(feols(empregado ~ treat_B_agg | year + cpf, data = dados4), se = "twoway")
    reg2 <- summary(feols(log(rendimento_medio_real) ~ treat_B_agg | year + cpf, data = dados4), se = "twoway")
    reg3 <- summary(feols(migration_geral ~ treat_B_agg | year + cpf, data = dados4), se = "twoway")
    
    output2 <- rbind(
      data.frame(Regression = "A - Employment (1/0)", parmseq = "Flash Flood Post", estimate = coef(reg1), se = reg3$se, min = coef(reg1) - 1.96 * reg1$se, max = coef(reg1) + 1.96 * reg1$se, p.value = broom::tidy(reg1)$p.value,nobs = reg1$nobs,  treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control)),
      data.frame(Regression = "B - Wage Value", parmseq = "Flash Flood Post", estimate = coef(reg2), se = reg3$se, min = coef(reg2) - 1.96 * reg2$se, max = coef(reg2) + 1.96 * reg2$se,p.value = broom::tidy(reg2)$p.value,nobs = reg2$nobs,  treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control)),
      data.frame(Regression = "C - Migration (0/1)", parmseq = "Flash Flood Post", estimate = coef(reg3), se = reg3$se, min = coef(reg3) - 1.96 * reg3$se, max = coef(reg3) + 1.96 * reg3$se,p.value = broom::tidy(reg3)$p.value, nobs = reg3$nobs,  treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control))
    )
    
    output2$type <- 'type_treatment'
    
    
    reg1 <- summary(feols(empregado ~ treat_B_2008 + treat_B_2009 + treat_B_2010 + treat_B_2011 + treat_B_2012 + treat_B_2013 + treat_B_2014 + treat_B_2015 + treat_B_2016 | year + cpf, data = dados4), se = "twoway")
    reg2 <- summary(feols(log(rendimento_medio_real) ~  treat_B_2008 + treat_B_2009 + treat_B_2010 + treat_B_2011 + treat_B_2012 + treat_B_2013 + treat_B_2014 + treat_B_2015 + treat_B_2016 | year + cpf, data = dados4), se = "twoway")
    reg3 <- summary(feols(migration_geral ~  treat_B_2008 + treat_B_2009 + treat_B_2010 + treat_B_2011 + treat_B_2012 + treat_B_2013 + treat_B_2014 + treat_B_2015 + treat_B_2016 | year + cpf, data = dados4), se = "twoway")
    
    
    
    
    output3 <- rbind(
      data.frame(Regression = "A - Employment (1/0)", parmseq = gsub("treat_B_", "Flash Flood ", names(coef(reg1))), estimate = coef(reg1), se = reg3$se, min = coef(reg1) - 1.96 * reg1$se, max = coef(reg1) + 1.96 * reg1$se,p.value = broom::tidy(reg1)$p.value, nobs = reg1$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control)),
      data.frame(Regression = "B - Wage Value", parmseq = gsub("treat_B_", "Flash Flood ", names(coef(reg2))), estimate = coef(reg2), se = reg3$se, min = coef(reg2) - 1.96 * reg2$se, max = coef(reg2) + 1.96 * reg2$se,p.value = broom::tidy(reg2)$p.value, nobs = reg2$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control)),
      data.frame(Regression = "C - Migration (0/1)", parmseq = gsub("treat_B_", "Flash Flood ", names(coef(reg3))), estimate = coef(reg3), se = reg3$se, min = coef(reg3) - 1.96 * reg3$se, max = coef(reg3) + 1.96 * reg3$se,p.value = broom::tidy(reg3)$p.value, nobs = reg3$nobs, treat = paste0(min_treat, "-", max_treat), control = paste0(min_control, "-", max_control))
    )
    
    
    
    
    
    output3$type <- 'type_treatment'
    
    output <- plyr::rbind.fill(output1,output2,output3)
  }
  
  
  
  
  
  row.names(output) <-  1:nrow(output)
  return(output)
}

gen_table <- function(df){
  table <- df %>% 
    filter(type == 'type_treatment') %>% 
    rename(std.error = se,
           term = parmseq,
           output = Regression)
  
  nobs <- table %>% select(c(output,nobs)) %>% distinct(output, .keep_all = T) 
  nobs <- nobs$nobs
  
  
  table <- table %>% 
    mutate(std.error = as.numeric(std.error),
           estimate = as.numeric(estimate),
           estimate  = round(estimate, 5),
           estimate  = sprintf("%.5f", estimate),
           std.error = round(std.error, 5),
           std.error = sprintf("%.5f", std.error),
           p.value   = as.numeric(p.value),
           estimate  = ifelse(p.value <= 0.01, paste0(estimate, "***"),
                              ifelse(p.value <= 0.05, paste0(estimate, "**"),
                                     ifelse(p.value <= 0.10, paste0(estimate, "*"), estimate))),
           std.error = paste0("(",std.error,")")) %>% 
    select(c(output,term,Coef = estimate, std.error))
  
  table <- table %>% pivot_wider(names_from = 'output', values_from = c('Coef','std.error'))
  
  # Invertendo a ordem das linhas do dataframe
  #table1 <- table1[rev(rownames(table1)), ]
  
  table <- do.call(rbind, lapply(1:nrow(table), function(i) {
    rbind(data.frame(term = table$term[i], Employment  = table$`Coef_A - Employment (1/0)`[i],
                     `log Wage`  = table$`Coef_B - Wage Value`[i],
                     Migration   = table$`Coef_C - Migration (0/1)`[i],stringsAsFactors = FALSE),
          data.frame(term = ""           , Employment  = table$`std.error_A - Employment (1/0)`[i],
                     `log Wage`  = table$`std.error_B - Wage Value`[i],
                     Migration   = table$`std.error_C - Migration (0/1)`[i],stringsAsFactors = FALSE))
  }))
  
  
  table <- rbind(table,c("",nobs))   
  return(table)
  
}

event_study_plot <- function(x){
  output_trend %>% 
    filter(type == 'event_study') %>% 
    filter(Regression == x) %>% 
    mutate(parmseq = as.numeric(parmseq)) %>% 
    ggplot(aes(x = parmseq, y = estimate, group = 1)) +
    geom_ribbon(aes(ymax = max, ymin = min), fill = "grey90", alpha = 0.5) +
    geom_line(aes(parmseq, max), color = "grey30", size = 0.1) +
    geom_line(aes(parmseq, min), color = "grey30", size = 0.1) + 
    geom_point( size = 2, color = "firebrick", fill = "black") +
    geom_line(color = "firebrick", size = 1) +
    scale_x_continuous(breaks = c(2003:2012),labels = c(2003:2012)) +
    geom_hline(yintercept=0) +
    geom_vline(xintercept=2007) +
    labs( x='Years', y='Coefficient', title = paste0(x)) +
    theme_bw() + 
    theme(legend.position = "bottom")
}

# Função para plotar os gráficos
plot <- function(x){
  output %>% 
    filter(Regression == x) %>% 
    ggplot(aes(y = parmseq, x = estimate, color = radius)) +
    geom_pointrange(
      aes(xmax = max, xmin = min),
      size = 0.5 , position = position_dodge(width=0.7)
    ) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_y_discrete(limits = c("Post",  "2008","2009", "2010", "2011", "2012")) + # Incluindo o "ATT" no eixo y
    labs(x = 'Coefficient', y = 'Year', title = paste0(x),
         color = "Control Radius") +
    scale_color_brewer(palette = "Reds") +
    coord_flip() +
    theme_bw() + 
    theme(legend.position = "bottom")
}

