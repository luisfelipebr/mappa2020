
##### DEFICIT E INADEQUACAO TOTAL POR MUNICIPIO ----

deficit_e_inadequacao_habitacional_total_por_municipio <- function(microdados2010, codigo_ibge_municipio) {
  
  # filtra os dados (a partir do codigo_ibge)
  censo2010 <- filter(microdados2010, codigo_ibge == codigo_ibge_municipio)
  
  # define as variaveis
  censo2010 <- censo2010 %>%
    mutate(V11 = case_when(V2011/V6529 < 0.3 | V6529 > 1530 ~ "Adequada",
                             V2011/V6529 >= 0.3 & V6529 <= 1530 ~ "Inadequada"),
           V21 = case_when(V4001 != 5 ~ "Adequada",
                             V4001 == 5 ~ "Inadequada"),
           V22 = case_when(V4002 %in% c(11, 12, 13, 15) ~ "Adequada",
                             V4002 %in% c(14, 51, 52, 53) ~ "Inadequada"),
           V23 = case_when(V0202 %in% c(1, 2, 3, 4) ~ "Adequada",
                             V0202 %in% c(5, 6, 7, 8, 9) ~ "Inadequada"),
           V24 = case_when(V0205 %in% c(1, 2, 3, 4, 5, 6, 7, 9) | V0206 == 1 ~ "Adequada",
                             V0205 %in% c(0) | V0206 == 2 ~ "Inadequada"),
           V31 = case_when(V6204 <= 3 ~ "Adequada",
                             V6204 > 3 ~ "Inadequada"),
           V32 = case_when(V6204 <= 3 & V5020 <= 1 ~ "Adequada",
                             V6204 > 3 & V5020 > 1 ~ "Inadequada"),
           V33 = case_when(V6204 <= 3 & V4002 == 13 ~ "Adequada",
                             V6204 > 3 & V4002 == 13 ~ "Inadequada"),
           V34 = case_when(V6204 <= 3 & V0201 %in% c(1, 2) ~ "Adequada",
                             V6204 > 3 & V0201 %in% c(3, 4, 5, 6) ~ "Inadequada"),
           V61 = case_when(V0207 %in% c(1, 2) ~ "Adequada",
                             V0207 %in% c(3, 4, 5, 6) ~ "Inadequada"),
           V62 = case_when(V0208 == 1 ~ "Adequada",
                             V0208 %in% c(2, 3, 4, 5, 6, 7) ~ "Inadequada"),
           V63 = case_when(V0209 == 1 ~ "Adequada",
                             V0209 %in% c(2, 3) ~ "Inadequada"),
           V64 = case_when(V0210 %in% c(1, 2) ~ "Adequada",
                             V0210 %in% c(3, 4, 5, 6, 7) ~ "Inadequada"),
           V65 = case_when(V0211 %in% c(1, 2) ~ "Adequada",
                             V0211 == 3 ~ "Inadequada"),
           deficit_ou_inadequacao = case_when(V21 == "Inadequada" | V23 == "Inadequada" | V32 == "Inadequada" | V33 == "Inadequada" | V34 == "Inadequada" ~ "Deficit",
                                              V11 == "Inadequada" | V22 == "Inadequada" | V24 == "Inadequada" | V31 == "Inadequada" | V61 == "Inadequada" | V62 == "Inadequada" | V63 == "Inadequada" | V64  == "Inadequada" | V65 == "Inadequada" ~ "Inadequacao")) %>%
    select(codigo_ibge, V0010, V0011, fpc, deficit_ou_inadequacao)
  
  censo2010[is.na(censo2010)] = "Sem Dados ou Não Aplicável"
  
  # define a pesquisa amostral
  censo2010_ponderado <- svydesign(data = censo2010,
                                   ids = ~1,
                                   strata = ~V0011,
                                   fpc = ~fpc,
                                   weights = ~V0010)
  
  # extrai os dados de deficit e inadequacao e total de domicilios
  deficit_ou_inadequacao <- as.data.frame(svytable(~ codigo_ibge + deficit_ou_inadequacao, censo2010_ponderado))
  total_de_domicilios <- as.data.frame(svytable(~ codigo_ibge, censo2010_ponderado))
  
  # cria a tabela de saida
  deficit_habitacional_total <- deficit_ou_inadequacao %>%
    spread(deficit_ou_inadequacao, Freq) %>%
    select(codigo_ibge, Deficit, Inadequacao) %>%
    mutate(`Deficit + Inadequacao` = Deficit + Inadequacao) %>%
    inner_join(total_de_domicilios, by = "codigo_ibge") %>%
    rename("Total de domicilios" = "Freq",
           "Codigo IBGE" = "codigo_ibge")
  
  # exporta a tabela de saida
  return(deficit_habitacional_total)
  
}

##### DEFICIT E INADEQUACAO POR VARIAVEL POR MUNICIPIO ----

deficit_e_inadequacao_habitacional_variaveis_por_municipio <- function(microdados2010, codigo_ibge_municipio) {
  
  # filtra os dados (a partir do codigo_ibge)
  censo2010 <- filter(microdados2010, codigo_ibge == codigo_ibge_municipio)
  
  # define as variaveis
  censo2010 <- censo2010 %>%
    mutate(V11 = case_when(V2011/V6529 < 0.3 | V6529 > 1530 ~ "Adequada",
                             V2011/V6529 >= 0.3 & V6529 <= 1530 ~ "Inadequada"),
           V21 = case_when(V4001 != 5 ~ "Adequada",
                             V4001 == 5 ~ "Inadequada"),
           V22 = case_when(V4002 %in% c(11, 12, 13, 15) ~ "Adequada",
                             V4002 %in% c(14, 51, 52, 53) ~ "Inadequada"),
           V23 = case_when(V0202 %in% c(1, 2, 3, 4) ~ "Adequada",
                             V0202 %in% c(5, 6, 7, 8, 9) ~ "Inadequada"),
           V24 = case_when(V0205 %in% c(1, 2, 3, 4, 5, 6, 7, 9) | V0206 == 1 ~ "Adequada",
                             V0205 %in% c(0) | V0206 == 2 ~ "Inadequada"),
           V31 = case_when(V6204 <= 3 ~ "Adequada",
                             V6204 > 3 ~ "Inadequada"),
           V32 = case_when(V6204 <= 3 & V5020 <= 1 ~ "Adequada",
                             V6204 > 3 & V5020 > 1 ~ "Inadequada"),
           V33 = case_when(V6204 <= 3 & V4002 == 13 ~ "Adequada",
                             V6204 > 3 & V4002 == 13 ~ "Inadequada"),
           V34 = case_when(V6204 <= 3 & V0201 %in% c(1, 2) ~ "Adequada",
                             V6204 > 3 & V0201 %in% c(3, 4, 5, 6) ~ "Inadequada"),
           V61 = case_when(V0207 %in% c(1, 2) ~ "Adequada",
                             V0207 %in% c(3, 4, 5, 6) ~ "Inadequada"),
           V62 = case_when(V0208 == 1 ~ "Adequada",
                             V0208 %in% c(2, 3, 4, 5, 6, 7) ~ "Inadequada"),
           V63 = case_when(V0209 == 1 ~ "Adequada",
                             V0209 %in% c(2, 3) ~ "Inadequada"),
           V64 = case_when(V0210 %in% c(1, 2) ~ "Adequada",
                             V0210 %in% c(3, 4, 5, 6, 7) ~ "Inadequada"),
           V65 = case_when(V0211 %in% c(1, 2) ~ "Adequada",
                             V0211 == 3 ~ "Inadequada"),
           deficit_ou_inadequacao = case_when(V21 == "Inadequada" | V23 == "Inadequada" | V32 == "Inadequada" | V33 == "Inadequada" | V34 == "Inadequada" ~ "Deficit",
                                              V11 == "Inadequada" | V22 == "Inadequada" | V24 == "Inadequada" | V31 == "Inadequada" | V61 == "Inadequada" | V62 == "Inadequada" | V63 == "Inadequada" | V64  == "Inadequada" | V65 == "Inadequada" ~ "Inadequacao"))
  
  censo2010[is.na(censo2010)] = "Sem Dados ou Não Aplicável"
  
  # define a pesquisa amostral
  censo2010_ponderado <- svydesign(data = censo2010,
                                   ids = ~1,
                                   strata = ~V0011,
                                   fpc = ~fpc,
                                   weights = ~V0010)
  
  # extrai os dados de deficit e inadequacao e total de domicilios
  V11 <- as.data.frame(svytable(~ codigo_ibge + V11, censo2010_ponderado))
  V21 <- as.data.frame(svytable(~ codigo_ibge + V21, censo2010_ponderado)) 
  V22 <- as.data.frame(svytable(~ codigo_ibge + V22, censo2010_ponderado)) 
  V23 <- as.data.frame(svytable(~ codigo_ibge + V23, censo2010_ponderado)) 
  V24 <- as.data.frame(svytable(~ codigo_ibge + V24, censo2010_ponderado)) 
  V31 <- as.data.frame(svytable(~ codigo_ibge + V31, censo2010_ponderado))
  V32 <- as.data.frame(svytable(~ codigo_ibge + V32, censo2010_ponderado))
  V33 <- as.data.frame(svytable(~ codigo_ibge + V33, censo2010_ponderado))
  V34 <- as.data.frame(svytable(~ codigo_ibge + V34, censo2010_ponderado))
  V61 <- as.data.frame(svytable(~ codigo_ibge + V61, censo2010_ponderado)) 
  V62 <- as.data.frame(svytable(~ codigo_ibge + V62, censo2010_ponderado)) 
  V63 <- as.data.frame(svytable(~ codigo_ibge + V63, censo2010_ponderado)) 
  V64 <- as.data.frame(svytable(~ codigo_ibge + V64, censo2010_ponderado)) 
  V65 <- as.data.frame(svytable(~ codigo_ibge + V65, censo2010_ponderado))
  total_de_domicilios <- as.data.frame(svytable(~ codigo_ibge, censo2010_ponderado))
  
  # cria a tabela de saida
  deficit_habitacional_variaveis <- list(V11, V21, V22, V23, V24, V31, V32, V33, V34, V61, V62, V63, V64, V65)
  names(deficit_habitacional_variaveis) <- c("V11", "V21", "V22", "V23", "V24", "V31", "V32", "V33", "V34", "V61", "V62", "V63", "V64", "V65")
  deficit_habitacional_variaveis <- deficit_habitacional_variaveis %>% 
    lapply(setNames, c("codigo_ibge", "cond", "freq")) %>% 
    lapply(spread, cond, freq) %>%
    bind_rows(.id = "variavel") %>%
    inner_join(total_de_domicilios, by = "codigo_ibge") %>%
    rename("Total de domicilios" = "Freq",
           "Codigo IBGE" = "codigo_ibge",
           "Variavel" = "variavel") %>%
    select("Variavel", "Codigo IBGE", everything())
  deficit_habitacional_variaveis[is.na(deficit_habitacional_variaveis)] <- 0
  
  # exporta a tabela de saida
  return(deficit_habitacional_variaveis)
  
}

##### DO DEFICIT E INADEQUACAO TOTAL POR MUNICIPIO POR RENDA ----

deficit_e_inadequacao_habitacional_total_por_municipio_por_renda <- function(microdados2010, codigo_ibge_municipio) {
  
  # filtra os dados (a partir do codigo_ibge)
  censo2010 <- filter(microdados2010, codigo_ibge == codigo_ibge_municipio)
  
  # define as variaveis
  censo2010 <- censo2010 %>%
    mutate(V11 = case_when(V2011/V6529 < 0.3 | V6529 > 1530 ~ "Adequada",
                             V2011/V6529 >= 0.3 & V6529 <= 1530 ~ "Inadequada"),
           V21 = case_when(V4001 != 5 ~ "Adequada",
                             V4001 == 5 ~ "Inadequada"),
           V22 = case_when(V4002 %in% c(11, 12, 13, 15) ~ "Adequada",
                             V4002 %in% c(14, 51, 52, 53) ~ "Inadequada"),
           V23 = case_when(V0202 %in% c(1, 2, 3, 4) ~ "Adequada",
                             V0202 %in% c(5, 6, 7, 8, 9) ~ "Inadequada"),
           V24 = case_when(V0205 %in% c(1, 2, 3, 4, 5, 6, 7, 9) | V0206 == 1 ~ "Adequada",
                             V0205 %in% c(0) | V0206 == 2 ~ "Inadequada"),
           V31 = case_when(V6204 <= 3 ~ "Adequada",
                             V6204 > 3 ~ "Inadequada"),
           V32 = case_when(V6204 <= 3 & V5020 <= 1 ~ "Adequada",
                             V6204 > 3 & V5020 > 1 ~ "Inadequada"),
           V33 = case_when(V6204 <= 3 & V4002 == 13 ~ "Adequada",
                             V6204 > 3 & V4002 == 13 ~ "Inadequada"),
           V34 = case_when(V6204 <= 3 & V0201 %in% c(1, 2) ~ "Adequada",
                             V6204 > 3 & V0201 %in% c(3, 4, 5, 6) ~ "Inadequada"),
           V61 = case_when(V0207 %in% c(1, 2) ~ "Adequada",
                             V0207 %in% c(3, 4, 5, 6) ~ "Inadequada"),
           V62 = case_when(V0208 == 1 ~ "Adequada",
                             V0208 %in% c(2, 3, 4, 5, 6, 7) ~ "Inadequada"),
           V63 = case_when(V0209 == 1 ~ "Adequada",
                             V0209 %in% c(2, 3) ~ "Inadequada"),
           V64 = case_when(V0210 %in% c(1, 2) ~ "Adequada",
                             V0210 %in% c(3, 4, 5, 6, 7) ~ "Inadequada"),
           V65 = case_when(V0211 %in% c(1, 2) ~ "Adequada",
                             V0211 == 3 ~ "Inadequada"),
           deficit_ou_inadequacao = case_when(V21 == "Inadequada" | V23 == "Inadequada" | V32 == "Inadequada" | V33 == "Inadequada" | V34 == "Inadequada" ~ "Deficit",
                                              V11 == "Inadequada" | V22 == "Inadequada" | V24 == "Inadequada" | V31 == "Inadequada" | V61 == "Inadequada" | V62 == "Inadequada" | V63 == "Inadequada" | V64  == "Inadequada" | V65 == "Inadequada" ~ "Inadequacao"),
           sm1 = case_when(deficit_ou_inadequacao == "Deficit" & V6530 <= 1 ~ "Deficit",
                           deficit_ou_inadequacao == "Inadequacao" & V6530 <= 1 ~ "Inadequacao"),
           sm2 = case_when(deficit_ou_inadequacao == "Deficit" & V6530 > 1 & V6530 <= 2 ~ "Deficit",
                           deficit_ou_inadequacao == "Inadequacao" & V6530 > 1 & V6530 <= 2 ~ "Inadequacao"),
           sm3 = case_when(deficit_ou_inadequacao == "Deficit" & V6530 > 2 & V6530 <= 3 ~ "Deficit",
                           deficit_ou_inadequacao == "Inadequacao" & V6530 > 2 & V6530 <= 3 ~ "Inadequacao"),
           sm4 = case_when(deficit_ou_inadequacao == "Deficit" & V6530 > 3 & V6530 <= 4 ~ "Deficit",
                           deficit_ou_inadequacao == "Inadequacao" & V6530 > 3 & V6530 <= 4 ~ "Inadequacao"),
           sm5 = case_when(deficit_ou_inadequacao == "Deficit" & V6530 > 4 & V6530 <= 5 ~ "Deficit",
                           deficit_ou_inadequacao == "Inadequacao" & V6530 > 4 & V6530 <= 5 ~ "Inadequacao"),
           sm10 = case_when(deficit_ou_inadequacao == "Deficit" & V6530 > 5 & V6530 <= 10 ~ "Deficit",
                            deficit_ou_inadequacao == "Inadequacao" & V6530 > 5 & V6530 <= 10 ~ "Inadequacao"),
           sm11 = case_when(deficit_ou_inadequacao == "Deficit" & V6530 > 10 ~ "Deficit",
                            deficit_ou_inadequacao == "Inadequacao" & V6530 > 10 ~ "Inadequacao"),
           smna = case_when(deficit_ou_inadequacao == "Deficit" & V6530 == NA ~ "Deficit",
                            deficit_ou_inadequacao == "Inadequacao" & V6530 == NA ~ "Inadequacao"),
           smtotal = case_when(V6530 <= 1 ~ "Até 1 SM", 
                               V6530 > 1 & V6530 <= 3 ~ "De 1 à 3 SM", 
                               V6530 > 3 & V6530 <= 5 ~ "De 3 à 5 SM", 
                               V6530 > 5 ~ "Acima de 5 SM", 
                               V6530 == NA ~ "Sem Dados"))
  censo2010[is.na(censo2010)] = "Sem Dados"
  
  # define a pesquisa amostral
  censo2010_ponderado <- svydesign(data = censo2010,
                                   ids = ~1,
                                   strata = ~V0011,
                                   fpc = ~fpc,
                                   weights = ~V0010)
  
  # extrai os dados de deficit e inadequacao e total de domicilios
  deficit_ou_inadequacao <- as.data.frame(svytable(~ codigo_ibge + deficit_ou_inadequacao + smtotal, censo2010_ponderado))
  total_de_domicilios <- as.data.frame(svytable(~ codigo_ibge + smtotal, censo2010_ponderado))
  
  # cria a tabela de saida
  deficit_habitacional_total <- deficit_ou_inadequacao %>%
    spread(deficit_ou_inadequacao, Freq) %>%
    mutate(`Deficit + Inadequacao` = Deficit + Inadequacao) %>%
    select(codigo_ibge, smtotal, Deficit, Inadequacao, "Deficit + Inadequacao", "Sem Dados") %>%
    inner_join(total_de_domicilios, by = c("codigo_ibge", "smtotal")) %>%
    rename("Total de domicilios" = "Freq",
           "Codigo IBGE" = "codigo_ibge")
  
  # exporta a tabela de saida
  return(deficit_habitacional_total)
  
}
