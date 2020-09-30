
compatibilizacao1 <- function(data, ibge_code, ap_territorial, ap_ap, projecao_seade){
  
  proj_seade <- projecao_seade %>%
    filter(mun_id == ibge_code) %>%
    filter(ano == 2018)
  
  censo2010_1 <- data[data$codigo_ibge == ibge_code,]
  
  ap_total <- ap_territorial[ap_territorial$codigo_ibge == ibge_code,]
  
  censo2010_1 <- censo2010_1 %>%
    mutate(var11 = case_when(V2011/V6529 < 0.3 | V6529 > 1530 ~ "Adequada",
                             V2011/V6529 >= 0.3 & V6529 <= 1530 ~ "Inadequada"),
           var21 = case_when(V4001 != 5 ~ "Adequada",
                             V4001 == 5 ~ "Inadequada"),
           var22 = case_when(V4002 %in% c(11, 12, 13, 15) ~ "Adequada",
                             V4002 %in% c(14, 51, 52, 53) ~ "Inadequada"),
           var23 = case_when(V0202 %in% c(1, 2, 3, 4) ~ "Adequada",
                             V0202 %in% c(5, 6, 7, 8, 9) ~ "Inadequada"),
           var24 = case_when(V0205 %in% c(1, 2, 3, 4, 5, 6, 7, 9) | V0206 == 1 ~ "Adequada",
                             V0205 %in% c(0) | V0206 == 2 ~ "Inadequada"),
           var31 = case_when(V6204 <= 3 ~ "Adequada",
                             V6204 > 3 ~ "Inadequada"),
           var32 = case_when(V6204 <= 3 & V5020 <= 1 ~ "Adequada",
                             V6204 > 3 & V5020 > 1 ~ "Inadequada"),
           var33 = case_when(V6204 <= 3 & V4002 == 13 ~ "Adequada",
                             V6204 > 3 & V4002 == 13 ~ "Inadequada"),
           var34 = case_when(V6204 <= 3 & V0201 %in% c(1, 2) ~ "Adequada",
                             V6204 > 3 & V0201 %in% c(3, 4, 5, 6) ~ "Inadequada"),
           var61 = case_when(V0207 %in% c(1, 2) ~ "Adequada",
                             V0207 %in% c(3, 4, 5, 6) ~ "Inadequada"),
           var62 = case_when(V0208 == 1 ~ "Adequada",
                             V0208 %in% c(2, 3, 4, 5, 6, 7) ~ "Inadequada"),
           var63 = case_when(V0209 == 1 ~ "Adequada",
                             V0209 %in% c(2, 3) ~ "Inadequada"),
           var64 = case_when(V0210 %in% c(1, 2) ~ "Adequada",
                             V0210 %in% c(3, 4, 5, 6, 7) ~ "Inadequada"),
           var65 = case_when(V0211 %in% c(1, 2) ~ "Adequada",
                             V0211 == 3 ~ "Inadequada"),
           deficit_auxiliar = ifelse(var21 == "Inadequada" | var23 == "Inadequada" | var32 == "Inadequada" | var33 == "Inadequada" | var34 == "Inadequada", 1, 2),
           inadequacao_auxiliar = ifelse(var11 == "Inadequada" | var22 == "Inadequada" | var24 == "Inadequada" | var31 == "Inadequada" | var61 == "Inadequada" | var62 == "Inadequada" | var63 == "Inadequada" | var64  == "Inadequada" | var65 == "Inadequada", 1, 2),
           deficit_ou_inadequacao = case_when(deficit_auxiliar == 1 ~ "Deficit",
                                              inadequacao_auxiliar == 1 ~ "Inadequacao")) %>%
    select(codigo_ibge, V0010, V0011, fpc, 
           var11, 
           var21, var22, var23, var24, 
           var31, var32, var33, var34, 
           var61, var62, var63, var64, var65, 
           deficit_ou_inadequacao)
  
  censo2010_1$V0011 <- as.character(censo2010_1$V0011)
  
  censo2010_2 <- censo2010_1 %>%
    filter(deficit_ou_inadequacao %in% c("Deficit", "Inadequacao")) %>%
    mutate(f01 = ifelse(V0011 %in% ap_ap$V0011, NA, 1),
           f01 = ifelse(is.na(f01) & var33 == "Inadequada", 2, f01),
           f01 = ifelse(is.na(f01) & var23 == "Inadequada" & var22 != "Inadequada", 3, f01),
           f01 = ifelse(is.na(f01) & var24 == "Inadequada" & var22 != "Inadequada", 4, f01),
           f01 = ifelse(is.na(f01) & var65 == "Inadequada", 5, f01),
           f01 = ifelse(is.na(f01) & var62 == "Inadequada", 6, f01),
           f01 = ifelse(is.na(f01) & var63 == "Inadequada" & var22 != "Inadequada", 7, f01),
           f01 = ifelse(is.na(f01) & var64 == "Inadequada", 8, f01),
           f01 = ifelse(is.na(f01) & var61 == "Inadequada", 9, f01),
           f01 = ifelse(is.na(f01) & var21 == "Inadequada", 11, f01),
           f01 = ifelse(is.na(f01) & var22 == "Inadequada", 10, f01),
           f01 = ifelse(is.na(f01) & var32 == "Inadequada", 12, f01),
           f01 = ifelse(is.na(f01) & var34 == "Inadequada", 13, f01),
           f01 = ifelse(is.na(f01) & var31 == "Inadequada", 14, f01),
           f01 = ifelse(is.na(f01) & var11 == "Inadequada", 15, f01))
  
  dentro_ou_fora <- tibble(c("fora", "fora", "dentro", "dentro", "dentro", "dentro", "dentro", "dentro", "dentro", "fora", "fora", "proporcional", "proporcional", "proporcional", "proporcional"),
                           c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
  names(dentro_ou_fora) <- c("dentro_ou_fora", "f01")
  
  censo2010_2[is.na(censo2010_2)] = "Sem dados"
  
  # Aqui e definido um parametro de pesquisa adicional, para ponderacao da pesquisa amostral (correcao de populacao finita)
  
  # Cria um objeto de pesquisa amostral
  censo2010_2_ponderado <- svydesign(data = censo2010_2,
                                     ids = ~1,
                                     strata = ~V0011,
                                     fpc = ~fpc,
                                     weights = ~V0010)
  
  censo2010_1_ponderado <- svydesign(data = censo2010_1,
                                     ids = ~1,
                                     strata = ~V0011,
                                     fpc = ~fpc,
                                     weights = ~V0010)
  
  # calcula as proporcoes
  var11 <- as.data.frame(svytable(~ var11, censo2010_2_ponderado))
  var31 <- as.data.frame(svytable(~ var31, censo2010_2_ponderado))
  var32 <- as.data.frame(svytable(~ var32, censo2010_2_ponderado))
  var34 <- as.data.frame(svytable(~ var34, censo2010_2_ponderado))
  total_de_domicilios <- as.data.frame(svytable(~ codigo_ibge, censo2010_1_ponderado))
  
  prop11 <- var11[2,2]/(var11[2,2] + var31[2,2] + var32[2,2] + var34[2,2])
  prop31 <- var31[2,2]/(var11[2,2] + var31[2,2] + var32[2,2] + var34[2,2])
  prop32 <- var32[2,2]/(var11[2,2] + var31[2,2] + var32[2,2] + var34[2,2])
  prop34 <- var34[2,2]/(var11[2,2] + var31[2,2] + var32[2,2] + var34[2,2])
  
  prop1 <- ap_total$Total/proj_seade$DOM
  prop2 <- proj_seade$DOM/total_de_domicilios$Freq
  
  # Faz a contagem de domicilios a partir do objeto de pesquisa amostral
  f01_2 <- as.data.frame(svytable(~ deficit_ou_inadequacao + f01, censo2010_2_ponderado)) %>%
    spread(deficit_ou_inadequacao, Freq) %>%
    mutate(f01 = as.numeric(as.character(f01))) %>%
    complete(f01 = full_seq(1:15, 1)) %>%
    mutate(Deficit = Deficit * prop2,
           Inadequacao = Inadequacao * prop2)
  f01_2[is.na(f01_2)] = 0
  
  f01 <- as.data.frame(svytable(~ f01, censo2010_2_ponderado))
  f01 <- f01 %>% 
    #  spread(deficit_ou_inadequacao, Freq) %>%
    mutate(f01 = as.numeric(as.character(f01))) %>%
    complete(f01 = full_seq(1:15, 1)) %>%
    mutate(Freq = Freq * prop2) %>%
    inner_join(f01_2)
  f01 <- bind_cols(f01, dentro_ou_fora = dentro_ou_fora$dentro_ou_fora)
  f01[is.na(f01)] = 0
  
  f01 <- f01 %>%
    mutate(fora = case_when(dentro_ou_fora == "fora" ~ Freq,
                            dentro_ou_fora == "proporcional" ~ (1-prop1) * Freq),
           dentro = case_when(dentro_ou_fora == "dentro" ~ Freq,
                              dentro_ou_fora == "proporcional" ~ prop1 * Freq))
  f01[is.na(f01)] = 0
  
  f01[1,]$fora <- ifelse((sum(f01[1:1,]$dentro)) <= ap_total$Total, f01[1,]$fora, f01[1,]$fora + (f01[1,]$dentro) - (ap_total$Total - sum(f01[1:1,]$dentro)))
  f01[1,]$dentro <- ifelse((sum(f01[1:1,]$dentro)) <= ap_total$Total, f01[1,]$dentro, (ap_total$Total - sum(f01[1:1,]$dentro)))
  
  f01[2,]$fora <- ifelse((sum(f01[1:2,]$dentro)) <= ap_total$Total, f01[2,]$fora, f01[2,]$fora + (f01[2,]$dentro) - (ap_total$Total - sum(f01[1:1,]$dentro)))
  f01[2,]$dentro <- ifelse((sum(f01[1:2,]$dentro)) <= ap_total$Total, f01[2,]$dentro, (ap_total$Total - sum(f01[1:1,]$dentro)))
  
  f01[3,]$fora <- ifelse((sum(f01[1:3,]$dentro)) <= ap_total$Total, f01[3,]$fora, f01[3,]$fora + (f01[3,]$dentro) - (ap_total$Total - sum(f01[1:2,]$dentro)))
  f01[3,]$dentro <- ifelse((sum(f01[1:3,]$dentro)) <= ap_total$Total, f01[3,]$dentro, (ap_total$Total - sum(f01[1:2,]$dentro)))
  
  f01[4,]$fora <- ifelse((sum(f01[1:4,]$dentro)) <= ap_total$Total, f01[4,]$fora, f01[4,]$fora + (f01[4,]$dentro) - (ap_total$Total - sum(f01[1:3,]$dentro)))
  f01[4,]$dentro <- ifelse((sum(f01[1:4,]$dentro)) <= ap_total$Total, f01[4,]$dentro, (ap_total$Total - sum(f01[1:3,]$dentro)))
  
  f01[5,]$fora <- ifelse((sum(f01[1:5,]$dentro)) <= ap_total$Total, f01[5,]$fora, f01[5,]$fora + (f01[5,]$dentro) - (ap_total$Total - sum(f01[1:4,]$dentro)))
  f01[5,]$dentro <- ifelse((sum(f01[1:5,]$dentro)) <= ap_total$Total, f01[5,]$dentro, (ap_total$Total - sum(f01[1:4,]$dentro)))
  
  f01[6,]$fora <- ifelse((sum(f01[1:6,]$dentro)) <= ap_total$Total, f01[6,]$fora, f01[6,]$fora + (f01[6,]$dentro) - (ap_total$Total - sum(f01[1:5,]$dentro)))
  f01[6,]$dentro <- ifelse((sum(f01[1:6,]$dentro)) <= ap_total$Total, f01[6,]$dentro, (ap_total$Total - sum(f01[1:5,]$dentro)))
  
  f01[7,]$fora <- ifelse((sum(f01[1:7,]$dentro)) <= ap_total$Total, f01[7,]$fora, f01[7,]$fora + (f01[7,]$dentro) - (ap_total$Total - sum(f01[1:6,]$dentro)))
  f01[7,]$dentro <- ifelse((sum(f01[1:7,]$dentro)) <= ap_total$Total, f01[7,]$dentro, (ap_total$Total - sum(f01[1:6,]$dentro)))
  
  f01[8,]$fora <- ifelse((sum(f01[1:8,]$dentro)) <= ap_total$Total, f01[8,]$fora, f01[8,]$fora + (f01[8,]$dentro) - (ap_total$Total - sum(f01[1:7,]$dentro)))
  f01[8,]$dentro <- ifelse((sum(f01[1:8,]$dentro)) <= ap_total$Total, f01[8,]$dentro, (ap_total$Total - sum(f01[1:7,]$dentro)))
  
  f01[9,]$fora <- ifelse((sum(f01[1:9,]$dentro)) <= ap_total$Total, f01[9,]$fora, f01[9,]$fora + (f01[9,]$dentro) - (ap_total$Total - sum(f01[1:8,]$dentro)))
  f01[9,]$dentro <- ifelse((sum(f01[1:9,]$dentro)) <= ap_total$Total, f01[9,]$dentro, (ap_total$Total - sum(f01[1:8,]$dentro)))
  
  f01[10,]$fora <- ifelse((sum(f01[1:10,]$dentro)) <= ap_total$Total, f01[10,]$fora, f01[10,]$fora + (f01[10,]$dentro) - (ap_total$Total - sum(f01[1:9,]$dentro)))
  f01[10,]$dentro <- ifelse((sum(f01[1:10,]$dentro)) <= ap_total$Total, f01[10,]$dentro, (ap_total$Total - sum(f01[1:9,]$dentro)))
  
  f01[11,]$fora <- ifelse((sum(f01[1:11,]$dentro)) <= ap_total$Total, f01[11,]$fora, f01[11,]$fora + (f01[11,]$dentro) - (ap_total$Total - sum(f01[1:10,]$dentro)))
  f01[11,]$dentro <- ifelse((sum(f01[1:11,]$dentro)) <= ap_total$Total, f01[11,]$dentro, (ap_total$Total - sum(f01[1:10,]$dentro)))
  
  
  
  f01[12,]$dentro <- ifelse(sum(f01$dentro) >= ap_total$Total, (ap_total$Total - sum(f01[1:11,]$dentro)) * prop32, f01[12,]$dentro)
  f01[12,]$fora <- ifelse(sum(f01$dentro) >= ap_total$Total, f01[12,]$Freq - f01[12,]$dentro, f01[12,]$fora)
  
  f01[13,]$dentro <- ifelse(sum(f01$dentro) >= ap_total$Total, (ap_total$Total - sum(f01[1:11,]$dentro)) * prop34, f01[13,]$dentro)
  f01[13,]$fora <- ifelse(sum(f01$dentro) >= ap_total$Total, f01[13,]$Freq - f01[13,]$dentro, f01[13,]$fora)
  
  f01[14,]$dentro <- ifelse(sum(f01$dentro) >= ap_total$Total, (ap_total$Total - sum(f01[1:11,]$dentro)) * prop31, f01[14,]$dentro)
  f01[14,]$fora <- ifelse(sum(f01$dentro) >= ap_total$Total, f01[14,]$Freq - f01[14,]$dentro, f01[14,]$fora)
  
  f01[15,]$dentro <- ifelse(sum(f01$dentro) >= ap_total$Total, (ap_total$Total - sum(f01[1:11,]$dentro)) * prop11, f01[15,]$dentro)
  f01[15,]$fora <- ifelse(sum(f01$dentro) >= ap_total$Total, f01[15,]$Freq - f01[15,]$dentro, f01[15,]$fora)
  
  
  
  #f01[12,]$fora <- ifelse((sum(f01[1:12,]$dentro)) <= ap_total$Total, f01[12,]$fora, f01[12,]$fora + (f01[12,]$dentro) - (ap_total$Total - sum(f01[1:11,]$dentro)))
  #f01[12,]$dentro <- ifelse((sum(f01[1:12,]$dentro)) <= ap_total$Total, f01[12,]$dentro, (ap_total$Total - sum(f01[1:11,]$dentro)))
  
  #f01[13,]$fora <- ifelse((sum(f01[1:13,]$dentro)) <= ap_total$Total, f01[13,]$fora, f01[13,]$fora + (f01[13,]$dentro) - (ap_total$Total - sum(f01[1:12,]$dentro)))
  #f01[13,]$dentro <- ifelse((sum(f01[1:13,]$dentro)) <= ap_total$Total, f01[13,]$dentro, (ap_total$Total - sum(f01[1:12,]$dentro)))
  
  #f01[14,]$fora <- ifelse((sum(f01[1:14,]$dentro)) <= ap_total$Total, f01[14,]$fora, f01[14,]$fora + (f01[14,]$dentro) - (ap_total$Total - sum(f01[1:13,]$dentro)))
  #f01[14,]$dentro <- ifelse((sum(f01[1:14,]$dentro)) <= ap_total$Total, f01[14,]$dentro, (ap_total$Total - sum(f01[1:13,]$dentro)))
  
  #f01[15,]$fora <- ifelse((sum(f01[1:15,]$dentro)) <= ap_total$Total, f01[15,]$fora, f01[15,]$fora + (f01[15,]$dentro) - (ap_total$Total - sum(f01[1:14,]$dentro)))
  #f01[15,]$dentro <- ifelse((sum(f01[1:15,]$dentro)) <= ap_total$Total, f01[15,]$dentro, (ap_total$Total - sum(f01[1:14,]$dentro)))
  
  f01
  
  
  
  
  
  
  
  f01_final <- f01 %>%
    rename(Variavel = f01,
           Alocacao = dentro_ou_fora,
           Fora = fora,
           Dentro = dentro) %>%
    mutate(deficit_fora = ifelse(Fora > 0, Fora * (Deficit/(Deficit + Inadequacao)), 0),
           inadequacao_fora = ifelse(Fora > 0, Fora * (Inadequacao/(Deficit + Inadequacao)), 0),
           deficit_dentro = ifelse(Dentro > 0, Dentro * (Deficit/(Deficit + Inadequacao)), 0),
           inadequacao_dentro = ifelse(Dentro > 0, Dentro * (Inadequacao/(Deficit + Inadequacao)), 0)) %>%
    mutate(`Deficit + Inadequacao` = Deficit + Inadequacao) %>%
    select(Alocacao, Variavel, deficit_fora, inadequacao_fora, Fora, deficit_dentro, inadequacao_dentro, Dentro, Deficit, Inadequacao, `Deficit + Inadequacao`)
  
  f01_final2 <- f01_final %>%
    add_row(Alocacao = "territorial",
            Variavel = 16,
            deficit_dentro = ifelse(ap_total$Deficit > sum(f01_final$deficit_dentro), ap_total$Deficit - sum(f01_final$deficit_dentro), 0),
            inadequacao_dentro = ifelse(ap_total$Inadequacao > sum(f01_final$inadequacao_dentro), ap_total$Inadequacao - sum(f01_final$inadequacao_dentro), 0),
            #deficit_dentro = ifelse(ap_total$Deficit > sum(f01_final$deficit_dentro), ap_total$Deficit - sum(f01_final$deficit_dentro), f01_final$deficit_dentro),
            #inadequacao_dentro = ifelse(ap_total$Inadequacao > sum(f01_final$inadequacao_dentro), ap_total$Inadequacao, f01_final$inadequacao_dentro),
            #deficit_dentro = ifelse(ap_total$Total > sum(f01_final$Dentro), (ap_total$Total - sum(f01_final$Dentro)) * (ap_total$Deficit/(ap_total$Total)), 0),
            #inadequacao_dentro = ifelse(ap_total$Total > sum(f01_final$Dentro), (ap_total$Total - sum(f01_final$Dentro)) * (ap_total$Inadequacao/(ap_total$Total)), 0),
            Dentro = deficit_dentro + inadequacao_dentro,
            Deficit = deficit_dentro,
            Inadequacao = inadequacao_dentro,
            `Deficit + Inadequacao` = Deficit + Inadequacao)
  
  f01_final2[is.na(f01_final2)] = 0
  
  p02 <- f01_final %>%
    as_tabyl() %>%
    adorn_rounding(digits = 0) %>%
    adorn_totals(where = "row")
  
  p03 <- f01_final2 %>%
    as_tabyl() %>%
    adorn_rounding(digits = 0) %>%
    adorn_totals(where = "row")
  
  
  p01 <- f01 %>%
    rename(Variavel = f01,
           Total = Freq,
           Fora = fora,
           Dentro = dentro,
           Alocacao = dentro_ou_fora) %>%
    select(Variavel, Alocacao, Deficit, Inadequacao, Total, Fora, Dentro) %>%
    as_tabyl() %>%
    adorn_rounding(digits = 0) %>%
    adorn_totals(where = "row")
  
  
  return(p01)
}

compatibilizacao2 <- function(data, ibge_code, ap_territorial, ap_ap, projecao_seade){
  
  proj_seade <- projecao_seade %>%
    filter(mun_id == ibge_code) %>%
    filter(ano == 2018)
  
  censo2010_1 <- data[data$codigo_ibge == ibge_code,]
  
  ap_total <- ap_territorial[ap_territorial$codigo_ibge == ibge_code,]
  
  censo2010_1 <- censo2010_1 %>%
    mutate(var11 = case_when(V2011/V6529 < 0.3 | V6529 > 1530 ~ "Adequada",
                             V2011/V6529 >= 0.3 & V6529 <= 1530 ~ "Inadequada"),
           var21 = case_when(V4001 != 5 ~ "Adequada",
                             V4001 == 5 ~ "Inadequada"),
           var22 = case_when(V4002 %in% c(11, 12, 13, 15) ~ "Adequada",
                             V4002 %in% c(14, 51, 52, 53) ~ "Inadequada"),
           var23 = case_when(V0202 %in% c(1, 2, 3, 4) ~ "Adequada",
                             V0202 %in% c(5, 6, 7, 8, 9) ~ "Inadequada"),
           var24 = case_when(V0205 %in% c(1, 2, 3, 4, 5, 6, 7, 9) | V0206 == 1 ~ "Adequada",
                             V0205 %in% c(0) | V0206 == 2 ~ "Inadequada"),
           var31 = case_when(V6204 <= 3 ~ "Adequada",
                             V6204 > 3 ~ "Inadequada"),
           var32 = case_when(V6204 <= 3 & V5020 <= 1 ~ "Adequada",
                             V6204 > 3 & V5020 > 1 ~ "Inadequada"),
           var33 = case_when(V6204 <= 3 & V4002 == 13 ~ "Adequada",
                             V6204 > 3 & V4002 == 13 ~ "Inadequada"),
           var34 = case_when(V6204 <= 3 & V0201 %in% c(1, 2) ~ "Adequada",
                             V6204 > 3 & V0201 %in% c(3, 4, 5, 6) ~ "Inadequada"),
           var61 = case_when(V0207 %in% c(1, 2) ~ "Adequada",
                             V0207 %in% c(3, 4, 5, 6) ~ "Inadequada"),
           var62 = case_when(V0208 == 1 ~ "Adequada",
                             V0208 %in% c(2, 3, 4, 5, 6, 7) ~ "Inadequada"),
           var63 = case_when(V0209 == 1 ~ "Adequada",
                             V0209 %in% c(2, 3) ~ "Inadequada"),
           var64 = case_when(V0210 %in% c(1, 2) ~ "Adequada",
                             V0210 %in% c(3, 4, 5, 6, 7) ~ "Inadequada"),
           var65 = case_when(V0211 %in% c(1, 2) ~ "Adequada",
                             V0211 == 3 ~ "Inadequada"),
           deficit_auxiliar = ifelse(var21 == "Inadequada" | var23 == "Inadequada" | var32 == "Inadequada" | var33 == "Inadequada" | var34 == "Inadequada", 1, 2),
           inadequacao_auxiliar = ifelse(var11 == "Inadequada" | var22 == "Inadequada" | var24 == "Inadequada" | var31 == "Inadequada" | var61 == "Inadequada" | var62 == "Inadequada" | var63 == "Inadequada" | var64  == "Inadequada" | var65 == "Inadequada", 1, 2),
           deficit_ou_inadequacao = case_when(deficit_auxiliar == 1 ~ "Deficit",
                                              inadequacao_auxiliar == 1 ~ "Inadequacao")) %>%
    select(codigo_ibge, V0010, V0011, fpc, 
           var11, 
           var21, var22, var23, var24, 
           var31, var32, var33, var34, 
           var61, var62, var63, var64, var65, 
           deficit_ou_inadequacao)
  
  censo2010_1$V0011 <- as.character(censo2010_1$V0011)
  
  censo2010_2 <- censo2010_1 %>%
    filter(deficit_ou_inadequacao %in% c("Deficit", "Inadequacao")) %>%
    mutate(f01 = ifelse(V0011 %in% ap_ap$V0011, NA, 1),
           f01 = ifelse(is.na(f01) & var33 == "Inadequada", 2, f01),
           f01 = ifelse(is.na(f01) & var23 == "Inadequada" & var22 != "Inadequada", 3, f01),
           f01 = ifelse(is.na(f01) & var24 == "Inadequada" & var22 != "Inadequada", 4, f01),
           f01 = ifelse(is.na(f01) & var65 == "Inadequada", 5, f01),
           f01 = ifelse(is.na(f01) & var62 == "Inadequada", 6, f01),
           f01 = ifelse(is.na(f01) & var63 == "Inadequada" & var22 != "Inadequada", 7, f01),
           f01 = ifelse(is.na(f01) & var64 == "Inadequada", 8, f01),
           f01 = ifelse(is.na(f01) & var61 == "Inadequada", 9, f01),
           f01 = ifelse(is.na(f01) & var21 == "Inadequada", 11, f01),
           f01 = ifelse(is.na(f01) & var22 == "Inadequada", 10, f01),
           f01 = ifelse(is.na(f01) & var32 == "Inadequada", 12, f01),
           f01 = ifelse(is.na(f01) & var34 == "Inadequada", 13, f01),
           f01 = ifelse(is.na(f01) & var31 == "Inadequada", 14, f01),
           f01 = ifelse(is.na(f01) & var11 == "Inadequada", 15, f01))
  
  dentro_ou_fora <- tibble(c("fora", "fora", "dentro", "dentro", "dentro", "dentro", "dentro", "dentro", "dentro", "fora", "fora", "proporcional", "proporcional", "proporcional", "proporcional"),
                           c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
  names(dentro_ou_fora) <- c("dentro_ou_fora", "f01")
  
  censo2010_2[is.na(censo2010_2)] = "Sem dados"
  
  # Aqui e definido um parametro de pesquisa adicional, para ponderacao da pesquisa amostral (correcao de populacao finita)
  
  # Cria um objeto de pesquisa amostral
  censo2010_2_ponderado <- svydesign(data = censo2010_2,
                                     ids = ~1,
                                     strata = ~V0011,
                                     fpc = ~fpc,
                                     weights = ~V0010)
  
  censo2010_1_ponderado <- svydesign(data = censo2010_1,
                                     ids = ~1,
                                     strata = ~V0011,
                                     fpc = ~fpc,
                                     weights = ~V0010)
  
  # calcula as proporcoes
  var11 <- as.data.frame(svytable(~ var11, censo2010_2_ponderado))
  var31 <- as.data.frame(svytable(~ var31, censo2010_2_ponderado))
  var32 <- as.data.frame(svytable(~ var32, censo2010_2_ponderado))
  var34 <- as.data.frame(svytable(~ var34, censo2010_2_ponderado))
  total_de_domicilios <- as.data.frame(svytable(~ codigo_ibge, censo2010_1_ponderado))
  
  prop11 <- var11[2,2]/(var11[2,2] + var31[2,2] + var32[2,2] + var34[2,2])
  prop31 <- var31[2,2]/(var11[2,2] + var31[2,2] + var32[2,2] + var34[2,2])
  prop32 <- var32[2,2]/(var11[2,2] + var31[2,2] + var32[2,2] + var34[2,2])
  prop34 <- var34[2,2]/(var11[2,2] + var31[2,2] + var32[2,2] + var34[2,2])
  
  prop1 <- ap_total$Total/proj_seade$DOM
  prop2 <- proj_seade$DOM/total_de_domicilios$Freq
  
  # Faz a contagem de domicilios a partir do objeto de pesquisa amostral
  f01_2 <- as.data.frame(svytable(~ deficit_ou_inadequacao + f01, censo2010_2_ponderado)) %>%
    spread(deficit_ou_inadequacao, Freq) %>%
    mutate(f01 = as.numeric(as.character(f01))) %>%
    complete(f01 = full_seq(1:15, 1)) %>%
    mutate(Deficit = Deficit * prop2,
           Inadequacao = Inadequacao * prop2)
  f01_2[is.na(f01_2)] = 0
  
  f01 <- as.data.frame(svytable(~ f01, censo2010_2_ponderado))
  f01 <- f01 %>% 
    #  spread(deficit_ou_inadequacao, Freq) %>%
    mutate(f01 = as.numeric(as.character(f01))) %>%
    complete(f01 = full_seq(1:15, 1)) %>%
    mutate(Freq = Freq * prop2) %>%
    inner_join(f01_2)
  f01 <- bind_cols(f01, dentro_ou_fora = dentro_ou_fora$dentro_ou_fora)
  f01[is.na(f01)] = 0
  
  f01 <- f01 %>%
    mutate(fora = case_when(dentro_ou_fora == "fora" ~ Freq,
                            dentro_ou_fora == "proporcional" ~ (1-prop1) * Freq),
           dentro = case_when(dentro_ou_fora == "dentro" ~ Freq,
                              dentro_ou_fora == "proporcional" ~ prop1 * Freq))
  f01[is.na(f01)] = 0
  
  f01[1,]$fora <- ifelse((sum(f01[1:1,]$dentro)) <= ap_total$Total, f01[1,]$fora, f01[1,]$fora + (f01[1,]$dentro) - (ap_total$Total - sum(f01[1:1,]$dentro)))
  f01[1,]$dentro <- ifelse((sum(f01[1:1,]$dentro)) <= ap_total$Total, f01[1,]$dentro, (ap_total$Total - sum(f01[1:1,]$dentro)))
  
  f01[2,]$fora <- ifelse((sum(f01[1:2,]$dentro)) <= ap_total$Total, f01[2,]$fora, f01[2,]$fora + (f01[2,]$dentro) - (ap_total$Total - sum(f01[1:1,]$dentro)))
  f01[2,]$dentro <- ifelse((sum(f01[1:2,]$dentro)) <= ap_total$Total, f01[2,]$dentro, (ap_total$Total - sum(f01[1:1,]$dentro)))
  
  f01[3,]$fora <- ifelse((sum(f01[1:3,]$dentro)) <= ap_total$Total, f01[3,]$fora, f01[3,]$fora + (f01[3,]$dentro) - (ap_total$Total - sum(f01[1:2,]$dentro)))
  f01[3,]$dentro <- ifelse((sum(f01[1:3,]$dentro)) <= ap_total$Total, f01[3,]$dentro, (ap_total$Total - sum(f01[1:2,]$dentro)))
  
  f01[4,]$fora <- ifelse((sum(f01[1:4,]$dentro)) <= ap_total$Total, f01[4,]$fora, f01[4,]$fora + (f01[4,]$dentro) - (ap_total$Total - sum(f01[1:3,]$dentro)))
  f01[4,]$dentro <- ifelse((sum(f01[1:4,]$dentro)) <= ap_total$Total, f01[4,]$dentro, (ap_total$Total - sum(f01[1:3,]$dentro)))
  
  f01[5,]$fora <- ifelse((sum(f01[1:5,]$dentro)) <= ap_total$Total, f01[5,]$fora, f01[5,]$fora + (f01[5,]$dentro) - (ap_total$Total - sum(f01[1:4,]$dentro)))
  f01[5,]$dentro <- ifelse((sum(f01[1:5,]$dentro)) <= ap_total$Total, f01[5,]$dentro, (ap_total$Total - sum(f01[1:4,]$dentro)))
  
  f01[6,]$fora <- ifelse((sum(f01[1:6,]$dentro)) <= ap_total$Total, f01[6,]$fora, f01[6,]$fora + (f01[6,]$dentro) - (ap_total$Total - sum(f01[1:5,]$dentro)))
  f01[6,]$dentro <- ifelse((sum(f01[1:6,]$dentro)) <= ap_total$Total, f01[6,]$dentro, (ap_total$Total - sum(f01[1:5,]$dentro)))
  
  f01[7,]$fora <- ifelse((sum(f01[1:7,]$dentro)) <= ap_total$Total, f01[7,]$fora, f01[7,]$fora + (f01[7,]$dentro) - (ap_total$Total - sum(f01[1:6,]$dentro)))
  f01[7,]$dentro <- ifelse((sum(f01[1:7,]$dentro)) <= ap_total$Total, f01[7,]$dentro, (ap_total$Total - sum(f01[1:6,]$dentro)))
  
  f01[8,]$fora <- ifelse((sum(f01[1:8,]$dentro)) <= ap_total$Total, f01[8,]$fora, f01[8,]$fora + (f01[8,]$dentro) - (ap_total$Total - sum(f01[1:7,]$dentro)))
  f01[8,]$dentro <- ifelse((sum(f01[1:8,]$dentro)) <= ap_total$Total, f01[8,]$dentro, (ap_total$Total - sum(f01[1:7,]$dentro)))
  
  f01[9,]$fora <- ifelse((sum(f01[1:9,]$dentro)) <= ap_total$Total, f01[9,]$fora, f01[9,]$fora + (f01[9,]$dentro) - (ap_total$Total - sum(f01[1:8,]$dentro)))
  f01[9,]$dentro <- ifelse((sum(f01[1:9,]$dentro)) <= ap_total$Total, f01[9,]$dentro, (ap_total$Total - sum(f01[1:8,]$dentro)))
  
  f01[10,]$fora <- ifelse((sum(f01[1:10,]$dentro)) <= ap_total$Total, f01[10,]$fora, f01[10,]$fora + (f01[10,]$dentro) - (ap_total$Total - sum(f01[1:9,]$dentro)))
  f01[10,]$dentro <- ifelse((sum(f01[1:10,]$dentro)) <= ap_total$Total, f01[10,]$dentro, (ap_total$Total - sum(f01[1:9,]$dentro)))
  
  f01[11,]$fora <- ifelse((sum(f01[1:11,]$dentro)) <= ap_total$Total, f01[11,]$fora, f01[11,]$fora + (f01[11,]$dentro) - (ap_total$Total - sum(f01[1:10,]$dentro)))
  f01[11,]$dentro <- ifelse((sum(f01[1:11,]$dentro)) <= ap_total$Total, f01[11,]$dentro, (ap_total$Total - sum(f01[1:10,]$dentro)))
  
  
  
  f01[12,]$dentro <- ifelse(sum(f01$dentro) >= ap_total$Total, (ap_total$Total - sum(f01[1:11,]$dentro)) * prop32, f01[12,]$dentro)
  f01[12,]$fora <- ifelse(sum(f01$dentro) >= ap_total$Total, f01[12,]$Freq - f01[12,]$dentro, f01[12,]$fora)
  
  f01[13,]$dentro <- ifelse(sum(f01$dentro) >= ap_total$Total, (ap_total$Total - sum(f01[1:11,]$dentro)) * prop34, f01[13,]$dentro)
  f01[13,]$fora <- ifelse(sum(f01$dentro) >= ap_total$Total, f01[13,]$Freq - f01[13,]$dentro, f01[13,]$fora)
  
  f01[14,]$dentro <- ifelse(sum(f01$dentro) >= ap_total$Total, (ap_total$Total - sum(f01[1:11,]$dentro)) * prop31, f01[14,]$dentro)
  f01[14,]$fora <- ifelse(sum(f01$dentro) >= ap_total$Total, f01[14,]$Freq - f01[14,]$dentro, f01[14,]$fora)
  
  f01[15,]$dentro <- ifelse(sum(f01$dentro) >= ap_total$Total, (ap_total$Total - sum(f01[1:11,]$dentro)) * prop11, f01[15,]$dentro)
  f01[15,]$fora <- ifelse(sum(f01$dentro) >= ap_total$Total, f01[15,]$Freq - f01[15,]$dentro, f01[15,]$fora)
  
  
  
  #f01[12,]$fora <- ifelse((sum(f01[1:12,]$dentro)) <= ap_total$Total, f01[12,]$fora, f01[12,]$fora + (f01[12,]$dentro) - (ap_total$Total - sum(f01[1:11,]$dentro)))
  #f01[12,]$dentro <- ifelse((sum(f01[1:12,]$dentro)) <= ap_total$Total, f01[12,]$dentro, (ap_total$Total - sum(f01[1:11,]$dentro)))
  
  #f01[13,]$fora <- ifelse((sum(f01[1:13,]$dentro)) <= ap_total$Total, f01[13,]$fora, f01[13,]$fora + (f01[13,]$dentro) - (ap_total$Total - sum(f01[1:12,]$dentro)))
  #f01[13,]$dentro <- ifelse((sum(f01[1:13,]$dentro)) <= ap_total$Total, f01[13,]$dentro, (ap_total$Total - sum(f01[1:12,]$dentro)))
  
  #f01[14,]$fora <- ifelse((sum(f01[1:14,]$dentro)) <= ap_total$Total, f01[14,]$fora, f01[14,]$fora + (f01[14,]$dentro) - (ap_total$Total - sum(f01[1:13,]$dentro)))
  #f01[14,]$dentro <- ifelse((sum(f01[1:14,]$dentro)) <= ap_total$Total, f01[14,]$dentro, (ap_total$Total - sum(f01[1:13,]$dentro)))
  
  #f01[15,]$fora <- ifelse((sum(f01[1:15,]$dentro)) <= ap_total$Total, f01[15,]$fora, f01[15,]$fora + (f01[15,]$dentro) - (ap_total$Total - sum(f01[1:14,]$dentro)))
  #f01[15,]$dentro <- ifelse((sum(f01[1:15,]$dentro)) <= ap_total$Total, f01[15,]$dentro, (ap_total$Total - sum(f01[1:14,]$dentro)))
  
  f01
  
  
  
  
  
  
  
  f01_final <- f01 %>%
    rename(Variavel = f01,
           Alocacao = dentro_ou_fora,
           Fora = fora,
           Dentro = dentro) %>%
    mutate(deficit_fora = ifelse(Fora > 0, Fora * (Deficit/(Deficit + Inadequacao)), 0),
           inadequacao_fora = ifelse(Fora > 0, Fora * (Inadequacao/(Deficit + Inadequacao)), 0),
           deficit_dentro = ifelse(Dentro > 0, Dentro * (Deficit/(Deficit + Inadequacao)), 0),
           inadequacao_dentro = ifelse(Dentro > 0, Dentro * (Inadequacao/(Deficit + Inadequacao)), 0)) %>%
    mutate(`Deficit + Inadequacao` = Deficit + Inadequacao) %>%
    select(Alocacao, Variavel, deficit_fora, inadequacao_fora, Fora, deficit_dentro, inadequacao_dentro, Dentro, Deficit, Inadequacao, `Deficit + Inadequacao`)
  
  f01_final2 <- f01_final %>%
    add_row(Alocacao = "territorial",
            Variavel = 16,
            #deficit_dentro = ap_total$Deficit - sum(f01_final$deficit_dentro),
            #inadequacao_dentro = ap_total$Inadequacao - sum(f01_final$inadequacao_dentro),
            deficit_dentro = ifelse(ap_total$Deficit > sum(f01_final$deficit_dentro), ap_total$Deficit - sum(f01_final$deficit_dentro), 0),
            inadequacao_dentro = ifelse(ap_total$Inadequacao > sum(f01_final$inadequacao_dentro), ap_total$Inadequacao - sum(f01_final$inadequacao_dentro), 0),
            #deficit_dentro = ifelse(ap_total$Deficit > sum(f01_final$deficit_dentro), ap_total$Deficit - sum(f01_final$deficit_dentro), f01_final$deficit_dentro),
            #inadequacao_dentro = ifelse(ap_total$Inadequacao > sum(f01_final$inadequacao_dentro), ap_total$Inadequacao, f01_final$inadequacao_dentro),
            #deficit_dentro = ifelse(ap_total$Total > sum(f01_final$Dentro), (ap_total$Total - sum(f01_final$Dentro)) * (ap_total$Deficit/(ap_total$Total)), 0),
            #inadequacao_dentro = ifelse(ap_total$Total > sum(f01_final$Dentro), (ap_total$Total - sum(f01_final$Dentro)) * (ap_total$Inadequacao/(ap_total$Total)), 0),
            Dentro = deficit_dentro + inadequacao_dentro,
            Deficit = deficit_dentro,
            Inadequacao = inadequacao_dentro,
            `Deficit + Inadequacao` = Deficit + Inadequacao)
  
  f01_final2[is.na(f01_final2)] = 0
  
  p02 <- f01_final %>%
    as_tabyl() %>%
    adorn_rounding(digits = 0) %>%
    adorn_totals(where = "row")
  
  p03 <- f01_final2 %>%
    as_tabyl() %>%
    adorn_rounding(digits = 0) %>%
    adorn_totals(where = "row")
  
  p01 <- f01 %>%
    rename(Variavel = f01,
           Total = Freq,
           Fora = fora,
           Dentro = dentro,
           Alocacao = dentro_ou_fora) %>%
    select(Variavel, Alocacao, Deficit, Inadequacao, Total, Fora, Dentro) %>%
    as_tabyl() %>%
    adorn_rounding(digits = 0) %>%
    adorn_totals(where = "row")
  
  return(p02)
  
}

compatibilizacao3 <- function(data, ibge_code, ap_territorial, ap_ap, projecao_seade){
  
  proj_seade <- projecao_seade %>%
    filter(mun_id == ibge_code) %>%
    filter(ano == 2018)
  
  censo2010_1 <- data[data$codigo_ibge == ibge_code,]
  
  ap_total <- ap_territorial[ap_territorial$codigo_ibge == ibge_code,]
  
  censo2010_1 <- censo2010_1 %>%
    mutate(var11 = case_when(V2011/V6529 < 0.3 | V6529 > 1530 ~ "Adequada",
                             V2011/V6529 >= 0.3 & V6529 <= 1530 ~ "Inadequada"),
           var21 = case_when(V4001 != 5 ~ "Adequada",
                             V4001 == 5 ~ "Inadequada"),
           var22 = case_when(V4002 %in% c(11, 12, 13, 15) ~ "Adequada",
                             V4002 %in% c(14, 51, 52, 53) ~ "Inadequada"),
           var23 = case_when(V0202 %in% c(1, 2, 3, 4) ~ "Adequada",
                             V0202 %in% c(5, 6, 7, 8, 9) ~ "Inadequada"),
           var24 = case_when(V0205 %in% c(1, 2, 3, 4, 5, 6, 7, 9) | V0206 == 1 ~ "Adequada",
                             V0205 %in% c(0) | V0206 == 2 ~ "Inadequada"),
           var31 = case_when(V6204 <= 3 ~ "Adequada",
                             V6204 > 3 ~ "Inadequada"),
           var32 = case_when(V6204 <= 3 & V5020 <= 1 ~ "Adequada",
                             V6204 > 3 & V5020 > 1 ~ "Inadequada"),
           var33 = case_when(V6204 <= 3 & V4002 == 13 ~ "Adequada",
                             V6204 > 3 & V4002 == 13 ~ "Inadequada"),
           var34 = case_when(V6204 <= 3 & V0201 %in% c(1, 2) ~ "Adequada",
                             V6204 > 3 & V0201 %in% c(3, 4, 5, 6) ~ "Inadequada"),
           var61 = case_when(V0207 %in% c(1, 2) ~ "Adequada",
                             V0207 %in% c(3, 4, 5, 6) ~ "Inadequada"),
           var62 = case_when(V0208 == 1 ~ "Adequada",
                             V0208 %in% c(2, 3, 4, 5, 6, 7) ~ "Inadequada"),
           var63 = case_when(V0209 == 1 ~ "Adequada",
                             V0209 %in% c(2, 3) ~ "Inadequada"),
           var64 = case_when(V0210 %in% c(1, 2) ~ "Adequada",
                             V0210 %in% c(3, 4, 5, 6, 7) ~ "Inadequada"),
           var65 = case_when(V0211 %in% c(1, 2) ~ "Adequada",
                             V0211 == 3 ~ "Inadequada"),
           deficit_auxiliar = ifelse(var21 == "Inadequada" | var23 == "Inadequada" | var32 == "Inadequada" | var33 == "Inadequada" | var34 == "Inadequada", 1, 2),
           inadequacao_auxiliar = ifelse(var11 == "Inadequada" | var22 == "Inadequada" | var24 == "Inadequada" | var31 == "Inadequada" | var61 == "Inadequada" | var62 == "Inadequada" | var63 == "Inadequada" | var64  == "Inadequada" | var65 == "Inadequada", 1, 2),
           deficit_ou_inadequacao = case_when(deficit_auxiliar == 1 ~ "Deficit",
                                              inadequacao_auxiliar == 1 ~ "Inadequacao")) %>%
    select(codigo_ibge, V0010, V0011, fpc, 
           var11, 
           var21, var22, var23, var24, 
           var31, var32, var33, var34, 
           var61, var62, var63, var64, var65, 
           deficit_ou_inadequacao)
  
  censo2010_1$V0011 <- as.character(censo2010_1$V0011)
  
  censo2010_2 <- censo2010_1 %>%
    filter(deficit_ou_inadequacao %in% c("Deficit", "Inadequacao")) %>%
    mutate(f01 = ifelse(V0011 %in% ap_ap$V0011, NA, 1),
           f01 = ifelse(is.na(f01) & var33 == "Inadequada", 2, f01),
           f01 = ifelse(is.na(f01) & var23 == "Inadequada" & var22 != "Inadequada", 3, f01),
           f01 = ifelse(is.na(f01) & var24 == "Inadequada" & var22 != "Inadequada", 4, f01),
           f01 = ifelse(is.na(f01) & var65 == "Inadequada", 5, f01),
           f01 = ifelse(is.na(f01) & var62 == "Inadequada", 6, f01),
           f01 = ifelse(is.na(f01) & var63 == "Inadequada" & var22 != "Inadequada", 7, f01),
           f01 = ifelse(is.na(f01) & var64 == "Inadequada", 8, f01),
           f01 = ifelse(is.na(f01) & var61 == "Inadequada", 9, f01),
           f01 = ifelse(is.na(f01) & var21 == "Inadequada", 11, f01),
           f01 = ifelse(is.na(f01) & var22 == "Inadequada", 10, f01),
           f01 = ifelse(is.na(f01) & var32 == "Inadequada", 12, f01),
           f01 = ifelse(is.na(f01) & var34 == "Inadequada", 13, f01),
           f01 = ifelse(is.na(f01) & var31 == "Inadequada", 14, f01),
           f01 = ifelse(is.na(f01) & var11 == "Inadequada", 15, f01))
  
     
  dentro_ou_fora <- tibble(c("fora", "fora", "dentro", "dentro", "dentro", "dentro", "dentro", "dentro", "dentro", "fora", "fora", "proporcional", "proporcional", "proporcional", "proporcional"),
                           c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
  names(dentro_ou_fora) <- c("dentro_ou_fora", "f01")
  
  censo2010_2[is.na(censo2010_2)] = "Sem dados"
  
  censo2010_3 <- censo2010_2 %>%
    filter(f01 %in% c(8, 9, 10, 11, 12, 13)) %>%
    mutate(f02 = ifelse(var32 == "Inadequada" | var34 == "Inadequada", "dentro", "fora"))
  
  # Aqui e definido um parametro de pesquisa adicional, para ponderacao da pesquisa amostral (correcao de populacao finita)
  
  # Cria um objeto de pesquisa amostral
#  censo2010_3_ponderado <- svydesign(data = censo2010_3,
#                                     ids = ~1,
#                                     strata = ~V0011,
#                                     fpc = ~fpc,
#                                     weights = ~V0010)
  
  censo2010_2_ponderado <- svydesign(data = censo2010_2,
                                     ids = ~1,
                                     strata = ~V0011,
                                     fpc = ~fpc,
                                     weights = ~V0010)
  
  censo2010_1_ponderado <- svydesign(data = censo2010_1,
                                     ids = ~1,
                                     strata = ~V0011,
                                     fpc = ~fpc,
                                     weights = ~V0010)
  
  # dentro e fora ultimo passo
#  dfdf <- as.data.frame(svytable(~ deficit_ou_inadequacao + f01 + f02, censo2010_3_ponderado)) %>%
#    filter(deficit_ou_inadequacao == "Deficit")
  
  # calcula as proporcoes
  var11 <- as.data.frame(svytable(~ var11, censo2010_2_ponderado))
  var31 <- as.data.frame(svytable(~ var31, censo2010_2_ponderado))
  var32 <- as.data.frame(svytable(~ var32, censo2010_2_ponderado))
  var34 <- as.data.frame(svytable(~ var34, censo2010_2_ponderado))
  total_de_domicilios <- as.data.frame(svytable(~ codigo_ibge, censo2010_1_ponderado))
  
  prop11 <- var11[2,2]/(var11[2,2] + var31[2,2] + var32[2,2] + var34[2,2])
  prop31 <- var31[2,2]/(var11[2,2] + var31[2,2] + var32[2,2] + var34[2,2])
  prop32 <- var32[2,2]/(var11[2,2] + var31[2,2] + var32[2,2] + var34[2,2])
  prop34 <- var34[2,2]/(var11[2,2] + var31[2,2] + var32[2,2] + var34[2,2])
  
  prop1 <- ap_total$Total/proj_seade$DOM
  prop2 <- proj_seade$DOM/total_de_domicilios$Freq
  
  # Faz a contagem de domicilios a partir do objeto de pesquisa amostral
  f01_2 <- as.data.frame(svytable(~ deficit_ou_inadequacao + f01, censo2010_2_ponderado)) %>%
    spread(deficit_ou_inadequacao, Freq) %>%
    mutate(f01 = as.numeric(as.character(f01))) %>%
    complete(f01 = full_seq(1:15, 1)) %>%
    mutate(Deficit = Deficit * prop2,
           Inadequacao = Inadequacao * prop2)
  f01_2[is.na(f01_2)] = 0
  
  f01 <- as.data.frame(svytable(~ f01, censo2010_2_ponderado))
  f01 <- f01 %>% 
    #  spread(deficit_ou_inadequacao, Freq) %>%
    mutate(f01 = as.numeric(as.character(f01))) %>%
    complete(f01 = full_seq(1:15, 1)) %>%
    mutate(Freq = Freq * prop2) %>%
    inner_join(f01_2)
  f01 <- bind_cols(f01, dentro_ou_fora = dentro_ou_fora$dentro_ou_fora)
  f01[is.na(f01)] = 0
  
  f01 <- f01 %>%
    mutate(fora = case_when(dentro_ou_fora == "fora" ~ Freq,
                            dentro_ou_fora == "proporcional" ~ (1-prop1) * Freq),
           dentro = case_when(dentro_ou_fora == "dentro" ~ Freq,
                              dentro_ou_fora == "proporcional" ~ prop1 * Freq))
  f01[is.na(f01)] = 0
  
  f01[1,]$fora <- ifelse((sum(f01[1:1,]$dentro)) <= ap_total$Total, f01[1,]$fora, f01[1,]$fora + (f01[1,]$dentro) - (ap_total$Total - sum(f01[1:1,]$dentro)))
  f01[1,]$dentro <- ifelse((sum(f01[1:1,]$dentro)) <= ap_total$Total, f01[1,]$dentro, (ap_total$Total - sum(f01[1:1,]$dentro)))
  
  f01[2,]$fora <- ifelse((sum(f01[1:2,]$dentro)) <= ap_total$Total, f01[2,]$fora, f01[2,]$fora + (f01[2,]$dentro) - (ap_total$Total - sum(f01[1:1,]$dentro)))
  f01[2,]$dentro <- ifelse((sum(f01[1:2,]$dentro)) <= ap_total$Total, f01[2,]$dentro, (ap_total$Total - sum(f01[1:1,]$dentro)))
  
  f01[3,]$fora <- ifelse((sum(f01[1:3,]$dentro)) <= ap_total$Total, f01[3,]$fora, f01[3,]$fora + (f01[3,]$dentro) - (ap_total$Total - sum(f01[1:2,]$dentro)))
  f01[3,]$dentro <- ifelse((sum(f01[1:3,]$dentro)) <= ap_total$Total, f01[3,]$dentro, (ap_total$Total - sum(f01[1:2,]$dentro)))
  
  f01[4,]$fora <- ifelse((sum(f01[1:4,]$dentro)) <= ap_total$Total, f01[4,]$fora, f01[4,]$fora + (f01[4,]$dentro) - (ap_total$Total - sum(f01[1:3,]$dentro)))
  f01[4,]$dentro <- ifelse((sum(f01[1:4,]$dentro)) <= ap_total$Total, f01[4,]$dentro, (ap_total$Total - sum(f01[1:3,]$dentro)))
  
  f01[5,]$fora <- ifelse((sum(f01[1:5,]$dentro)) <= ap_total$Total, f01[5,]$fora, f01[5,]$fora + (f01[5,]$dentro) - (ap_total$Total - sum(f01[1:4,]$dentro)))
  f01[5,]$dentro <- ifelse((sum(f01[1:5,]$dentro)) <= ap_total$Total, f01[5,]$dentro, (ap_total$Total - sum(f01[1:4,]$dentro)))
  
  f01[6,]$fora <- ifelse((sum(f01[1:6,]$dentro)) <= ap_total$Total, f01[6,]$fora, f01[6,]$fora + (f01[6,]$dentro) - (ap_total$Total - sum(f01[1:5,]$dentro)))
  f01[6,]$dentro <- ifelse((sum(f01[1:6,]$dentro)) <= ap_total$Total, f01[6,]$dentro, (ap_total$Total - sum(f01[1:5,]$dentro)))
  
  f01[7,]$fora <- ifelse((sum(f01[1:7,]$dentro)) <= ap_total$Total, f01[7,]$fora, f01[7,]$fora + (f01[7,]$dentro) - (ap_total$Total - sum(f01[1:6,]$dentro)))
  f01[7,]$dentro <- ifelse((sum(f01[1:7,]$dentro)) <= ap_total$Total, f01[7,]$dentro, (ap_total$Total - sum(f01[1:6,]$dentro)))
  
  f01[8,]$fora <- ifelse((sum(f01[1:8,]$dentro)) <= ap_total$Total, f01[8,]$fora, f01[8,]$fora + (f01[8,]$dentro) - (ap_total$Total - sum(f01[1:7,]$dentro)))
  f01[8,]$dentro <- ifelse((sum(f01[1:8,]$dentro)) <= ap_total$Total, f01[8,]$dentro, (ap_total$Total - sum(f01[1:7,]$dentro)))
  
  f01[9,]$fora <- ifelse((sum(f01[1:9,]$dentro)) <= ap_total$Total, f01[9,]$fora, f01[9,]$fora + (f01[9,]$dentro) - (ap_total$Total - sum(f01[1:8,]$dentro)))
  f01[9,]$dentro <- ifelse((sum(f01[1:9,]$dentro)) <= ap_total$Total, f01[9,]$dentro, (ap_total$Total - sum(f01[1:8,]$dentro)))
  
  f01[10,]$fora <- ifelse((sum(f01[1:10,]$dentro)) <= ap_total$Total, f01[10,]$fora, f01[10,]$fora + (f01[10,]$dentro) - (ap_total$Total - sum(f01[1:9,]$dentro)))
  f01[10,]$dentro <- ifelse((sum(f01[1:10,]$dentro)) <= ap_total$Total, f01[10,]$dentro, (ap_total$Total - sum(f01[1:9,]$dentro)))
  
  f01[11,]$fora <- ifelse((sum(f01[1:11,]$dentro)) <= ap_total$Total, f01[11,]$fora, f01[11,]$fora + (f01[11,]$dentro) - (ap_total$Total - sum(f01[1:10,]$dentro)))
  f01[11,]$dentro <- ifelse((sum(f01[1:11,]$dentro)) <= ap_total$Total, f01[11,]$dentro, (ap_total$Total - sum(f01[1:10,]$dentro)))
  
  
  
  f01[12,]$dentro <- ifelse(sum(f01$dentro) >= ap_total$Total, (ap_total$Total - sum(f01[1:11,]$dentro)) * prop32, f01[12,]$dentro)
  f01[12,]$fora <- ifelse(sum(f01$dentro) >= ap_total$Total, f01[12,]$Freq - f01[12,]$dentro, f01[12,]$fora)
  
  f01[13,]$dentro <- ifelse(sum(f01$dentro) >= ap_total$Total, (ap_total$Total - sum(f01[1:11,]$dentro)) * prop34, f01[13,]$dentro)
  f01[13,]$fora <- ifelse(sum(f01$dentro) >= ap_total$Total, f01[13,]$Freq - f01[13,]$dentro, f01[13,]$fora)
  
  f01[14,]$dentro <- ifelse(sum(f01$dentro) >= ap_total$Total, (ap_total$Total - sum(f01[1:11,]$dentro)) * prop31, f01[14,]$dentro)
  f01[14,]$fora <- ifelse(sum(f01$dentro) >= ap_total$Total, f01[14,]$Freq - f01[14,]$dentro, f01[14,]$fora)
  
  f01[15,]$dentro <- ifelse(sum(f01$dentro) >= ap_total$Total, (ap_total$Total - sum(f01[1:11,]$dentro)) * prop11, f01[15,]$dentro)
  f01[15,]$fora <- ifelse(sum(f01$dentro) >= ap_total$Total, f01[15,]$Freq - f01[15,]$dentro, f01[15,]$fora)
  
  
  
  #f01[12,]$fora <- ifelse((sum(f01[1:12,]$dentro)) <= ap_total$Total, f01[12,]$fora, f01[12,]$fora + (f01[12,]$dentro) - (ap_total$Total - sum(f01[1:11,]$dentro)))
  #f01[12,]$dentro <- ifelse((sum(f01[1:12,]$dentro)) <= ap_total$Total, f01[12,]$dentro, (ap_total$Total - sum(f01[1:11,]$dentro)))
  
  #f01[13,]$fora <- ifelse((sum(f01[1:13,]$dentro)) <= ap_total$Total, f01[13,]$fora, f01[13,]$fora + (f01[13,]$dentro) - (ap_total$Total - sum(f01[1:12,]$dentro)))
  #f01[13,]$dentro <- ifelse((sum(f01[1:13,]$dentro)) <= ap_total$Total, f01[13,]$dentro, (ap_total$Total - sum(f01[1:12,]$dentro)))
  
  #f01[14,]$fora <- ifelse((sum(f01[1:14,]$dentro)) <= ap_total$Total, f01[14,]$fora, f01[14,]$fora + (f01[14,]$dentro) - (ap_total$Total - sum(f01[1:13,]$dentro)))
  #f01[14,]$dentro <- ifelse((sum(f01[1:14,]$dentro)) <= ap_total$Total, f01[14,]$dentro, (ap_total$Total - sum(f01[1:13,]$dentro)))
  
  #f01[15,]$fora <- ifelse((sum(f01[1:15,]$dentro)) <= ap_total$Total, f01[15,]$fora, f01[15,]$fora + (f01[15,]$dentro) - (ap_total$Total - sum(f01[1:14,]$dentro)))
  #f01[15,]$dentro <- ifelse((sum(f01[1:15,]$dentro)) <= ap_total$Total, f01[15,]$dentro, (ap_total$Total - sum(f01[1:14,]$dentro)))
  
  #f01[1,]$deficit_dentro <- case_when(f01[1,]$Deficit = 0 ~ 0,
  #                                    f01[1,]$Deficit > 0 & f01[1,]$dentro = 0 ~ 0,
  #                                    f01[1,]$Deficit > 0 & f01[1,]$dentro > 0 & f01[1,]$Inadequacao = 0 ~ f01[1,]$dentro,
  #                                    f01[1,]$Deficit > 0 & f01[1,]$dentro > 0 & f01[1,]$Inadequacao > 0 &)
  #f01[1,]$inadequacao_dentro <- 
  
  f01_final <- f01 %>%
    rename(Variavel = f01,
           Alocacao = dentro_ou_fora,
           Fora = fora,
           Dentro = dentro) %>%
    mutate(deficit_fora = ifelse(Fora > 0, Deficit * (Fora/(Fora + Dentro)), 0),
           inadequacao_fora = ifelse(Fora > 0, Inadequacao * (Fora/(Fora + Dentro)), 0),
           deficit_dentro = ifelse(Dentro > 0, Deficit * (Dentro/(Fora + Dentro)), 0),
           inadequacao_dentro = ifelse(Dentro > 0, Inadequacao * (Dentro/(Fora + Dentro)), 0)) %>%
    mutate(`Deficit + Inadequacao` = Deficit + Inadequacao) %>%
    select(Alocacao, Variavel, deficit_fora, inadequacao_fora, Fora, deficit_dentro, inadequacao_dentro, Dentro, Deficit, Inadequacao, `Deficit + Inadequacao`)
  
  
  f01_final2 <- f01_final %>%
    add_row(Alocacao = "territorial",
            Variavel = 16,
            deficit_dentro = ifelse(ap_total$Deficit > sum(f01_final$deficit_dentro), ap_total$Deficit - sum(f01_final$deficit_dentro), 0),
            inadequacao_dentro = 0,
            #inadequacao_dentro = ifelse(ap_total$Deficit > sum(f01_final$deficit_dentro), ap_total$Inadequacao - sum(f01_final$inadequacao_dentro), ifelse(ap_total$Deficit + ap_total$Inadequacao > sum(f01_final$deficit_dentro) + sum(f01_final$inadequacao_dentro), ap_total$Deficit + ap_total$Inadequacao - sum(f01_final$deficit_dentro) - sum(f01_final$inadequacao_dentro), 0)),
            Dentro = deficit_dentro + inadequacao_dentro,
            Deficit = deficit_dentro,
            Inadequacao = inadequacao_dentro,
            `Deficit + Inadequacao` = Deficit + Inadequacao) %>%
    add_row(Alocacao = "territorial",
            Variavel = 17,
            deficit_dentro = 0,
            #deficit_dentro = ifelse(ap_total$Deficit > sum(f01_final$deficit_dentro), ap_total$Deficit - sum(f01_final$deficit_dentro), 0),
            inadequacao_dentro = ifelse(ap_total$Deficit > sum(f01_final$deficit_dentro), ap_total$Inadequacao - sum(f01_final$inadequacao_dentro), ifelse(ap_total$Deficit + ap_total$Inadequacao > sum(f01_final$deficit_dentro) + sum(f01_final$inadequacao_dentro), ap_total$Deficit + ap_total$Inadequacao - sum(f01_final$deficit_dentro) - sum(f01_final$inadequacao_dentro), 0)),
            Dentro = deficit_dentro + inadequacao_dentro,
            Deficit = deficit_dentro,
            Inadequacao = inadequacao_dentro,
            `Deficit + Inadequacao` = Deficit + Inadequacao)
  
  f01_final2[is.na(f01_final2)] = 0
  
  p02 <- f01_final %>%
    as_tabyl() %>%
    adorn_rounding(digits = 0) %>%
    adorn_totals(where = "row")
  
  p03 <- f01_final2 %>%
    as_tabyl() %>%
    adorn_rounding(digits = 0) %>%
    adorn_totals(where = "row")
  
  p01 <- f01 %>%
    rename(Variavel = f01,
           Total = Freq,
           Fora = fora,
           Dentro = dentro,
           Alocacao = dentro_ou_fora) %>%
    select(Variavel, Alocacao, Deficit, Inadequacao, Total, Fora, Dentro) %>%
    as_tabyl() %>%
    adorn_rounding(digits = 0) %>%
    adorn_totals(where = "row")
  
  return(p03)
  
}
