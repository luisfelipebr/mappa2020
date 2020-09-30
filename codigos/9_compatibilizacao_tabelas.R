
# abre as bibliotecas
if (!require('data.table')) install.packages('data.table'); library('data.table') # adiciona uma funcao para abrir grandes volumes de dados no R
if (!require('dplyr')) install.packages('dplyr'); library('dplyr') # adiciona o comando pipe (%>%), que deixa o codigo mais legivel
if (!require('tidyr')) install.packages('tidyr'); library('tidyr') # adiciona funcoes para realizar a limpeza dos dados
if (!require('stringr')) install.packages('stringr'); library('stringr') # adiciona funcoes para lidar com textos
if (!require('survey')) install.packages('survey'); library('survey') # adiciona funcoes para trabalhar com os dados de pesquisas amostrais
if (!require('janitor')) install.packages('janitor'); library('janitor')

# MICRODADOS_DOMICILIAR = censo2010
censo2010 <- read.csv2("dados/censo2010_RMSP.csv")

# DADOS_TERRITORIAL = abordagem_territorial
abordagem_territorial <- read.csv2("dados/abordagem_territorial.csv")

aoi <- abordagem_territorial$codigo_ibge

# AREAS_DE_PONDERACAO_COM_ASSENTAMENTOS_PRECARIOS = ap_ap
ap_ap <- read.csv2("dados/ap_ap.csv")

# PROJECAO_SEADE = proj_seade
projecao_seade <- read.csv("./dados/seade/tb_dados.txt", sep=";")

data <- censo2010
ibge_code <- aoi[6]
ap_territorial <- abordagem_territorial

municipios <- read_csv("dados/municipios.csv") %>%
  filter(codigo_ibge %in% aoi)

total_de_domicilios_abc <- projecao_seade %>%
  filter(mun_id %in% aoi) %>%
  filter(ano == 2018) %>%
  select(mun_id, DOM)

###

produto1 <- lapply(aoi, compatibilizacao1, data = censo2010, ap_territorial = abordagem_territorial, ap_ap = ap_ap, projecao_seade = projecao_seade)
names(produto1) <- abordagem_territorial$Município

lapply(1:length(produto1), 
       function(i) write.csv2(produto1[[i]],
                              file = paste0("resultados/t1_", names(produto1[i]), ".csv"),
                              row.names = FALSE))

produto2 <- lapply(aoi, compatibilizacao3, data = censo2010, ap_territorial = abordagem_territorial, ap_ap = ap_ap, projecao_seade = projecao_seade)
names(produto2) <- abordagem_territorial$Município

lapply(1:length(produto2), 
       function(i) write.csv2(produto2[[i]],
                              file = paste0("resultados/t2_", names(produto2[i]), ".csv"),
                              row.names = FALSE))

grau_de_incerteza <- c("1) Área de Ponderação sem Assentamentos Precários",
                       "2) Densidade Excessiva + Apartamento",
                       "3) Material de Parede Externa Inadequado (Exceto cortiço)",
                       "4) Sem Banheiro (Exceto cortiço)",
                       "5) Sem Energia Elétrica",
                       "6) Sem Rede de Abastecimento de Água",
                       "7) Sem Canalização de Água (Exceto cortiço)",
                       "8) Sem Coleta de Lixo",
                       "9) Sem Rede Geral de Esgoto ou Fossa Séptica",
                       "10) Cortiço",
                       "11) Domicílio Improvisado",
                       "12) Densidade Excessiva + Coabitação",
                       "13) Densidade Excessiva + Não Próprio",
                       "14) Densidade Excessiva (Exceto relacionada a apartamento, coabitação e domicílios não próprios)",
                       "15) Ônus Excessivo com Aluguel",
                       "Com previsão de remoção",
                       "Sem previsão de remoção",
                       "-")

produto3 <- lapply(aoi, compatibilizacao3, data = censo2010, ap_territorial = abordagem_territorial, ap_ap = ap_ap, projecao_seade = projecao_seade) %>%
  lapply(mutate, "Variavel/Grau de Incerteza" = grau_de_incerteza) %>%
  lapply(select, Alocacao, "Variavel/Grau de Incerteza", Dentro, Fora, "Deficit + Inadequacao") %>%
  lapply(rename, Alocação = Alocacao, "Variável/Grau de Incerteza" = "Variavel/Grau de Incerteza", Total = "Deficit + Inadequacao")
names(produto3) <- abordagem_territorial$Município

lapply(1:length(produto3), 
       function(i) write.csv2(produto3[[i]],
                              file = paste0("resultados/t3_", names(produto3[i]), ".csv"),
                              row.names = FALSE))

produto4 <- lapply(aoi, compatibilizacao3, data = censo2010, ap_territorial = abordagem_territorial, ap_ap = ap_ap, projecao_seade = projecao_seade)
names(produto4) <- abordagem_territorial$Município
produto4 <- lapply(produto4, filter, Alocacao == "Total") %>%
  bind_rows() %>%
  mutate(Alocacao = abordagem_territorial$Município) %>%
  select(-Variavel) %>%
  rename("Nome do Município" = Alocacao,
         "Déficit Fora" = deficit_fora,
         "Inadequação Fora" = inadequacao_fora,
         "Déficit + Inadequação Fora" = Fora,
         "Déficit Dentro" = deficit_dentro,
         "Inadequação Dentro" = inadequacao_dentro,
         "Déficit + Inadequação Dentro" = Dentro,
         "Déficit Total" = Deficit,
         "Inadequação Total" = Inadequacao,
         "Déficit + Inadequação Total" = "Deficit + Inadequacao") %>%
    mutate("Total de domicílios" = total_de_domicilios_abc$DOM)

write.csv2(produto4, "resultados/t4_rmbs.csv", row.names = FALSE)

produto0 <- lapply(produto1, filter, Variavel == "Total") %>%
  bind_rows() %>%
  mutate(Alocacao = abordagem_territorial$Município) %>%
  select(-Variavel, -Fora, -Dentro) %>%
  rename(Nome = Alocacao) %>%
  mutate("Total de domicílios" = total_de_domicilios_abc$DOM)

write.csv2(produto0, "dados/abordagem_domiciliar.csv")
