
# abre as bibliotecas ----
library(tidyverse)
library(survey)
library(janitor)

# abre os dados
censo2010 <- read.csv2("dados/censo2010_RMSP.csv")

rmsp <- c(3503901, 3505708, 3506607, 3509007, 3509205, 3510609, 
          3513009, 3513801, 3515004, 3515103, 3515707, 3516309, 3516408, 
          3518305, 3518800, 3522208, 3522505, 3523107, 3525003, 3526209, 
          3528502, 3529401, 3530607, 3534401, 3539103, 3539806, 3543303, 
          3544103, 3545001, 3546801, 3547304, 3547809, 3548708, 3548807, 
          3549953, 3550308, 3552502, 3552809, 3556453)

municipios <- read_csv("dados/municipios.csv")

municipios_rmsp <- municipios %>%
  filter(codigo_ibge %in% rmsp)

# aplica funcao na rmsp
def_hab_renda <- lapply(rmsp, microdados2010 = censo2010, deficit_e_inadequacao_habitacional_total_por_municipio_por_renda)

# cria tabela de saída
def_hab_renda2 <- def_hab_renda %>%
  bind_rows() %>%
  select(-"Deficit + Inadequacao") %>%
  mutate(codigo_ibge = as.numeric(as.character(`Codigo IBGE`))) %>%
  inner_join(municipios, by = "codigo_ibge") %>%
  select(-c("Inadequacao", "Sem Dados", "Total de domicilios")) %>%
  spread("smtotal", "Deficit") %>%
  select("nome", "Até 1 SM", "De 1 à 3 SM", "De 3 à 5 SM", "Acima de 5 SM") %>%
  adorn_rounding(digits = 0, rounding = "half up") %>%
  adorn_totals(where = c("col", "row")) %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 2, affix_sign = TRUE) %>%
  adorn_ns(position = "front") %>%
  select("nome", "Até 1 SM", "De 1 à 3 SM", "De 3 à 5 SM", "Acima de 5 SM", "Total") %>%
  rename("Nome do Município" = "nome",
         "Déficit Total" = "Total")

# exporta os resultados
write.csv2(def_hab_renda2, "resultados/tabela3.csv", row.names = F)
