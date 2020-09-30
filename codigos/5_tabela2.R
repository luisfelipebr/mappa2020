
# extra function ----
round_preserve_sum <- function(x, digits = 0) {
  up <- 10^digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}

round_everything <- function(df){
  for (x in 1:nrow(df)){
    df[x,3:5] <- round_preserve_sum(df[x,3:5])
  }
  return(df)
}

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
def_hab_variaveis <- lapply(rmsp, microdados2010 = censo2010, deficit_e_inadequacao_habitacional_variaveis_por_municipio)

# cria tabela de saida
def_hab_variaveis2 <- def_hab_variaveis %>%
  lapply(round_everything) %>%
  bind_rows() %>%
  mutate(codigo_ibge = as.numeric(as.character(`Codigo IBGE`))) %>%
  inner_join(municipios, by = "codigo_ibge") %>%
  select("nome", "Variavel", "Adequada", "Inadequada", "Sem Dados ou Não Aplicável") %>%
  adorn_totals(where = "col") %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 2, affix_sign = TRUE) %>%
  adorn_ns(position = "front") %>%
  select("nome", "Variavel", "Inadequada", "Total") %>%
  spread("Variavel", "Inadequada") %>%
  mutate("Nome do Município" = nome,
         "Total de domicílios" = Total) %>%
  select(-c(nome, Total)) %>%
  select("Nome do Município", everything())

# exporta os resultados
write.csv2(def_hab_variaveis2, "resultados/tabela2.csv", row.names = F)
