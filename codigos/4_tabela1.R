
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
def_hab_total <- lapply(rmsp, microdados2010 = censo2010, deficit_e_inadequacao_habitacional_total_por_municipio)

# cria as tabelas de saida
def_hab_total2 <- def_hab_total %>%
  bind_rows() %>%
  mutate(codigo_ibge = as.numeric(as.character(`Codigo IBGE`))) %>%
  inner_join(municipios, by = "codigo_ibge") %>%
  rename("Nome do Municipio" = "nome") %>%
  adorn_rounding(digits = 0, rounding = "half up") %>%
  select("Nome do Municipio", "Deficit", "Inadequacao", "Deficit + Inadequacao", "Total de domicilios") %>%
  adorn_totals(where = "row") %>%
  mutate(`Deficit + Inadequacao` = Deficit + Inadequacao)

def_hab_total_number <- def_hab_total %>%
  bind_rows() %>%
  mutate(codigo_ibge = as.numeric(as.character(`Codigo IBGE`))) %>%
  inner_join(municipios, by = "codigo_ibge") %>%
  rename("Nome do Municipio" = "nome") %>%
  adorn_rounding(digits = 0, rounding = "half up") %>%
  mutate(Adequado = `Total de domicilios` - Deficit - Inadequacao) %>%
  select("Nome do Municipio", "Adequado", "Deficit", "Inadequacao") %>%
  adorn_totals(where = c("row", "col"))

def_hab_total_pct <- def_hab_total %>%
  bind_rows() %>%
  mutate(codigo_ibge = as.numeric(as.character(`Codigo IBGE`))) %>%
  inner_join(municipios, by = "codigo_ibge") %>%
  rename("Nome do Municipio" = "nome") %>%
  adorn_rounding(digits = 0, rounding = "half up") %>%
  mutate(Adequado = `Total de domicilios` - Deficit - Inadequacao) %>%
  select("Nome do Municipio", "Adequado", "Deficit", "Inadequacao") %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting(digits = 2, affix_sign = FALSE)

def_hab_total_final <- def_hab_total_number[,c("Nome do Municipio", "Total")] %>%
  mutate(Déficit = paste0(def_hab_total_number$Deficit, " (", def_hab_total_pct$Deficit, "%)"),
         Inadequação = paste0(def_hab_total_number$Inadequacao, " (", def_hab_total_pct$Inadequacao, "%)"),
         "Déficit + Inadequação" = paste0(def_hab_total_number$Deficit + def_hab_total_number$Inadequacao, " (", as.numeric(def_hab_total_pct$Deficit) + as.numeric(def_hab_total_pct$Inadequacao), "%)"),
         "Total de domicílios" = paste0(def_hab_total_number$Total, " (", def_hab_total_pct$Total, "%)")) %>%
  select(-Total)

# salva os resultados
write.csv2(def_hab_total_final, "resultados/tabela1.csv", row.names = FALSE)
