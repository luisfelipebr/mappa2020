
# open libraries
library(data.table)
library(tidyverse)
library(stringr)

# define variables
variaveis_domicilios <- c("V0001", "V0002", "V0300", "V0010", "V0011", 
                          "V2011", "V6529", "V4001", "V4002", "V0202", 
                          "V0206", "V6204", "V0201", "V0207", "V0208", 
                          "V0209", "V0210", "V0211", "V0205", "V1004",
                          "V6530")
variaveis_pessoas <- c("V0001", "V0002", "V0300", "V5020", "V1004")

# open data
censo2010_dom <- fread("./dados/ibge/censo/dados_csv2010/censo2010_RMSPdom.csv", select = variaveis_domicilios)
censo2010_pes <- fread("./dados/ibge/censo/dados_csv2010/censo2010_RMSPpes.csv", select = variaveis_pessoas)

# create ibge code
censo2010_dom$V0001 <- as.character(censo2010_dom$V0001)
censo2010_dom$V0002 <- as.character(censo2010_dom$V0002)
censo2010_dom$V0002 <- str_pad(censo2010_dom$V0002, 5, pad = "0")
censo2010_dom <- censo2010_dom %>% mutate(codigo_ibge = paste0(V0001, V0002))
censo2010_dom$codigo_ibge <- as.numeric(censo2010_dom$codigo_ibge)

censo2010_pes$V0001 <- as.character(censo2010_pes$V0001)
censo2010_pes$V0002 <- as.character(censo2010_pes$V0002)
censo2010_pes$V0002 <- str_pad(censo2010_pes$V0002, 5, pad = "0")
censo2010_pes <- censo2010_pes %>% mutate(codigo_ibge = paste0(V0001, V0002))
censo2010_pes$codigo_ibge <- as.numeric(censo2010_pes$codigo_ibge)

# join houses and people datasets
censo2010 <- censo2010_pes %>%
  group_by(V0300) %>%
  summarize(V5020 = max(V5020)) %>%
  right_join(censo2010_dom, by = "V0300")
#censo2010$V0011 <- as.double(censo2010$V0011)

# define research parameter (finite population correction)
tamanho_pop <- aggregate(V0010 ~ V0011, data = censo2010, FUN = "sum")
names(tamanho_pop) <- c("V0011", "fpc")
censo2010 <- inner_join(censo2010, tamanho_pop, by = "V0011")

# export data
write.csv2(censo2010, "dados/censo2010_RMSP.csv")
