############################ DEFININDO DIRETÓRIO ###############################

setwd("C:/Users/UM/Desktop/R/Data-TCC")
library(tidyverse)
library(readxl)

##################### CRIANDO A BASE - PIB POR MUNICÍPIO #######################

##Convertendo para Data Frame

mun_pib <- read_xlsx("C:/Users/UM/Desktop/R/Data-TCC/Municipios_PIB_pc_2015_2019.xlsx")

str(mun_pib)

mun_pib <- as.data.frame(mun_pib)

##Limpando os casos de informações totais e legendas de tabela

mun_pib <- mun_pib[complete.cases(mun_pib), ]

head(mun_pib, n = 5)

##Incluindo variável - variação do PIB

mun_pib <- mun_pib %>% 
  mutate(var_pib = ((PIB_pc_2019 - PIB_pc_2015)/PIB_pc_2015)) %>% 
  mutate(var_pib_pcapita = ((PIB_pcapita_2019 - PIB_pcapita_2015)/PIB_pcapita_2015)) %>%
  view

##Checando a existência de NAs

sum(is.na(mun_pib))

##################### CRIANDO A BASE - PIB-ÁREAS URBANAS #######################

##Conferindo igualdade entre nomes e códigos de municípios antes do Join

base_mun$cod %in% mun_pib$cod

##Left Join

base_pib <-
  left_join(base_mun, mun_pib, by = "cod")
View(base_pib)
sum(is.na(base_pib))

##Removendo variáveis repetidas

base_pib <- within(base_pib, rm(nome.y, nome_uf.y, sigla_uf.y))

##Renomeando variáveis

base_pib <- base_pib %>% 
  rename(nome = "nome.x",
         nome_uf = "nome_uf.x",
         sigla_uf = "sigla_uf.x")
str(base_pib)
save(base_pib,file = "base_pib.RData")
  
