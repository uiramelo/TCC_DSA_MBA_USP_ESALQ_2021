################## ESTIMATIVAS POPULACIONAIS - 2015-2019 #######################

####################### DEFININDO DIRETÓRIO E PACOTES ##########################

## Definindo diretório
setwd("C:/Users/UM/Desktop/R/Data-TCC")

## Instalando pacotes
install.packages("tidyverse)")
library(tidyverse)
install.packages("openxlsx")
library(openxlsx)
install.packages("readxl")
library(readxl)

############# CRIANDO BASE - ESTIMATIVAS POPULACIONAIS - 2015-2019 #############

##Convertendo para Data Frame

excel_sheets("C:/Users/UM/Desktop/R/Data-TCC/estimativa_dou_2015.xlsx")
pop_2015 <- read.xlsx("estimativa_dou_2015.xlsx", 2, colNames = TRUE)
View(pop_2015)

excel_sheets("C:/Users/UM/Desktop/R/Data-TCC/estimativa_dou_2019.xlsx")
pop_2019 <- read.xlsx("estimativa_dou_2019.xlsx", 2, colNames = TRUE)
View(pop_2019)

str(pop_2015)
str(pop_2019)

pop_2015$COD..MUNIC <- as.numeric(pop_2015$COD..MUNIC)
pop_2015$POPULAÇÃO.ESTIMADA <- as.numeric(pop_2015$POPULAÇÃO.ESTIMADA)

pop_2019$COD..MUNIC <- as.numeric(pop_2019$COD..MUNIC)
pop_2019$POPULAÇÃO.ESTIMADA <- as.numeric(pop_2019$POPULAÇÃO.ESTIMADA)

sum(is.na(pop_2015))
is.na(pop_2015)
which(is.na(pop_2015))
apply(is.na(pop_2015), 2, which)

sum(is.na(pop_2019))
is.na(pop_2019)

## Criando Base Populacional

base_pop_est <- merge(pop_2015, pop_2019, by = c("COD..MUNIC", "NOME.DO.MUNICÍPIO")) 

base_pop_est <- base_pop_est %>% 
  select(-c(COD..UF.x, UF.y, COD..UF.y)) %>% 
  rename(cod = "COD..MUNIC",
         nome = "NOME.DO.MUNICÍPIO",
         sigla_uf = "UF.x",
         pop_est_2015 = "POPULAÇÃO.ESTIMADA.x",
         pop_est_2019 = "POPULAÇÃO.ESTIMADA.y")

base_pop_est <- base_pop_est %>% 
  mutate(var_pop = ((pop_est_2019 - pop_est_2015)/pop_est_2015))

str(base_pop_est)

save(base_pop_est, file = "base_pop_est.RData")

## Unificando os códigos dos municípios e selecionando amostra

base_pop <- merge(base_comp, base_pop_est, by = c("nome", "sigla_uf"))
View(base_pop)

base_pop <- base_pop %>% 
  select(cod.x, nome, pop_est_2015, pop_est_2019, var_pop) %>% 
  rename(cod = "cod.x")
head(base_pop)
str(base_pop)
