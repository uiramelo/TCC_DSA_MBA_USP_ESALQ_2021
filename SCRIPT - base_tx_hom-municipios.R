####################### DEFININDO DIRETÓRIO DE TRABALHO ########################

setwd("C:/Users/UM/Desktop/R/Data-TCC")

##Instalando Pacotes
install.packages("tidyverse")
install.packages("readxl")

##Carregando pacotes
library(tidyverse, readxl)

##################### CRIANDO BASE HOMICÍDIOS POR MUNICÍPIO ####################

tx_hom_BR <- read.csv("C:/Users/UM/Desktop/R/Data-TCC/tx_hom_2000-2021.csv",
                      header = TRUE, sep = ";", dec = ".")
tx_hom_BR <- as_data_frame(tx_hom_BR) %>% view
str(tx_hom_BR)

##Convertendo para o formato wide

tx_hom_br <- pivot_wider(tx_hom_BR, id_cols = c("cod", "nome"),
                         names_from = "período",
                         values_from = "valor")
view(tx_hom_br)
str(tx_hom_br)
