########################## DEFININDO O DIRETÓRIO ###############################

setwd("C:/Users/UM/Desktop/R/Data-TCC")
library(tidyverse)
library(readxl)

############# CALCULANDO A ÁREA URBANIZADA POR CONCENTRAÇÃO URBANA #############

######################### CONVERTENDO PARA DATA FRAME ##########################

excel_sheets("C:/Users/UM/Desktop/R/Data-TCC/area_urbanizada_2015_2019.xlsx")

sep_sheets <- excel_sheets("C:/Users/UM/Desktop/R/Data-TCC/area_urbanizada_2015_2019.xlsx") %>% 
  map(~read_xlsx("C:/Users/UM/Desktop/R/Data-TCC/area_urbanizada_2015_2019.xlsx"))

area_urb_ls <- sep_sheets[1]
str(area_urb_ls)

area_urb <- as.data.frame(do.call(cbind, area_urb_ls))

#Limpando os casos de informações totais e legendas de tabela

area_urb <- area_urb[complete.cases(area_urb), ]
view(area_urb)

#Renomeando variáveis necessárias

area_urb <- area_urb %>% 
 rename(cod = "CÓDIGO CONCENTRAÇÃO URBANA",
        nome = "NOME CONCENTRAÇÃO URBANA",
        total_2015 = "TOTAL 2015 (RECALCULADO)",
        total_2019 = "TOTAL COMPARÁVEL 2019")
head(area_urb, n = 5)

#Eliminando variáveis desnecessárias e incluindo variável - variação da area urbana

area_urb_comp <- area_urb %>% 
  select(cod, nome, total_2015, total_2019) %>%
  mutate(var_area = ((total_2019 - total_2015)/total_2015)) %>% view

#Padronizando o nome dos municípios e eliminando areas conurbadas (mun.1 - mun.2 ou internacional)
area_urb_cl <- area_urb_comp %>% 
  filter(!grepl("-", nome)) %>% view()

area_urb_cl <-area_urb_cl %>% 
  filter(!grepl("[Ii]nternacional", nome)) %>% view()

#Retirando a unidade federativa (/UF) do nome
area_urb_cl <- lapply(area_urb_cl, function(x) str_remove(x,"/.*")) %>% view

################## INCORPORANDO ÁREAS TOTAIS DOS MUNICÍPIOS ####################

area_mun <- read_xlsx("C:/Users/UM/Desktop/R/Data-TCC/Area_Mun_2022.xlsx")

area_mun <- area_mun %>% 
  rename(cod_uf = "CD_UF",
         nome_uf = "NM_UF",
         sigla_uf = "NM_UF_SIGLA",
         cod = "CD_MUN",
         nome = "NM_MUN",
         area_mun = "AR_MUN_2022")
head(area_mun, n = 5)

area_urb_cl$cod <-as.integer(area_urb_cl$cod)
area_mun$cod <- as.integer(area_mun$cod)
base_mun <- left_join(area_urb_cl, area_mun, by = "cod")

sum(is.na(base_mun))

str(base_mun)

base_mun$total_2015 <- as.numeric(base_mun$total_2015)
base_mun$total_2019 <- as.numeric(base_mun$total_2019)

str(base_mun)

base_mun <- base_mun %>% 
  select(-nome.y) %>% 
  rename(nome = "nome.x") %>% 
  mutate(prop_urb_2015 = total_2015/area_mun) %>% 
  mutate(prop_urb_2019 = total_2019/area_mun)

############## CRIANDO BASE COM NR. DE HOMICÍDIOS POR MUNICÍPIOS ###############

##Selecionando os dados para o período desejado

tx_hom_df <- as.data.frame(tx_hom_br)
str(tx_hom_df)
as.character(colnames(tx_hom_df))

tx_hom_sel <- select(tx_hom_df, cod, nome, `2015`, `2019`)

##Conferindo igualdade entre nomes e códigos de municípios antes do Join
base_mun$cod %in% tx_hom_sel$cod

## Left Join
base_urb_hom <-
  left_join(base_mun, tx_hom_sel, by = "cod")

base_urb_hom <- within(base_urb_hom, rm(nome.y))

##Novos nomes variáveis
base_urb_hom <- base_urb_hom %>% 
  rename(nome = "nome.x",
         area_2015 = "total_2015",
         area_2019 = "total_2019",
         hom_2015 = "2015",
         hom_2019 = "2019")
head(base_urb_hom)
save(base_urb_hom,file = "base_urb_hom.RData")
  
