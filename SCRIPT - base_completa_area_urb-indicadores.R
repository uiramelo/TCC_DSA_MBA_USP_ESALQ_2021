############ CRIANDO A BASE COMPLETA - AREAS URBANIZADAS INDICADORES ###########

######################## DEFININDO DIRETÓRIO E PACOTES #########################
setwd("C:/Users/UM/Desktop/R/Data-TCC")
library(tidyverse)
library(readxl)
install.packages("openxlsx")
library(openxlsx)

################ SELECIONANDO INDICADORES DE POLÍTICAS PÚBLICAS ################

##Convertendo para Data Frame
excel_sheets("C:/Users/UM/Desktop/R/Data-TCC/Base_MUNIC_2021.xlsx")

rh <- read.xlsx("Base_MUNIC_2021.xlsx", 3, colNames = TRUE)

leg_plan <- read.xlsx("Base_MUNIC_2021.xlsx", 4, colNames = TRUE)

educ <- read.xlsx("Base_MUNIC_2021.xlsx", 5, colNames = TRUE)

cult <- read.xlsx("Base_MUNIC_2021.xlsx", 6, colNames = TRUE)

esp <- read.xlsx("Base_MUNIC_2021.xlsx", 7, colNames = TRUE)

saude <- read.xlsx("Base_MUNIC_2021.xlsx", 8, colNames = TRUE)

##Separando as variáveis e compondo o dataframe único

rh <- rh %>% select(CodMun, Mun, Pop, Mreh0116)
leg_plan <- leg_plan %>% select(CodMun, Mun, Mleg01)
educ <- educ %>% select(CodMun, Mun, Medu114)
cult <- cult %>% select(CodMun, Mun, Mcul10)
esp <- esp %>% select(CodMun, Mun, Mesp07)
saude <- saude %>%  select(CodMun, Mun, Msau10)

## Unificando em base única

pol_pub <- left_join(rh, leg_plan, by = "CodMun")
pol_pub <- left_join(pol_pub, educ, by = "CodMun")
pol_pub <- left_join(pol_pub, cult, by = "CodMun")
pol_pub <- left_join(pol_pub, esp, by = "CodMun")
pol_pub <- left_join(pol_pub, saude, by = "CodMun")
View(pol_pub)
str(pol_pub)

as.character(colnames(pol_pub))

## Removendo informações duplicadas

pol_pub <- pol_pub %>% 
 select(-Mun.x.x,
        -Mun.x.x.x,
        -Mun.y,
        -Mun.y.y,
        -Mun.y.y.y)

View(pol_pub)
str(pol_pub)
as.data.frame(pol_pub)

## Renomeando variáveis selecionadas

pol_pub <- pol_pub %>% 
  rename(
    cod = "CodMun", 
    nome = "Mun.x", 
    nr_pop = "Pop", 
    rh_adm_dir = "Mreh0116",
    leg_plan = "Mleg01",
    educ = "Medu114",
    cult = "Mcul10",
    esp = "Mesp07",
    saude = "Msau10"
  )
str(pol_pub)

## Convertendo para numérico rh_adm_dir a fim de construir indicador

pol_pub$rh_adm_dir <- as.numeric(pol_pub$rh_adm_dir)


## Criando a variável nr servidores na administração direta/população total

pol_pub <- pol_pub %>% 
mutate(prop_rh_pop = rh_adm_dir/nr_pop)

##Removendo strings desnecessárias

pol_pub <- pol_pub %>%  
  mutate(cult = replace(cult, cult == "Em elaboração", "Sim")) %>% 
  mutate(cult = replace(cult, cult == "Sim, por instrumento legal", "Sim")) %>% 
  mutate(cult = replace(cult, cult == "Sim, mas não é regulamentado por instrumento legal", "Sim"))

View(pol_pub)

## Removendo "NAs"

pol_pub <- na.omit(pol_pub) 
sum(is.na(pol_pub))

## Transformando "Sim" e "Não" em  "0" e "1"

pol_pub_nr <- pol_pub %>% 
  mutate(leg_plan = ifelse(leg_plan == "Sim", 1,0)) %>% 
  mutate(educ = ifelse(educ == "Sim", 1,0)) %>%       
  mutate(cult = ifelse(cult == "Sim", 1,0)) %>%
  mutate(esp = ifelse(esp == "Sim", 1,0)) %>%
  mutate(saude = ifelse(saude == "Sim", 1,0)) %>% 
  mutate(sum_pol_pub = leg_plan + educ + cult + esp + saude)
View(pol_pub_nr)
str(pol_pub_nr)

sum(is.na(pol_pub_nr))

######## CRIANDO DATA FRAME-BASE COMPLETA COM INDICADORES SELECIONADOS #########

## Políticas públicas - homicídios - area urbanizada

base_pp_hom <- left_join(pol_pub_nr, base_urb_hom, by = "cod")
View(base_comp)

base_pp_hom <- na.omit(base_pp_hom) %>% 
  select(-nome.y)

sum(is.na(base_pp_hom))


## Políticas públicas - homicídios - PIB - população total - area urbanizada

base_comp <- left_join(base_pp_hom, base_pib, by = "cod")

base_comp <- left_join(base_comp, base_pop, by = "cod")

str(base_comp)

save(base_comp, file = "base_comp.RData")

################## BASE COMPLETA COM INDICADORES SELECIONADOS ##################

base_comp_f <- base_comp %>% 
  select(-c(nome.x.x,
            nome.y,
            nr_pop,
            var_area.x,
            cod_uf.y,
            nome_uf.y,
            sigla_uf.y,
            area_mun.y,
            prop_urb_2015.y,
            prop_urb_2019.y,
            total_2015,
            total_2019)) %>% 
  rename(nome = "nome.x",
         var_area = "var_area.y",
         cod_uf = "cod_uf.x",
         nome_uf = "nome_uf.x",
         sigla_uf = "sigla_uf.x",
         area_mun = "area_mun.x",
         prop_urb_2015 = "prop_urb_2015.x",
         prop_urb_2019 = "prop_urb_2019.x")

View(base_comp_f)
str(base_comp_f)

base_comp_f$var_area <- as.numeric(base_comp_f$var_area)

str(base_comp_f)
summary(base_comp_f)

save(base_comp_f, file = "base_comp_f.RData")

write.xlsx(base_comp_f, file = "base_com_f.xlsx")
write.csv(base_comp_f, file = "base_comp_f.csv")
