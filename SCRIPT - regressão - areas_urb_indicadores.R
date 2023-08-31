############# INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS #################

##Pacotes utilizados
pacotes <- c("plotly","tidyverse","ggrepel","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggplot2","ggraph","ggpubr", "nortest","rgl","car","olsrr","jtools","ggstance",
             "Rcpp")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


###################### CARREGAMENTO DA BASE DE DADOS ###########################               

##Diretorio

setwd("C:/Users/UM/Desktop/R/Data-TCC")

##Lista dos arquivos

list.files()

##Carregando a base de dados

load(file = "base_comp_f.RData")


########### ANÁLISE DA COMPOSIÇÃO ESTADUAL DA BASE DE MUNICÍPIOS ###############               

## Categorizando as variáves - character -> factor

base_comp_f$cod_uf <- as.numeric(base_comp_f$cod_uf)
base_comp_f$nome_uf <- as.factor(base_comp_f$nome_uf)
base_comp_f$sigla_uf <- as.factor(base_comp_f$sigla_uf)
View(base_comp_f)


mun_uf <- base_comp_f%>% 
  count(sigla_uf)
mun_uf <- as.data.frame(mun_uf)
mun_uf <- mun_uf %>% 
  rename(nr_mun = "n")

mun_uf <- na.omit(mun_uf)

## Determinando as regiões
mun_regiao <- mun_uf %>% 
  mutate(regiao = case_when(sigla_uf == "AC" ~ "Norte",
                            sigla_uf == "AM" ~ "Norte",
                            sigla_uf == "AP" ~ "Norte",
                            sigla_uf == "PA" ~ "Norte",
                            sigla_uf == "RO" ~ "Norte",
                            sigla_uf == "RR" ~ "Norte",
                            sigla_uf == "TO" ~ "Norte",
                            sigla_uf == "MA" ~ "Nordeste",
                            sigla_uf == "PI" ~ "Nordeste",
                            sigla_uf == "CE" ~ "Nordeste",
                            sigla_uf == "RN" ~ "Nordeste",
                            sigla_uf == "PE" ~ "Nordeste",
                            sigla_uf == "PB" ~ "Nordeste",
                            sigla_uf == "SE" ~ "Nordeste",
                            sigla_uf == "AL" ~ "Nordeste",
                            sigla_uf == "BA" ~ "Nordeste",
                            sigla_uf == "MT" ~ "Centro Oeste",
                            sigla_uf == "MS" ~ "Centro Oeste",
                            sigla_uf == "GO" ~ "Centro Oeste",
                            sigla_uf == "DF" ~ "Centro Oeste",
                            sigla_uf == "SP" ~ "Sudeste",
                            sigla_uf == "RJ" ~ "Sudeste",
                            sigla_uf == "ES" ~ "Sudeste",
                            sigla_uf == "MG" ~ "Sudeste",
                            sigla_uf == "RS" ~ "Sul",
                            sigla_uf == "SC" ~ "Sul",
                            sigla_uf == "PR" ~ "Sul"))

regioes_soma <- mun_regiao %>% 
  group_by(regiao) %>%
  summarize(total_reg = sum(nr_mun)) %>% 
  mutate(perc = (total_reg/sum(total_reg))*100)


## Gráfico da distribuição de frequência dos municípios por estados na amostra

base_comp_f %>%
  ggplot(aes(sigla_uf, fill = sigla_uf))+
  geom_bar(position = "dodge", alpha = 1)+
  theme_bw()+
  scale_fill_viridis(discrete = TRUE)+
  theme(panel.grid = element_blank())+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, size = 7, hjust = 0, vjust = 0) )+
  labs(title = "Frequência de municípios por estado",
       x = "Estados",
       y = "Quantidade de Municípios")

freq_estado <- as.data.frame(base_comp_f %>% count(base_comp_f$sigla_uf))


################ OBSERVANDO OS DADOS CARREGADOS DO DATASET  ####################

base_comp_f %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 22)

## Estatísticas univariadas

summary(base_comp_f)

## Gerando novo dataset com valores transformados pelo Logarítmo natural
base_comp_f_log <- base_comp_f %>%
  mutate(ln_rh_adm_dir = log(rh_adm_dir),
         ln_area_2015 = log(area_2015),
         ln_area_2019 = log(area_2019),
         ln_PIB_pc_2015 = log(PIB_pc_2015),
         ln_PIB_pc_2019 = log(PIB_pc_2019),
         ln_PIB_pcapita_2015 = log(PIB_pcapita_2015),
         ln_PIB_pcapita_2019 = log(PIB_pcapita_2019),
         ln_pop_est_2015 = log(pop_est_2015),
         ln_pop_est_2019 = log(pop_est_2019))

View(base_comp_f_log)
summary(base_comp_f_log)

##################### ESTUDO DA RELAÇÃO ENTRE VARIÁVEIS ########################

############################# GRÁFICO DE DISPERSÃO #############################


## Variação das áreas Urbanizadas - (2015;2019) x variação da população - (2015;2019)

ggplotly(
  ggplot(base_comp_f, aes(x = var_area, y = var_pop)) +
    geom_point(color = "#000000", size = 1.5, alpha = 0.5) +
    geom_smooth(aes(x = var_area, y = var_pop, color = "Ajuste-linear"),
                method = "lm", se = F, size = 1) +
    geom_smooth(aes(x = var_area, y = var_pop, color = "Ajuste-auto-R"),
                se = F, size = 1)+
    xlab("Variação da área urbanizada - 2015-2019") +
    ylab("Variação da população - 2015-2019") +
    scale_color_manual("Legenda:",
                       values = c("#D55E00", "#56B4E9"))+
    theme_classic()
)

## Estimando o modelo linear simples - variação area-urbanizada x variação população

modelo_area_pop <- lm(formula = var_pop ~ var_area,
                     data = base_comp_f)

## Observando os parâmetros do modelo_area_pop

summary(modelo_area_pop)

summ(modelo_area_pop, confint = T, digits = 4, ci.width = .95)
export_summs(modelo_area_pop, scale = F, digits = 4)


## Gráficos de dispersão e estimação da curva (LOG-LOG)
###LOG-Area urbanizada em 2015 x LOG-População estimada no ano de 2015

ggplotly(
  ggplot(base_comp_f_log, aes(x = ln_pop_est_2015, y = ln_area_2015)) +
    geom_point(color = "#0072B2", size = 1.5, alpha = 0.5) +
    geom_smooth(aes(x = ln_pop_est_2015, y = ln_area_2015), color = "black",
                method = "lm", se = F, size = 1)+
    xlab("LOG-População estimada em 2015") +
    ylab("LOG-Área urbanizada em km² - 2015")+
    theme_classic()
  )
## Gráficos de dispersão e estimação da curva (LOG-LOG)
###LOG-Area urbanizada em 2019 x LOG-População estimada no ano de 2019
ggplotly(
  ggplot(base_comp_f_log, aes(x = ln_pop_est_2019, y = ln_area_2019)) +
    geom_point(color = "#0072B2", size = 1.5, alpha = 0.5) +
    geom_smooth(aes(x = ln_pop_est_2019, y = ln_area_2019),color = "red",
                method = "lm", se = F, size = 1)+
    xlab("LOG-População estimada em 2019") +
    ylab("LOG-Área urbanizada em km² - 2019")+
    theme_classic()
)

## Estimando o modelo - area-urbanizada 2015 x população estimada 2015 valores totais

modelo_area_2015_pop <- lm(formula = ln_area_2015  ~ ln_pop_est_2015,
                       data = base_comp_f_log)

###Observando os parâmetros do modelo_area_pop_vt
summary(modelo_area_2015_pop)

summ(modelo_area_2015_pop, confint = T, digits = 4, ci.width = .95)
export_summs(modelo_area_2015_pop, scale = F, digits = 4)


## Estimando o modelo - area-urbanizada 2019 x população estimada 2019 valores totais

modelo_area_2019_pop <- lm(formula = ln_area_2019  ~ ln_pop_est_2019,
                         data = base_comp_f_log)

###Observando os parâmetros do modelo_area_pop_vt
summary(modelo_area_2019_pop)

summ(modelo_area_2019_pop, confint = T, digits = 4, ci.width = .95)
export_summs(modelo_area_2019_pop, scale = F, digits = 4)


############### A CORRELAÇÃO ENTRE AS VARIÁVEIS SELECIONADAS ###################

#A função chart.Correlation() do pacote PerformanceAnalytics apresenta as
#distribuições das variáveis, scatters, valores das correlações e suas
#respectivas significâncias
summary(base_comp_f_log)

corr_base <- base_comp_f_log[ , c(10, 20, 31, 33, 35, 37)]

View(corr_base)

chart.Correlation(corr_base, histogram = TRUE)

############ CRIANDO NOVA BASE PARA GERAÇÃO DE MODELOS DE REGRESSÃO ############

corr_base_2 <- corr_base %>% 
  rename(ln_urb = "ln_area_2019",
         ln_rh = "ln_rh_adm_dir",
         p_publica = "sum_pol_pub",
         hom = "hom_2019", 
         ln_PIB = "ln_PIB_pc_2019",
         ln_PIB_pcap = "ln_PIB_pcapita_2019")

corr_base_2 %>%
  corr_plot(ln_urb, ln_rh, p_publica, hom, ln_PIB, ln_PIB_pcap,
            shape.point = 21,
            col.point = "black",
            fill.point = "red",
            size.point = 1,
            alpha.point = 0.6,
            maxsize = 3.3,
            minsize = 2,
            smooth = TRUE,
            col.smooth = "black",
            col.sign = "grey",
            upper = "corr",
            lower = "scatter",
            diag.type = "histogram",
            col.diag = "grey",
            pan.spacing = 0,
            lab.position = "bl")

save(corr_base_2, file = "corr_base_2.RData")


################ MODELES LINEARES PARA V.I. = ÁREA URBANIZADA ##################

#Estimando o modelo linear simples - area-urbanizada 2019 x nr. servidores adm. direta

modelo_rh_area_urb <- lm(formula = ln_rh ~ ln_urb,
                         data = corr_base_2)

#Observando os parâmetros do modelo
summary(modelo_rh_area_urb)

summ(modelo_rh_area_urb, confint = T, digits = 4, ci.width = .95)
export_summs(modelo_rh_area_urb, scale = F, digits = 4)

#Plot do modelo
plot(modelo_rh_area_urb)

#Plot dos dados com linha regressão
ggplotly(
  ggplot(corr_base_2, aes(x = ln_urb, y = ln_rh)) +
    geom_point(color = "#0072B2", size = 1.5, alpha = 0.5) +
    geom_smooth(aes(x = ln_urb, y = ln_rh),color = "red",
                method = "lm", se = F, size = 1)+
    xlab("LOG-Área urbanizada 2019") +
    ylab("LOG-Número de servidores adm. direta")+
    theme_classic()
)

## Teste de verificação da aderência dos resíduos à Normalidade - SHAPIRO-FRANCIA                                  #

sf.test(modelo_rh_area_urb) #função sf.test do pacote nortest

#Histograma dos resíduos do modelo_urb_rh
corr_base_2 %>%
  mutate(residuos = modelo_rh_area_urb$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_rh_area_urb$residuals),
                            sd = sd(modelo_rh_area_urb$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

## Transformação de Box-Cox

### O lambda de Box-Cox

lambda_BC <- powerTransform(corr_base_2$ln_rh) #função powerTransform do pacote car#
lambda_BC

### Inserindo o lambda de Box-Cox na base de dados para a estimação de um novo modelo

corr_base_2$bc_ln_rh <- (((corr_base_2$ln_rh ^ lambda_BC$lambda) - 1) / 
                            lambda_BC$lambda)

### Estimando um novo modelo OLS com variável dependente transformada por Box-Cox

modelo_bc_rh_area_urb <- lm(formula = bc_ln_rh ~ ln_urb,
                data = corr_base_2)

### Parâmetros do modelo
summary(modelo_bc_rh_area_urb)

### Comparando os parâmetros do modelo_linear com os do modelo_bc_urb_rh

export_summs(modelo_rh_area_urb, modelo_bc_rh_area_urb, scale = F, digits = 4)

### Qualidade do ajuste para o modelo não linear (R²)

data.frame("R²OLS" = round(summary(modelo_rh_area_urb)$r.squared, 4),
           "R²BoxCox" = round(summary(modelo_bc_rh_area_urb)$r.squared, 4)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center", 
                full_width = F, 
                font_size = 30)

### Teste de Shapiro-Francia para os resíduos do modelo_bc

sf.test(modelo_bc_rh_area_urb$residuals)

### Histograma dos resíduos do modelo_bc

corr_base_2 %>%
  mutate(residuos = modelo_bc_rh_area_urb$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "gray90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_bc_rh_area_urb$residuals),
                            sd = sd(modelo_bc_rh_area_urb$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")


###########################################################################################


###########################################################################################

# Modelo lm - ln_area-urbanizada 2019 x ln_PIB_2019

modelo_urb_PIB<- lm(formula = ln_PIB ~ ln_urb,
                    data = corr_base_2)

#Plot dos dados com linha regressão
ggplotly(
  ggplot(corr_base_2, aes(x = ln_urb, y = ln_PIB)) +
    geom_point(color = "#0072B2", size = 1.5, alpha = 0.5) +
    geom_smooth(aes(x = ln_urb, y = ln_PIB),color = "#666666",
                method = "lm", se = F, size = 1)+
    xlab("LOG-Área urbanizada 2019") +
    ylab("LOG-PIB")+
    theme_classic()
)

#Observando os parâmetros do modelo
summary(modelo_urb_PIB)


summ(modelo_urb_PIB, confint = T, digits = 4, ci.width = .95)
export_summs(modelo_urb_PIB, scale = F, digits = 4)


#           REGRESSÃO NÃO LINEAR SIMPLES E TRANSFORMAÇÃO DE BOX-COX              #
##################################################################################


##################################################################################
#          TESTE DE VERIFICAÇÃO DA ADERÊNCIA DOS RESÍDUOS À NORMALIDADE          #
#                               SHAPIRO-FRANCIA                                  #
##################################################################################

#Shapiro-Francia: n > 30
sf.test(modelo_urb_PIB) #função sf.test do pacote nortest

#Histograma dos resíduos do modelo OLS linear
corr_base_2 %>%
  mutate(residuos = modelo_urb_PIB$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_urb_PIB$residuals),
                            sd = sd(modelo_urb_PIB$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

##################################################################################
#                             TRANSFORMAÇÃO DE BOX-COX                           #
##################################################################################
#Para calcular o lambda de Box-Cox
lambda_BC <- powerTransform(corr_base_2$ln_PIB) #função powerTransform do pacote car#
lambda_BC

#Inserindo o lambda de Box-Cox na base de dados para a estimação de um novo modelo
corr_base_2$bc_PIB <- (((corr_base_2$ln_PIB ^ lambda_BC$lambda) - 1) / 
                         lambda_BC$lambda)

#Estimando um novo modelo OLS com variável dependente transformada por Box-Cox
modelo_bc_urb_PIB <- lm(formula = bc_PIB ~ ln_urb,
                        data = corr_base_2)

#Parâmetros do modelo
summary(modelo_bc_urb_PIB)

#Comparando os parâmetros do modelo_linear com os do modelo_bc
#CUIDADO!!! OS PARÂMETROS NÃO SÃO DIRETAMENTE COMPARÁVEIS!
export_summs(modelo_urb_PIB, modelo_bc_urb_PIB, scale = F, digits = 4)

#Qualidade do ajuste para o modelo não linear (R²)
data.frame("R²OLS" = round(summary(modelo_urb_PIB)$r.squared, 4),
           "R²BoxCox" = round(summary(modelo_bc_urb_PIB)$r.squared, 4)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center", 
                full_width = F, 
                font_size = 30)

#Teste de Shapiro-Francia para os resíduos do modelo_bc
sf.test(modelo_bc_urb_PIB$residuals) #função sf.test do pacote nortest

#Histograma dos resíduos do modelo_bc
corr_base_2 %>%
  mutate(residuos = modelo_bc_urb_PIB$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "gray90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_bc_urb_PIB$residuals),
                            sd = sd(modelo_bc_urb_PIB$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

###########################################################################################
