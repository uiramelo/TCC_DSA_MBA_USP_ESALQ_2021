
############# INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS #################

##Pacotes utilizados
pacotes <- c("plotly","tidyverse","ggrepel","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
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


## Gráfico de dispersão Area urbanizada em 2019 x População estimada no ano de 2019
###para valores totais

ggplotly(
  ggplot(base_comp_f, aes(x = area_2019, y = (pop_est_2019/1000))) +
    geom_point(color = "#0072B2", size = 1.5, alpha = 0.5) +
    geom_smooth(aes(x = area_2019, y = (pop_est_2019/1000), color = "Ajuste-linear"),
                method = "lm", se = F, size = 1) +
    geom_smooth(aes(x = area_2019, y = (pop_est_2019/1000), color = "Ajuste-auto-R"),
                se = F, size = 1)+
    xlab("Área urbanizada em km² - 2019") +
    ylab("População estimada em 2019 (p 1000)") +
    scale_color_manual("Legenda:",
                       values = c("#F0E442", "black"))+
    theme(legend.postion = "top")+
    theme_classic()
  )

## Estimando o modelo linear simples - area-urbanizada 2019 x população estimada 2019 valores totais

modelo_area_pop_vt <- lm(formula = pop_est_2019 ~ area_2019,
                       data = base_comp_f)

###Observando os parâmetros do modelo_area_pop_vt
summary(modelo_area_pop_vt)

summ(modelo_area_pop_vt, confint = T, digits = 4, ci.width = .95)
export_summs(modelo_area_pop_vt, scale = F, digits = 4)


############### A CORRELAÇÃO ENTRE AS VARIÁVEIS SELECIONADAS ###################

#A função chart.Correlation() do pacote PerformanceAnalytics apresenta as
#distribuições das variáveis, scatters, valores das correlações e suas
#respectivas significâncias

corr_base <- base_comp_f[ , c(3, 9, 10, 12, 18, 20, 24, 25)]

chart.Correlation(corr_base, histogram = TRUE)

############ CRIANDO NOVA BASE PARA GERAÇÃO DE MODELOS DE REGRESSÃO ############

corr_base_2 <- corr_base %>% 
  rename(urb = "area_2019",
         p_urb = "prop_urb_2019",
         rh = "rh_adm_dir",
         prh = "prop_rh_pop",
         pol_p = "sum_pol_pub",
         hom = "hom_2019", 
         PIB = "PIB_pc_2019",
         PIB_pc = "PIB_pcapita_2019")

corr_base_2 %>%
  corr_plot(urb, p_urb, rh, prh, pol_p, hom, PIB, PIB_pc,
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


################ MODELES LINEARES PARA V.I. - ÁREA URBANIZADA ##################

## Estimando o modelo linear simples - area-urbanizada 2019 x nr. servidores adm. direta

modelo_urb_rh <- lm(formula = rh ~ urb,
                         data = corr_base_2)

## Observando os parâmetros do modelo
summary(modelo_urb_rh)

summ(modelo_urb_rh, confint = T, digits = 4, ci.width = .95)
export_summs(modelo_urb_rh, scale = F, digits = 4)


## Teste de verificação da aderência dos resíduos à Normalidade - SHAPIRO-FRANCIA                                  #

sf.test(modelo_urb_rh$residuals) #função sf.test do pacote nortest

## Histograma dos resíduos do modelo_urb_rh
corr_base_2 %>%
  mutate(residuos = modelo_urb_rh$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_urb_rh$residuals),
                            sd = sd(modelo_urb_rh$residuals)),
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

lambda_BC <- powerTransform(corr_base_2$rh) #função powerTransform do pacote car#
lambda_BC

### Inserindo o lambda de Box-Cox na base de dados para a estimação de um novo modelo

corr_base_2$bc_rh <- (((corr_base_2$rh ^ lambda_BC$lambda) - 1) / 
                            lambda_BC$lambda)

### Estimando um novo modelo OLS com variável dependente transformada por Box-Cox

modelo_bc_urb_rh <- lm(formula = bc_rh ~ urb,
                data = corr_base_2)

### Parâmetros do modelo
summary(modelo_bc_urb_rh)

### Comparando os parâmetros do modelo_linear com os do modelo_bc_urb_rh

export_summs(modelo_urb_rh, modelo_bc_urb_rh, scale = F, digits = 4)

### Qualidade do ajuste para o modelo não linear (R²)

data.frame("R²OLS" = round(summary(modelo_urb_rh)$r.squared, 4),
           "R²BoxCox" = round(summary(modelo_bc_urb_rh)$r.squared, 4)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center", 
                full_width = F, 
                font_size = 30)

### Teste de Shapiro-Francia para os resíduos do modelo_bc

sf.test(modelo_bc_urb_rh$residuals)

### Histograma dos resíduos do modelo_bc

corr_base_2 %>%
  mutate(residuos = modelo_bc_urb_rh$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "gray90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_bc_urb_rh$residuals),
                            sd = sd(modelo_bc_urb_rh$residuals)),
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

## Modelo lm - area-urbanizada 2019 x PIB

modelo_urb_PIB<- lm(formula = PIB ~ urb,
                    data = corr_base_2)

##Observando os parâmetros do modelo
summary(modelo_urb_PIB)

summ(modelo_urb_PIB, confint = T, digits = 4, ci.width = .95)
export_summs(modelo_urb_PIB, scale = F, digits = 4)


## Shapiro-Francia: n > 30
sf.test(modelo_urb_PIB$residuals) #função sf.test do pacote nortest

## Histograma dos resíduos do modelo OLS linear

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

## Transformação de Box-Cox

## Para calcular o lambda de Box-Cox

lambda_BC <- powerTransform(corr_base_2$PIB) #função powerTransform do pacote car#
lambda_BC

## Inserindo o lambda de Box-Cox na base de dados para a estimação de um novo modelo

corr_base_2$bc_PIB <- (((corr_base_2$PIB ^ lambda_BC$lambda) - 1) / 
                         lambda_BC$lambda)

## Estimando um novo modelo OLS com variável dependente transformada por Box-Cox

modelo_bc_urb_PIB <- lm(formula = bc_PIB ~ urb,
                        data = corr_base_2)

### Parâmetros do modelo

summary(modelo_bc_urb_PIB)

### Comparando os parâmetros do modelo_linear com os do modelo_bc_urb_PIB

export_summs(modelo_urb_PIB, modelo_bc_urb_PIB, scale = F, digits = 4)

### Qualidade do ajuste para o modelo não linear (R²)
data.frame("R²OLS" = round(summary(modelo_urb_PIB)$r.squared, 4),
           "R²BoxCox" = round(summary(modelo_bc_urb_PIB)$r.squared, 4)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center", 
                full_width = F, 
                font_size = 30)

## Teste de Shapiro-Francia para os resíduos do modelo_bc
sf.test(modelo_bc_urb_PIB$residuals) #função sf.test do pacote nortest

## Histograma dos resíduos do modelo_bc
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
                size = 1) +
  scale_color_manual("Legenda:",
                     values = "blue") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey80"),
        panel.border = element_rect(NA),
        legend.position = "bottom")
################################################################################

###### GERANDO MODELOS COM AS VAR. DEPENDENTES log_rh, log_PIB E log_urb ####### 

corr_base_log <-corr_base_2 %>% 
  mutate(log_urb = (log(urb))) %>% 
  mutate(log_rh = (log(rh))) %>%
  mutate(log_pib = (log(PIB)))

View(corr_base_log)

sum(is.na(corr_base_log))

save(corr_base_log, file = "corr_base_log.RData")

########################## MODELO PIB - ÁREA URBANA ############################

modelo_log_pib <- lm(formula = log_pib ~ log_urb,
                 data = corr_base_log)

plot(modelo_log_pib)

summary(modelo_log_pib)

summ(modelo_log_pib, confint = T, digits = 4, ci.width = .95)
export_summs(modelo_log_pib, scale = F, digits = 4)


## Shapiro-Francia: n > 30

shapiro.test(modelo_log_pib$residuals) #função sf.test do pacote nortest

## Histograma dos resíduos do modelo OLS linear

corr_base_log %>%
  mutate(residuos = modelo_log_pib$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_log_pib$residuals),
                            sd = sd(modelo_log_pib$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 1) +
  scale_color_manual("Legenda:",
                     values = "green") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey80"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

## Transformação de Box-Cox

## Para calcular o lambda de Box-Cox

lambda_BC <- powerTransform(corr_base_log$log_pib) #função powerTransform do pacote car#
lambda_BC

## Inserindo o lambda de Box-Cox na base de dados para a estimação de um novo modelo

corr_base_log$bc_log_pib <- (((corr_base_log$log_pib ^ lambda_BC$lambda) - 1) / 
                         lambda_BC$lambda)

## Estimando um novo modelo OLS com variável dependente transformada por Box-Cox

modelo_bc_log_pib <- lm(formula = bc_log_pib ~ log_urb,
                        data = corr_base_log)

### Parâmetros do modelo

summary(modelo_bc_log_pib)

### Comparando os parâmetros do modelo_linear com os do modelo_bc_log_pib

export_summs(modelo_log_pib, modelo_bc_log_pib, scale = F, digits = 4)

### Qualidade do ajuste para o modelo não linear (R²)

data.frame("R²OLS" = round(summary(modelo_log_pib)$r.squared, 4),
           "R²BoxCox" = round(summary(modelo_bc_log_pib)$r.squared, 4)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center", 
                full_width = F, 
                font_size = 30)

## Teste de Shapiro-Francia para os resíduos do modelo_bc
sf.test(modelo_bc_log_pib$residuals) 

## Histograma dos resíduos do modelo_bc

corr_base_log %>%
  mutate(residuos = modelo_bc_log_pib$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "gray90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_bc_log_pib$residuals),
                            sd = sd(modelo_bc_log_pib$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 1) +
  scale_color_manual("Legenda:",
                     values = "red") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey80"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

################################################################################

################ MODELO Nr. SERVIDORES ADM. DIRETA - ÁREA URBANA ###############

modelo_log_rh <- lm(formula = log_rh ~ log_urb,
                     data = corr_base_log)

plot(modelo_log_rh)

summary(modelo_log_rh)

summ(modelo_log_rh, confint = T, digits = 4, ci.width = .95)
export_summs(modelo_log_rh, scale = F, digits = 4)


## Shapiro-Francia: n > 30

shapiro.test(modelo_log_rh$residuals)

## Histograma dos resíduos do modelo OLS linear

corr_base_log %>%
  mutate(residuos = modelo_log_rh$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_log_rh$residuals),
                            sd = sd(modelo_log_rh$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 1) +
  scale_color_manual("Legenda:",
                     values = "purple") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey80"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

## Transformação de Box-Cox

## Para calcular o lambda de Box-Cox

lambda_BC <- powerTransform(corr_base_log$log_rh) #função powerTransform do pacote car#
lambda_BC

## Inserindo o lambda de Box-Cox na base de dados para a estimação de um novo modelo

corr_base_log$bc_log_rh <- (((corr_base_log$log_rh ^ lambda_BC$lambda) - 1) / 
                               lambda_BC$lambda)

## Estimando um novo modelo OLS com variável dependente transformada por Box-Cox

modelo_bc_log_rh <- lm(formula = bc_log_rh ~ log_urb,
                        data = corr_base_log)

### Parâmetros do modelo

summary(modelo_bc_log_rh)

### Comparando os parâmetros do modelo_linear com os do modelo_bc_log_rh

export_summs(modelo_log_rh, modelo_bc_log_rh, scale = F, digits = 4)

### Qualidade do ajuste para o modelo não linear (R²)

data.frame("R²OLS" = round(summary(modelo_log_rh)$r.squared, 4),
           "R²BoxCox" = round(summary(modelo_bc_log_rh)$r.squared, 4)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center", 
                full_width = F, 
                font_size = 30)

## Teste de Shapiro-Francia para os resíduos do modelo_bc
sf.test(modelo_bc_log_rh$residuals) 

## Histograma dos resíduos do modelo_bc

corr_base_log %>%
  mutate(residuos = modelo_bc_log_rh$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "gray90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_bc_log_rh$residuals),
                            sd = sd(modelo_bc_log_rh$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 1) +
  scale_color_manual("Legenda:",
                     values = "brown") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey80"),
        panel.border = element_rect(NA),
        legend.position = "bottom")
