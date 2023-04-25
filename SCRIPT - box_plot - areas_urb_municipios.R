################ Análise das áreas urbanizadas ####################

install.packages("tidyverse")
install.packages("kableExtra")
library(tidyverse)
library(ggplot2)
library(kableExtra)
###########Box Plot####################

(p1 <- ggplot(base_comp_f,aes(x = prop_urb_2019, y = ""))+
   stat_boxplot(geom = "errorbar")+
   geom_boxplot(lwd=0.5,fill = "lightgrey", outlier.color = "navyblue",outlier.size = 2)+
   theme_bw()+
   labs(title = "Proporção: área urbanizada/área do município", x = "", y ="")+
   coord_cartesian(xlim = c(0,3.1))+
   scale_x_continuous(breaks = seq(0,3,0.5), expand = expand_scale(mult = c(0,0)))+
   theme(text = element_text(size=15),
   axis.text = element_text(size = 13)))
 
#################################################################################
#                 OBSERVANDO OS DADOS CARREGADOS DO DATASET tempodist           #
#################################################################################
m_area_urb <-base_comp_f %>% 
  select(nome, sigla_uf, prop_urb_2019)

m_area_urb <- as.data.frame(m_area_urb)

m_area_urb %>%  
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 22)

write.xlsx(m_area_urb, file = "Municipios_areas_urbanizadas.xlsx")

#Estatísticas univariadas
summary(base_comp_f)
