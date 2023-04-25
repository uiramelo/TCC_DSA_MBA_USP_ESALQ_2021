list.files()

load(file = "base_comp_f")

base_teste <-corr_base_2 %>% 
  select(rh, urb, hom, PIB,PIB_pc)

base_log <- log(base_teste)


ggplotly(
ggplot(base_teste, aes(x = urb, y = PIB)) +
         geom_point(color = "#000000", size = 1.5, alpha = 0.5) +
         geom_smooth(aes(x = urb, y = PIB, color = "Ajuste-linear"),
                     method = "lm", se = F, size = 1) +
         geom_smooth(aes(x = urb, y = PIB, color = "Ajuste-auto-R"),
                     se = F, size = 1)+
         xlab("Area") +
         ylab("PIB") +
         scale_color_manual("Legenda:",
                            values = c("#D55E00", "#56B4E9"))+
         theme_classic()
)

############################ PIB ######################################
modelo_pib <- lm(formula = PIB ~ urb,
                    data = base_log)
plot(modelo_pib)

summary(modelo_pib)

############################ PIB per capita#############################
modelo_pib_pc <- lm(formula = PIB_pc ~ urb,
                 data = base_log)
plot(modelo_pib_pc)

summary(modelo_pib_pc)


############################ RH ######################################
modelo_rh <- lm(formula = rh ~ urb,
                 data = base_log)
plot(modelo_rh)

summary(modelo_rh)

############################ Homicídios################################
modelo_hom <- lm(formula = hom ~ urb,
                data = base_log)
plot(modelo_hom)

summary(modelo_hom)
