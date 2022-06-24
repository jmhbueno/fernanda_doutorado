#Pacotes 

if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, car, rstatix, lmtest, ggpubr,
               QuantPsyc, psych, scatterplot3d)


#Modelos de Regressão montados com base nas correlações 

#Projeto de Carreira ====
######
qaes_projcar_reg <- lm(qaes_projcar ~ 
                         tre_efi+tre_ine + 
                         extrover + 
                         neurotic + 
                         conscien + 
                         amabilid, 
                       data = df2, 
                       na.action = na.omit)

summary(qaes_projcar_reg)

# Extroversão e neuroticismo capazes de predizer projeto de carreira
qaes_projcar_reg <- lm(qaes_projcar ~ extrover + neurotic, 
                       data = df2, 
                       na.action = na.omit)
summary(qaes_projcar_reg)

## Análise gráfica:

par(mfrow=c(2,2))

plot(qaes_projcar_reg)

## Normalidade dos resíduos:
shapiro.test(qaes_projcar_reg$residuals)


#Assimetria e Curtose
skew(qaes_projcar_reg$residuals)
kurtosi(qaes_projcar_reg$residuals)

## Outliers nos resíduos:
summary(rstandard(qaes_projcar_reg))
#Valores tem que estar entre -3 e 3
# projcar tem outliers 

## Independência dos resíduos (Durbin-Watson):
durbinWatsonTest(qaes_projcar_reg)
#Valores entre 1,5 e 2,5
#não há autocorrelação

## Homocedasticidade (Breusch-Pagan):
bptest(qaes_projcar_reg)
#p maior que 5% não há homocedasticidade

## Ausência de Multicolinearidade
### Multicolinearidade: r > 0.9 (ou 0.8)
########
#Adaptação Social ====
######
qaes_adptsoc_reg <- lm(qaes_adptsoc ~ 
                         extrover + 
                         neurotic + 
                         conscien + 
                         abertura +
                         amabilid,
                       data = df2, 
                       na.action = na.omit)

summary(qaes_adptsoc_reg)

# extroversão, neuroticismo e amabilidade capaz de predizer adaptação social 
qaes_adptsoc_reg <- lm(qaes_adptsoc ~ 
                         extrover + 
                         neurotic + 
                         amabilid,
                       data = df2, 
                       na.action = na.omit)
summary(qaes_adptsoc_reg)
## Análise gráfica:

par(mfrow=c(2,2))

plot(qaes_adptsoc_reg)

## Normalidade dos resíduos:
shapiro.test(qaes_adptsoc_reg$residuals)


#Assimetria e Curtose
skew(qaes_adptsoc_reg$residuals)
kurtosi(qaes_adptsoc_reg$residuals)

## Outliers nos resíduos:
summary(rstandard(qaes_adptsoc_reg))
#Valores tem que estar entre -3 e 3


## Independência dos resíduos (Durbin-Watson):
durbinWatsonTest(qaes_adptsoc_reg)
#Valores entre 1,5 e 2,5
#não há autocorrelação

## Homocedasticidade (Breusch-Pagan):
bptest(qaes_adptsoc_reg)
#p maior que 5% não há homocedasticidade

## Ausência de Multicolinearidade
### Multicolinearidade: r > 0.9 (ou 0.8)
#######


#Adaptação pessoal emocional =====
#######
qaes_apesemo_reg <- lm(qaes_apesemo ~ 
                         extrover + 
                         neurotic + 
                         conscien,
                       data = df2, 
                       na.action = na.omit)

summary(qaes_apesemo_reg)

#Extroversão e neuroticismo e conscienciosidade capazes de predizer adaptação pessoal emocional
## Análise gráfica:

par(mfrow=c(2,2))

plot(qaes_apesemo_reg)

## Normalidade dos resíduos:
shapiro.test(qaes_apesemo_reg$residuals)


#Assimetria e Curtose
skew(qaes_apesemo_reg$residuals)
kurtosi(qaes_apesemo_reg$residuals)

## Outliers nos resíduos:
summary(rstandard(qaes_apesemo_reg))
#Valores tem que estar entre -3 e 3


## Independência dos resíduos (Durbin-Watson):
durbinWatsonTest(qaes_apesemo_reg)
#Valores entre 1,5 e 2,5
#não há autocorrelação

## Homocedasticidade (Breusch-Pagan):
bptest(qaes_apesemo_reg)
#p maior que 5% não há homocedasticidade

## Ausência de Multicolinearidade
### Multicolinearidade: r > 0.9 (ou 0.8)
######
#Adaptação ao estudo ====
######
qaes_adptest_reg <- lm(qaes_adptest ~ 
                         extrover + 
                         neurotic + 
                         conscien + 
                         abertura +
                         amabilid,
                       data = df2, 
                       na.action = na.omit)

summary(qaes_adptest_reg)

# extroversão, consciencienciosidade e abertura predizem adaptação ao estudo
qaes_adptest_reg <- lm(qaes_adptest ~ extrover + conscien + abertura, 
                       data = df2, 
                       na.action = na.omit)
summary(qaes_adptest_reg)
## Análise gráfica:

par(mfrow=c(2,2))

plot(qaes_adptest_reg)

## Normalidade dos resíduos:
shapiro.test(qaes_adptest_reg$residuals)


#Assimetria e Curtose
skew(qaes_adptest_reg$residuals)
kurtosi(qaes_adptest_reg$residuals)

## Outliers nos resíduos:
summary(rstandard(qaes_adptest_reg))
#Valores tem que estar entre -3 e 3


## Independência dos resíduos (Durbin-Watson):
durbinWatsonTest(qaes_adptest_reg)
#Valores entre 1,5 e 2,5
#não há autocorrelação

## Homocedasticidade (Breusch-Pagan):
bptest(qaes_adptest_reg)
#p maior que 5% não há homocedasticidade

## Ausência de Multicolinearidade
### Multicolinearidade: r > 0.9 (ou 0.8)
######


#Adaptação a instituição ====
##########
qaes_adptins_reg <- lm(qaes_adptins ~ 
                         extrover + 
                         conscien + 
                         amabilid,
                       data = df2, 
                       na.action = na.omit)

summary(qaes_adptins_reg)

# extroversão prediz adaptação a instituição
qaes_adptins_reg <- lm(qaes_adptest ~ extrover, 
                       data = df2, 
                       na.action = na.omit)
summary(qaes_adptins_reg)

## Análise gráfica:

par(mfrow=c(2,2))

plot(qaes_adptins_reg)

## Normalidade dos resíduos:
shapiro_projcar <- 
# shapiro.test(qaes_projcar_reg$residuals)
# shapiro_projcar$statistic
# shapiro_projcar$p.value

#Assimetria e Curtose
skew(qaes_adptins_reg$residuals)
kurtosi(qaes_adptins_reg$residuals)

## Outliers nos resíduos:
summary(rstandard(qaes_adptins_reg))
#Valores tem que estar entre -3 e 3


## Independência dos resíduos (Durbin-Watson):
durbinWatsonTest(qaes_adptins_reg)
#Valores entre 1,5 e 2,5
#não há autocorrelação

## Homocedasticidade (Breusch-Pagan):
bptest(qaes_adptins_reg)
#p maior que 5% não há homocedasticidade

## Ausência de Multicolinearidade
### Multicolinearidade: r > 0.9 (ou 0.8)
######
###### QAES-Total ====
qaes_total_reg <- lm(qaes_totalsc~ 
                         extrover + 
                         conscien + 
                        neurotic+
                         amabilid,
                       data = df2, 
                       na.action = na.omit)

summary(qaes_total_reg)

## Análise gráfica:

par(mfrow=c(2,2))

plot(qaes_total_reg)

## Normalidade dos resíduos:
shapiro.test(qaes_total_reg$residuals)


#Assimetria e Curtose
skew(qaes_total_reg$residuals)
kurtosi(qaes_total_reg$residuals)

## Outliers nos resíduos:
summary(rstandard(qaes_total_reg))
#Valores tem que estar entre -3 e 3


## Independência dos resíduos (Durbin-Watson):
durbinWatsonTest(qaes_total_reg)
#Valores entre 1,5 e 2,5
#não há autocorrelação

## Homocedasticidade (Breusch-Pagan):
bptest(qaes_total_reg)
#p maior que 5% não há homocedasticidade

## Ausência de Multicolinearidade
### Multicolinearidade: r > 0.9 (ou 0.8)

## Obtenção dos coeficientes padronizados
lm.beta(qaes_total_reg)
########

mediag_reg <- lm(mediag ~ qaes_adptest + 
                         extrover + 
                         neurotic + 
                         conscien + 
                         abertura +
                         amabilid +
                         rv,
                       data = df2, 
                       na.action = na.omit)

summary(mediag_reg)

# Tabela: Assimetria e Curtose ====

sw_projcar <- shapiro.test(qaes_projcar_reg$residuals)
sw_adptsoc <- shapiro.test(qaes_adptsoc_reg$residuals)
sw_apesemo <- shapiro.test(qaes_apesemo_reg$residuals)
sw_adptest <- shapiro.test(qaes_adptest_reg$residuals)
sw_adptins <- shapiro.test(qaes_adptins_reg$residuals)
sw_totalsc <- shapiro.test(qaes_total_reg$residuals)


data.frame(Variáveis = c("Projeto de Carreira",
                         "Adaptação Social",
                         "Adapação pessoal/emocional",
                         "Adaptação ao estudo",
                         "Adaptação à Instituição",
                         "QAES-TOTAL"),
           "Shapiro-Wilk(W)" = c(round(sw_projcar$statistic, digits = 2),
                                 round(sw_adptsoc$statistic, digits = 2),
                                 round(sw_apesemo$statistic, digits = 2),
                                 round(sw_adptest$statistic, digits = 2),
                                 round(sw_adptins$statistic, digits = 2),
                                 round(sw_totalsc$statistic, digits = 2)),
                "p.valor" = c(round(sw_projcar$p.value, digits = 2),
                              round(sw_adptsoc$p.value, digits = 2),
                              round(sw_apesemo$p.value, digits = 2),
                              round(sw_adptest$p.value, digits = 2),
                              round(sw_adptins$p.value, digits = 2),
                              round(sw_totalsc$p.value, digits = 2)),
           Assimetria = c(round(skew(qaes_projcar_reg$residuals), digits = 2),
                          round(skew(qaes_adptsoc_reg$residuals), digits = 2),
                          round(skew(qaes_apesemo_reg$residuals), digits = 2),
                          round(skew(qaes_adptest_reg$residuals), digits = 2),
                          round(skew(qaes_adptins_reg$residuals), digits = 2),
                          round(skew(qaes_total_reg$residuals), digits = 2)),
           Curtose = c(round(kurtosi(qaes_projcar_reg$residuals),digits = 2),
                       round(kurtosi(qaes_adptsoc_reg$residuals),digits = 2),
                       round(kurtosi(qaes_apesemo_reg$residuals),digits = 2),
                       round(kurtosi(qaes_adptest_reg$residuals),digits = 2),
                       round(kurtosi(qaes_adptins_reg$residuals),digits = 2),
                       round(kurtosi(qaes_total_reg$residuals), digits = 2)))

# Nenhuma variável atendeu ao critério de normalidade pelo teste de Shapiro-Wilk
# Este teste é mais rigoroso e Maroco recomenda a utilização de assimetria e curtose entre -0.5 e 0.5
# Pelo critério de assimetria e curtose, somente duas variáveis não atendem.
# Porém, [autores Angélica] dizem que não há problema quanto à normalidade quando a amostra é superior a ....
# ver trabalho de Angélica

# Análise gráfica da distribuição dos resíduos ====

par(mfrow=c(1,1))

plot(qaes_projcar_reg)

data.frame(res = qaes_projcar_reg$residuals) %>% rstatix::identify_outliers(res)
data.frame(res = qaes_adptsoc_reg$residuals) %>% rstatix::identify_outliers(res)
data.frame(res = qaes_apesemo_reg$residuals) %>% rstatix::identify_outliers(res)
data.frame(res = qaes_adptest_reg$residuals) %>% rstatix::identify_outliers(res)
data.frame(res = qaes_adptins_reg$residuals) %>% rstatix::identify_outliers(res)
data.frame(res = qaes_total_reg$residuals) %>% rstatix::identify_outliers(res)

describe(df2$qaes_projcar) %>% pander()

hist(df2$qaes_projcar,breaks = 15)

# Para eliminar glimpse(df2)

só_pra_ver <- df2 %>% slice(-c(151,152))
glimpse(só_pra_ver)

write.csv(df2, "data_fernanda.csv")
