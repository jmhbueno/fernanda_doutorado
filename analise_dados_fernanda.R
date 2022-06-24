#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# INSTALACAO E ATIVACAO DOS PACOTES ====
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

install.packages("GPArotation")
install.packages("tidyverse")
install.packages("Rtools")
install.packages("haven")
install.packages("readr")
install.packages("readxl")
install.packages("mirt")
install.packages("stringr")
install.packages("latticeExtra")
install.packages('pillar')
install.packages("psych")
install.packages("knitr")
install.packages("gt")

library(GPArotation) # Pacote para rotacao analise fatorial.
library(tidyverse)
library(psych)
library(knitr)
library(readxl)
library(lavaan)
library(semPlot)
library(haven)
library(foreign)
library(pillar)
library(mirt)
library(igraph)
library(igraphdata)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ANALISE BANCO DE DADOS FERNANDA ====
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Importação do banco de dados baixado do GoogleForms

df1<- read_xlsx("Habilidades Socioemocioniais e Adaptação Acadêmica em Universitários  (respostas).xlsx")
names(df1)
view(df1)
glimpse(df1)
df1 <- df1[ ,-c(1:4)] # deletar quatro primeiras colunas do arquivo baixado do GoogleForms

df2 <- df1 %>% rename_all(list(~c("idade","genero","estado","cor","eb","es","ies",
                                  "anocurso","curso","mediag","turno","formato","opcaouni","opçãocurso",
                                  "a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11","a12",
                                  "a13","a14","a15","a16","a17","a18","a19","a20","a21","a22","a23","a24",
                                  "a25","a26","a27","a28","a29","a30","a31","a32","a33","a34","a35","a36",
                                  "a37","a38","a39","a40","ce1","ce2","ce3","ce4","ce5","ce6","ce7","ce8",
                                  "ce9","ce10","ce11","ce12","ce13","ce14","ce15","ce16","ce17","ce18","re1","re2",
                                  "re3","re4","re5","re6","re7","re8","re9","re10","re11","re12","re13","re14",
                                  "re15","re16","re17","re18","re19","re20","re21","re22","re23","re24","rv1","rv2",
                                  "rv3","rv4","rv5","rv6","rv7","rv8","rv9","rv10","rv11","rv12","p1","p2",
                                  "p3","p4","p5","p6","p7","p8","p9","p10","p11","p12","p13","p14",
                                  "p15","p16","p17","p18","p19","p20","p21","p22","p23","p24","p25","p26",
                                  "p27","p28","p29","p30","p31","p32","p33","p34","p35","p36","p37","p38",
                                  "p39","p40","p41","p42","p43","p44","ib1","ib2","ib3","ib4","ib5","ib6",
                                  "ib7","ib8","ib9","ib10","ib11","ib12","ib13","ib14","ib15","ib16","ib17","ib18")))
glimpse(df2)
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ESTATISTICAS DESCRITIVAS ==== 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df2 %>%                                     # pega o objeto (dataframe) df2
  summarise(                                # e aplica a funcao summarise, que monta uma tabela com os dados solicitados
    média_idade = mean(df2$idade, na.rm = T), # que, nesse caso, são: 1) a media da idade, removendo os dados ausentes (NA)
    desvio_padrão = sd(df2$idade, na.rm = T)) # e o desvio padrao da idade, removendo os dados ausentes (NA)

range(df2$idade) # reporta o menor e o maior valor da variável
nclass.Sturges(df2$idade)# em quantas categorias eu poderia classificar a variável idade
table(cut(df2$idade,seq(15,65,l = 10))) # cria uma tabela por classes de idade
                                        # cut(comando para cortar os valores
                                        # seq(valor mínimo, valor máximo , l = número de classes)
                                        # no número de classes, colocar uma a mais do que indicou o nclass.Sturges 

describe(df2$idade) # comando do pacote psych, que ja traz uma serie de estatisticas descritivas
describeBy(df2[ ,c("idade","mediag")],group = df2$genero) # describeBy: estatística descritiva por grupo, no caso "genero"
describeBy(df2[ ,c("idade","mediag")],group = df2$cor)
describeBy(df2[ ,c("idade","mediag")],group = df2$genero:df2$cor) # comando para agrupar por genero e cor

idade_table <- df2$idade %>% table() %>% as.data.frame()
idade_table$"%" <- idade_table$Freq %>% prop.table()*100

glimpse(df2[ ,1:14])

df2$mediag <- as.numeric(df2$mediag)

amostra_nota_media <- df2 %>% 
  summarise(
    média_idade = mean(df2$mediag, na.rm = T),
    desvio_padrão = sd(df2$mediag, na.rm = T))


df2 %>% count(genero) %>% mutate("%" = n/sum(n)*100)
df2 %>% count(cor) %>% mutate("%" = n/sum(n)*100)

df2$genero <- df2$genero %>% as.factor() 
levels(df2$genero) <- c("Feminino","Masculino","Não_binário")

df2$cor <- df2$cor %>% as.factor()
levels(df2$cor) <- c("Preto","Pardo","Branco","Indígena","Amarelo","Prefiro não informar")

amostra_sexo_cor <- df2 %>% group_by(genero) %>% count(cor) %>% mutate("%" = n/sum(n)*100)
amostra_sexo_cor <- df2 %>% count(genero,cor) %>% mutate("%" = n/sum(n)*100)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# PROPRIEDADES PSICOMETRICAS DOS INSTRUMENTOS ====
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

names(df2[ ,1:60])

qaes <- df2 %>% select(15:54) # criacao de um objeto (dataframe) só com os itens do QAES

# calculo dos coeficientes alfa de cronbach ============================================================

alpha_qaes_projcar <- qaes %>% select(5,10,15,20,25,30,35,40)%>% alpha()
alpha_qaes_adptsoc <- qaes %>% select(2,7,12,17,22,27,32,37) %>% alpha()
alpha_qaes_apesemo <- qaes %>% select(4,9,14,19,39,29,34,24) %>% alpha()
alpha_qaes_adptest <- qaes %>% select(3,38,13,18,23,28,33,8) %>% alpha()
alpha_qaes_adptins <- qaes %>% select(1,6,11,16,21,26,36,31) %>% alpha()


# OBS.: Todos os itens do fator apesemo estão na direção contrária da adaptação.
# Por isso as correlações desse fator com os demais dão negativas.

# ver tabela com os valores de alfa se o item for deletado 
alpha_qaes_projcar$alpha.drop 
alpha_qaes_adptsoc$alpha.drop
alpha_qaes_apesemo$alpha.drop
alpha_qaes_adptest$alpha.drop
alpha_qaes_adptins$alpha.drop

# calculo dos coeficientes omega =====================================================================
# uma vantagem do omega e que ele tambem informa o alfa
# no entanto, nao informa o alfa se o item for deletado

rely_qaes_projcar<-qaes %>% select(5,10,15,20,25,30,35,40) %>% omega(poly=TRUE)
rely_qaes_adptsoc<-qaes %>% select(2,7,12,17,22,27,32,37) %>% omega(poly=TRUE)
rely_qaes_apesemo<-qaes %>% select(4,9,14,19,39,29,34,24) %>% omega(poly=TRUE,key = c(-1,-1,-1,-1,-1,-1,-1,-1))
rely_qaes_adptest<-qaes %>% select(3,38,13,18,23,28,33,8) %>% omega(poly=TRUE)
rely_qaes_adptins<-qaes %>% select(1,6,11,16,21,26,36,31) %>% omega(poly=TRUE)

#rely_qaes_totalsc<-qaes %>% select(41:45) %>% omega(poly=F)

rely_qaes_totalsc<-qaes %>% select(41,42,44:46) %>% omega(poly=F)

rely_qaes_totalsc$omega.tot
rely_qaes_totalsc$alpha

# rely_qaes_projcar - Projeto de Carreira
# rely_qaes_adptsoc - Adaptacao social
# rely_qaes_apesemo - Adaptacao pessoal e emocional
# rely_qaes_adptest - Adaptacao ao estudo
# rely_qaes_adptins - Adaptacao institucional

projcar_alpha <- rely_qaes_projcar$alpha
adptsoc_alpha <- rely_qaes_adptsoc$alpha
apesemo_alpha <- rely_qaes_apesemo$alpha
adptest_alpha <- rely_qaes_adptest$alpha
adptins_alpha <- rely_qaes_adptins$alpha

projcar_omega <- rely_qaes_projcar$omega.tot
adptsoc_omega <- rely_qaes_adptsoc$omega.tot
apesemo_omega <- rely_qaes_apesemo$omega.tot
adptest_omega <- rely_qaes_adptest$omega.tot
adptins_omega <- rely_qaes_adptins$omega.tot

# Calculo dos escores nos fatores ====================================================================

# Inversão dos valores dos itens do fator apesemo que estão invertidos
expss::recode(qaes[ ,c('a4','a9','a14','a19','a39','a29','a34','a24')]) <- c(1~5,2~4,3~3,4~2,5~1)

qaes$qaes_projcar <- qaes %>% select(5,10,15,20,25,30,35,40) %>% rowMeans()
qaes$qaes_adptsoc <- qaes %>% select(2, 7,12,17,22,27,32,37) %>% rowMeans()
qaes$qaes_apesemo <- qaes %>% select(4, 9,14,19,39,29,34,24) %>% rowMeans()
qaes$qaes_adptest <- qaes %>% select(3,38,13,18,23,28,33, 8) %>% rowMeans()
qaes$qaes_adptins <- qaes %>% select(1, 6,11,16,21,26,36,31) %>% rowMeans()

df2$qaes_apesemo <- qaes$qaes_apesemo

glimpse(qaes)
names(df2)
# estatisticas descritivas do qaes

projcar_media <- mean(qaes$qaes_projcar,na.rm = TRUE)
adptsoc_media <- mean(qaes$qaes_adptsoc,na.rm = TRUE)
apesemo_media <- mean(qaes$qaes_apesemo,na.rm = TRUE)
adptest_media <- mean(qaes$qaes_adptest,na.rm = TRUE)
adptins_media <- mean(qaes$qaes_adptins,na.rm = TRUE)

projcar_dsvpd <- sd(qaes$qaes_projcar,na.rm = TRUE)
adptsoc_dsvpd <- sd(qaes$qaes_adptsoc,na.rm = TRUE)
apesemo_dsvpd <- sd(qaes$qaes_apesemo,na.rm = TRUE)
adptest_dsvpd <- sd(qaes$qaes_adptest,na.rm = TRUE)
adptins_dsvpd <- sd(qaes$qaes_adptins,na.rm = TRUE)

qaes_escores <- qaes %>% select(41:45) # selecao das colunas com os escores e salvar no objeto qaes_escores

df2 <- cbind(df2,qaes_escores)         # combinação de df2 com qaes_escores
names(df2)

df2$qaes_totalsc <- df2 %>% select(171:175) %>% rowMeans()
# tabela com estatísticas descritivas da QAES para a tese

df_qaes_descritivas <- data.frame(variáveis = c("Projeto de Carreira",
                                                "Adaptação Social",
                                                "Adaptação Pessoal e Emocional",
                                                "Adaptação ao Estudo",
                                                "Adaptação Institucional"),
                                  média = c(projcar_media,
                                            adptsoc_media,
                                            apesemo_media,
                                            adptest_media,
                                            adptins_media),
                                  "desvio padrão" = c(projcar_dsvpd,
                                                      adptsoc_dsvpd,
                                                      apesemo_dsvpd,
                                                      adptest_dsvpd,
                                                      adptins_dsvpd),
                                  alfa = c(projcar_alpha,
                                           adptsoc_alpha,
                                           apesemo_alpha,
                                           adptest_alpha,
                                           adptins_alpha),
                                  omega = c(projcar_alpha,
                                            adptsoc_alpha,
                                            apesemo_alpha,
                                            adptest_alpha,
                                            adptins_alpha)) 

df_qaes_descritivas %>% kable(format = "rst", escape = FALSE)

pairs.panels(qaes_escores)

#TCE

tce <- df2 %>% select(55:72)

tce_escores <- rowSums(tce)

tce <- cbind(tce,tce_escores)


tce_media <- mean(tce$tce_escores,na.rm = TRUE)
tce_sd <- sd(tce$tce_escores,na.rm = TRUE)

rely_tce <-tce %>% select(1:18) %>% omega(poly=T)

#RV
names(df2)
rv <- df2 %>% select(97:108)

rely_rv <- rv %>% select(1:12) %>% omega(poly=T)

rv_escores<- rowSums(rv)

rv <- cbind(rv,rv_escores)

rv_media <- mean(rv$rv_escores,na.rm = TRUE)
rv_sd <- sd(rv$rv_escores,na.rm = TRUE)

# BFI - PROPRIEDADES PSICOMETRICAS ==========================================================

# Sintaxe no spss... da pra ver os itens negativos
# Extroversão: ICGFP1,ICGFP5,ICGFP_12inv,ICGFP_16inv,ICGFP26,ICGFP29,ICGFP37,ICGFP_42inv
# Abertura: ICGFP9,ICGFP11,ICGFP13,ICGFP25,ICGFP33,ICGFP35,ICGFP39,ICGFP_43inv,ICGFP44
# NeuroticismoICGFP10,ICGFP_14inv,ICGFP_21inv,ICGFP_23inv,ICGFP34,ICGFP36
# Cosncienciosidade: ICGFP4,ICGFP6,ICGFP_17inv,ICGFP_19inv,ICGFP_22inv,ICGFP_38inv
# Amabilidade: ICGFP8,ICGFP15,ICGFP18

bfi <- df2 %>% select(109:152)

rely_abertura <- bfi %>% select(09,11,13,25,33,35,39,43,44) %>% omega(poly=TRUE,key=c(1,1,1,1,1,1,1,-1,1))#0,75/0,82 
rely_conscien <- bfi %>% select(04,06,17,19,22,38) %>% omega(poly=TRUE, key=c(1,1,-1,-1,-1,-1)) #0,72/0,84
rely_extrover <- bfi %>% select(01,05,12,16,26,29,37,42) %>% omega(poly=TRUE,key=c(1,1,-1,-1,1,1,1,-1)) #0,88/0,92
rely_amabilid <- bfi %>% select(08,15,18) %>% omega(poly=TRUE,key=c(1,1,1)) #0,87/0,88
rely_neurotic <- bfi %>% select(10,14,21,23,34,36) %>% omega(poly=TRUE,key=c(1,-1,-1,-1,1,1)) #0,85/ 0,9

extrover_alpha <- rely_extrover$alpha
neurotic_alpha <- rely_neurotic$alpha
conscien_alpha <- rely_conscien$alpha
abertura_alpha <- rely_abertura$alpha
amabilid_alpha <- rely_amabilid$alpha

extrover_omega <- rely_extrover$omega.tot
neurotic_omega <- rely_neurotic$omega.tot
conscien_omega <- rely_conscien$omega.tot
abertura_omega <- rely_abertura$omega.tot
amabilid_omega <- rely_amabilid$omega.tot


# Sintaxe no spss... da pra ver os itens negativos
# Extroversão: ICGFP1,ICGFP5,ICGFP_12inv,ICGFP_16inv,ICGFP26,ICGFP29,ICGFP37,ICGFP_42inv
# Abertura: ICGFP9,ICGFP11,ICGFP13,ICGFP25,ICGFP33,ICGFP35,ICGFP39,ICGFP_43inv,ICGFP44
# Neuroticismo: ICGFP10,ICGFP_14inv,ICGFP_21inv,ICGFP_23inv,ICGFP34,ICGFP36
# Cosncienciosidade: ICGFP4,ICGFP6,ICGFP_17inv,ICGFP_19inv,ICGFP_22inv,ICGFP_38inv
# Amabilidade: ICGFP8,ICGFP15,ICGFP18

bfi$i12inv <- ifelse(bfi$p12 %in% c(1),5,
              ifelse(bfi$p12 %in% c(2),4,
              ifelse(bfi$p12 %in% c(3),3,
              ifelse(bfi$p12 %in% c(4),2,
              ifelse(bfi$p12 %in% c(5),1,NA)))))

bfi$i14inv <- ifelse(bfi$p14 %in% c(1),5,
              ifelse(bfi$p14 %in% c(2),4,
              ifelse(bfi$p14 %in% c(3),3,
              ifelse(bfi$p14 %in% c(4),2,
              ifelse(bfi$p14 %in% c(5),1,NA)))))

bfi$i16inv <- ifelse(bfi$p16 %in% c(1),5,
              ifelse(bfi$p16 %in% c(2),4,
              ifelse(bfi$p16 %in% c(3),3,
              ifelse(bfi$p16 %in% c(4),2,
              ifelse(bfi$p16 %in% c(5),1,NA)))))

bfi$i17inv <- ifelse(bfi$p17 %in% c(1),5,
              ifelse(bfi$p17 %in% c(2),4,
              ifelse(bfi$p17 %in% c(3),3,
              ifelse(bfi$p17 %in% c(4),2,
              ifelse(bfi$p17 %in% c(5),1,NA)))))

bfi$i19inv <- ifelse(bfi$p19 %in% c(1),5,
              ifelse(bfi$p19 %in% c(2),4,
              ifelse(bfi$p19 %in% c(3),3,
              ifelse(bfi$p19 %in% c(4),2,
              ifelse(bfi$p19 %in% c(5),1,NA)))))

bfi$i21inv <- ifelse(bfi$p21 %in% c(1),5,
              ifelse(bfi$p21 %in% c(2),4,
              ifelse(bfi$p21 %in% c(3),3,
              ifelse(bfi$p21 %in% c(4),2,
              ifelse(bfi$p21 %in% c(5),1,NA)))))

bfi$i22inv <- ifelse(bfi$p22 %in% c(1),5,
              ifelse(bfi$p22 %in% c(2),4,
              ifelse(bfi$p22 %in% c(3),3,
              ifelse(bfi$p22 %in% c(4),2,
              ifelse(bfi$p22 %in% c(5),1,NA)))))

bfi$i23inv <- ifelse(bfi$p23 %in% c(1),5,
              ifelse(bfi$p23 %in% c(2),4,
              ifelse(bfi$p23 %in% c(3),3,
              ifelse(bfi$p23 %in% c(4),2,
              ifelse(bfi$p23 %in% c(5),1,NA)))))

bfi$i38inv <- ifelse(bfi$p38 %in% c(1),5,
              ifelse(bfi$p38 %in% c(2),4,
              ifelse(bfi$p38 %in% c(3),3,
              ifelse(bfi$p38 %in% c(4),2,
              ifelse(bfi$p38 %in% c(5),1,NA)))))

bfi$i42inv <- ifelse(bfi$p42 %in% c(1),5,
              ifelse(bfi$p42 %in% c(2),4,
              ifelse(bfi$p42 %in% c(3),3,
              ifelse(bfi$p42 %in% c(4),2,
              ifelse(bfi$p42 %in% c(5),1,NA)))))

bfi$i43inv <- ifelse(bfi$p43 %in% c(1),5,
              ifelse(bfi$p43 %in% c(2),4,
              ifelse(bfi$p43 %in% c(3),3,
              ifelse(bfi$p43 %in% c(4),2,
              ifelse(bfi$p43 %in% c(5),1,NA)))))

bfi$extrover <- bfi %>% select(01,05,"i12inv", "i16inv",26,29,37,"i42inv") %>% rowMeans()
bfi$neurotic <- bfi %>% select(10,"i14inv","i21inv","i23inv",34,36) %>% rowMeans()
bfi$conscien <- bfi %>% select(04,06,"i17inv","i19inv","i22inv","i38inv") %>% rowMeans()
bfi$abertura <- bfi %>% select(09,11,13,25,33,35,39,"i43inv",44) %>% rowMeans()
bfi$amabilid <- bfi %>% select(08,15,18) %>% rowMeans()

names(bfi)

abertura_media <- mean(bfi$abertura,na.rm = TRUE)
conscien_media <- mean(bfi$conscien,na.rm = TRUE)
extrover_media <- mean(bfi$extrover,na.rm = TRUE)
amabilid_media <- mean(bfi$amabilid,na.rm = TRUE)
neurotic_media <- mean(bfi$neurotic,na.rm = TRUE)

abertura_dsvpd <- sd(bfi$abertura,na.rm = TRUE)
conscien_dsvpd <- sd(bfi$conscien,na.rm = TRUE)
extrover_dsvpd <- sd(bfi$extrover,na.rm = TRUE)
amabilid_dsvpd <- sd(bfi$amabilid,na.rm = TRUE)
neurotic_dsvpd <- sd(bfi$neurotic,na.rm = TRUE)


tabela_bfi <- data.frame(indicadores       = c('média', 'desvio padrão','alfa','ômega'),
                         extroversão       = c(extrover_media,extrover_dsvpd,extrover_alpha,extrover_omega),
                         neuroticismo      = c(neurotic_media,neurotic_dsvpd,neurotic_alpha,neurotic_omega),
                         conscienciosidade = c(conscien_media,conscien_dsvpd,conscien_alpha,conscien_omega),
                         abertura          = c(abertura_media,abertura_dsvpd,abertura_alpha,abertura_omega),
                         amabilidade       = c(amabilid_media,amabilid_dsvpd,amabilid_alpha,amabilid_omega)) %>% kable()

tabela_bfi <- data.frame(indicadores     = c("extroversão","neuroticismo","conscienciosidade","abertura","amabilidade"),
                         média           = c(extrover_media,neurotic_media,conscien_media,abertura_media,amabilid_media),
                         "desvio padrão" = c(extrover_dsvpd,neurotic_dsvpd,conscien_dsvpd,abertura_dsvpd,amabilid_dsvpd),
                         alfa            = c(extrover_alpha,
                                             neurotic_alpha,
                                             conscien_alpha,
                                             abertura_alpha,
                                             amabilid_alpha),
                         ômega           = c(extrover_omega,
                                             neurotic_omega,
                                             conscien_omega,
                                             abertura_omega,
                                             amabilid_omega),
                         máximo          = c(which.max(bfi$extrover,
                                             which.max(bfi$neurotic,
                                             which.max(bfi$conscien,
                                             which.max(bfi$abertura,
                                             which.max(bfi$amabilid),
                         mínimo          = c(which.min(bfi$extrover,
                                             which.min(bfi$neurotic,
                                             which.min(bfi$conscien,
                                             which.min(bfi$abertura,
                                             which.min(bfi$amabilid))))))))))))

#==============================================================================
# Calculo dos percentis para aula de estatística com R.

p_extrover <- round(pnorm(seq(from=1,by=0.1,length=41),extrover_media ,extrover_dsvpd )*100,2)
p_neurotic <- round(pnorm(seq(from=1,by=0.1,length=41),neurotic_media ,neurotic_dsvpd )*100,2)
p_conscien <- round(pnorm(seq(from=1,by=0.1,length=41),conscien_media ,conscien_dsvpd )*100,2)
p_abertura <- round(pnorm(seq(from=1,by=0.1,length=41),abertura_media ,abertura_dsvpd )*100,2)
p_amabilid <- round(pnorm(seq(from=1,by=0.1,length=41),amabilid_media ,amabilid_dsvpd )*100,2)

table_bfi_percentil <- data.frame(
  Pontuações = c(seq(from=1,by=0.1,length=41)),
  Extroversão  = p_extrover,
  Neuroticismo = p_neurotic,
  Conscienciosidade = p_conscien,
  Abertura = p_abertura,
  Amabilidade = p_amabilid)

# OBS: A escala de amabilidade no BFI não é boa, dá efeito de topo, a dist não é normal
# Então, no lugar dos percentis teóricos, usei as frequências acumuladas....

p_extrover_freq  <- quantile(df2$extrover , c(.10,.20,.25,.30,.40,.50,.60,.70,.75,.80,.90)) %>% round(digits = 2)
p_neurotic_freq  <- quantile(df2$neurotic , c(.10,.20,.25,.30,.40,.50,.60,.70,.75,.80,.90)) %>% round(digits = 2)
p_conscien_freq  <- quantile(df2$conscien , c(.10,.20,.25,.30,.40,.50,.60,.70,.75,.80,.90)) %>% round(digits = 2)
p_abertura_freq  <- quantile(df2$abertura , c(.10,.20,.25,.30,.40,.50,.60,.70,.75,.80,.90)) %>% round(digits = 2)
p_amabilid_freq  <- quantile(df2$amabilid , c(.10,.20,.25,.30,.40,.50,.60,.70,.75,.80,.90)) %>% round(digits = 2)

table_bfi_freqacum <- data.frame(
#  Pontuações = c(10,20,25,30,40,50,60,70,75,80,90),
  Extroversão  = p_extrover_freq,
  Neuroticismo = p_neurotic_freq,
  Conscienciosidade = p_conscien_freq,
  Abertura = p_abertura_freq,
  Amabilidade = p_amabilid_freq) %>% kable()

# descrição da amostra

data.frame(Média = mean(df2$idade),"Desvio Padrão" = sd(df2$idade)) %>% kable()
count(df2,genero) %>% mutate(porc = n/sum(n)*100) %>% kable()
count(df2,cor) %>% mutate(porc = n/sum(n)*100) %>% kable()
count(df2,turno) %>% mutate(porc = n/sum(n)*100) %>% kable()
# END

# TRE - PROPRIEDADES PSICOMETRICAS ==========================================================
names(df2)
tre <- df2 %>% select(73:96) # Criacao do objeto tre com os itens de regulacao de emocoes
tre <- tre %>% select(-25)

tre$re_d01 <- ifelse(tre$re1 %in% c(1),1,
              ifelse(tre$re1 %in% c(2),0,
              ifelse(tre$re1 %in% c(3),0,
              ifelse(tre$re1 %in% c(4),0,
              ifelse(tre$re1 %in% c(5),0,NA)))))
tre$re_d02 <- ifelse(tre$re2 %in% c(1),1,
              ifelse(tre$re2 %in% c(2),0,
              ifelse(tre$re2 %in% c(3),0,
              ifelse(tre$re2 %in% c(4),0,
              ifelse(tre$re2 %in% c(5),0,NA)))))
tre$re_d03 <- ifelse(tre$re3 %in% c(1),0,
              ifelse(tre$re3 %in% c(2),0,
              ifelse(tre$re3 %in% c(3),0,
              ifelse(tre$re3 %in% c(4),0,
              ifelse(tre$re3 %in% c(5),1,NA)))))
tre$re_d04 <- ifelse(tre$re4 %in% c(1),0,
              ifelse(tre$re4 %in% c(2),0,
              ifelse(tre$re4 %in% c(3),0,
              ifelse(tre$re4 %in% c(4),0,
              ifelse(tre$re4 %in% c(5),1,NA)))))
tre$re_d05 <- ifelse(tre$re5 %in% c(1),1,
              ifelse(tre$re5 %in% c(2),0,
              ifelse(tre$re5 %in% c(3),0,
              ifelse(tre$re5 %in% c(4),0,
              ifelse(tre$re5 %in% c(5),0,NA)))))
tre$re_d06 <- ifelse(tre$re6 %in% c(1),1,
              ifelse(tre$re6 %in% c(2),1,
              ifelse(tre$re6 %in% c(3),1,
              ifelse(tre$re6 %in% c(4),0,
              ifelse(tre$re6 %in% c(5),0,NA)))))
tre$re_d07 <- ifelse(tre$re7 %in% c(1),0,
              ifelse(tre$re7 %in% c(2),0,
              ifelse(tre$re7 %in% c(3),0,
              ifelse(tre$re7 %in% c(4),0,
              ifelse(tre$re7 %in% c(5),1,NA)))))
tre$re_d08 <- ifelse(tre$re8 %in% c(1),1,
              ifelse(tre$re8 %in% c(2),0,
              ifelse(tre$re8 %in% c(3),0,
              ifelse(tre$re8 %in% c(4),0,
              ifelse(tre$re8 %in% c(5),0,NA)))))
tre$re_d09 <- ifelse(tre$re9 %in% c(1),1,
              ifelse(tre$re9 %in% c(2),0,
              ifelse(tre$re9 %in% c(3),0,
              ifelse(tre$re9 %in% c(4),0,
              ifelse(tre$re9 %in% c(5),0,NA)))))
tre$re_d10 <- ifelse(tre$re10 %in% c(1),1,
              ifelse(tre$re10 %in% c(2),0,
              ifelse(tre$re10 %in% c(3),0,
              ifelse(tre$re10 %in% c(4),0,
              ifelse(tre$re10 %in% c(5),0,NA)))))
tre$re_d11 <- ifelse(tre$re11 %in% c(1),1,
              ifelse(tre$re11 %in% c(2),0,
              ifelse(tre$re11 %in% c(3),0,
              ifelse(tre$re11 %in% c(4),0,
              ifelse(tre$re11 %in% c(5),0,NA)))))
tre$re_d12 <- ifelse(tre$re12 %in% c(1),0,
              ifelse(tre$re12 %in% c(2),0,
              ifelse(tre$re12 %in% c(3),0,
              ifelse(tre$re12 %in% c(4),0,
              ifelse(tre$re12 %in% c(5),1,NA)))))
tre$re_d13 <- ifelse(tre$re13 %in% c(1),0,
              ifelse(tre$re13 %in% c(2),0,
              ifelse(tre$re13 %in% c(3),1,
              ifelse(tre$re13 %in% c(4),1,
              ifelse(tre$re13 %in% c(5),1,NA)))))
tre$re_d14 <- ifelse(tre$re14 %in% c(1),0,
              ifelse(tre$re14 %in% c(2),0,
              ifelse(tre$re14 %in% c(3),0,
              ifelse(tre$re14 %in% c(4),0,
              ifelse(tre$re14 %in% c(5),1,NA)))))
tre$re_d15 <- ifelse(tre$re15 %in% c(1),1,
              ifelse(tre$re15 %in% c(2),0,
              ifelse(tre$re15 %in% c(3),0,
              ifelse(tre$re15 %in% c(4),0,
              ifelse(tre$re15 %in% c(5),0,NA)))))
tre$re_d16 <- ifelse(tre$re16 %in% c(1),1,
              ifelse(tre$re16 %in% c(2),0,
              ifelse(tre$re16 %in% c(3),0,
              ifelse(tre$re16 %in% c(4),0,
              ifelse(tre$re16 %in% c(5),0,NA)))))
tre$re_d17 <- ifelse(tre$re17 %in% c(1),0,
              ifelse(tre$re17 %in% c(2),0,
              ifelse(tre$re17 %in% c(3),0,
              ifelse(tre$re17 %in% c(4),0,
              ifelse(tre$re17 %in% c(5),1,NA)))))
tre$re_d18 <- ifelse(tre$re18 %in% c(1),1,
              ifelse(tre$re18 %in% c(2),0,
              ifelse(tre$re18 %in% c(3),0,
              ifelse(tre$re18 %in% c(4),0,
              ifelse(tre$re18 %in% c(5),0,NA)))))
tre$re_d19 <- ifelse(tre$re19 %in% c(1),0,
              ifelse(tre$re19 %in% c(2),0,
              ifelse(tre$re19 %in% c(3),0,
              ifelse(tre$re19 %in% c(4),0,
              ifelse(tre$re19 %in% c(5),1,NA)))))
tre$re_d20 <- ifelse(tre$re20 %in% c(1),0,
              ifelse(tre$re20 %in% c(2),0,
              ifelse(tre$re20 %in% c(3),1,
              ifelse(tre$re20 %in% c(4),0,
              ifelse(tre$re20 %in% c(5),0,NA)))))
tre$re_d21 <- ifelse(tre$re21 %in% c(1),1,
              ifelse(tre$re21 %in% c(2),0,
              ifelse(tre$re21 %in% c(3),0,
              ifelse(tre$re21 %in% c(4),0,
              ifelse(tre$re21 %in% c(5),0,NA)))))
tre$re_d22 <- ifelse(tre$re22 %in% c(1),0,
              ifelse(tre$re22 %in% c(2),0,
              ifelse(tre$re22 %in% c(3),0,
              ifelse(tre$re22 %in% c(4),0,
              ifelse(tre$re22 %in% c(5),1,NA)))))
tre$re_d23 <- ifelse(tre$re23 %in% c(1),1,
              ifelse(tre$re23 %in% c(2),0,
              ifelse(tre$re23 %in% c(3),0,
              ifelse(tre$re23 %in% c(4),0,
              ifelse(tre$re23 %in% c(5),0,NA)))))
tre$re_d24 <- ifelse(tre$re24 %in% c(1),0,
              ifelse(tre$re24 %in% c(2),0,
              ifelse(tre$re24 %in% c(3),0,
              ifelse(tre$re24 %in% c(4),0,
              ifelse(tre$re24 %in% c(5),1,NA)))))

glimpse(tre)
names(tre)
view(tre_dic)
tre_dic <- tre %>% select(25:48)

rely_re_eficaz <- tre_dic %>% select(3,4,7,12,14,17,19,22,23,24) %>% omega(poly=TRUE) 
rely_re_eficaz$alpha
rely_re_eficaz$omega.tot

rely_re_ineficaz <- tre_dic %>% select(1,2,8,9,10,11,15,16,18,21) %>% omega(poly=TRUE) 
rely_re_ineficaz$alpha
rely_re_ineficaz$omega.tot

tre$tre_efi <- tre_dic %>% select(3,4,7,12,14,17,19,22,23,24) %>% rowSums()
tre$tre_ine <- tre_dic %>% select(1,2,8,9,10,11,15,16,18,21) %>% rowSums()

names(df2)

# quetionário socioeconômico =====

qse <- df2 %>% select(153:167)

qse[ ,c(1:15)][qse[ ,c(1:15)] == 1] <- 0
qse[ ,c(1:15)][qse[ ,c(1:15)] == 2] <- 1
qse[ ,c(1:15)][qse[ ,c(1:15)] == 9] <- 1
qse[ ,c(8,14,15)][qse[ ,c(8,14,15)] == 3] <- 1
qse[ ,c(14,15)][qse[ ,c(14,15)] == 4] <- 1

expss::recode(qse[ ,c(1,4,7,8,11,14)]) <- c(9~NA)
names(qse)
rely_qse <- qse %>% select(c(1:5,7:13)) %>% omega(tet=TRUE) 
round(rely_qse$alpha,digits = 3)
round(rely_qse$omega.tot, digits = 3)

qse %>% select(c(1:5,7:13)) %>% scree(pc=FALSE)

efa_qse <- fa(qse[,c(1:5,7:13)],
              nfactors = 1, 
              cor = "cor",
              fm = "wls",
              rotate = "geominQ")

efa_qse <- fa(qse[,c(1,4,10,12,13)],
              nfactors = 1, 
              cor = "cor",
              fm = "wls",
              rotate = "geominQ")

rely_qse <- qse %>% select(c(1,4,10,12,13)) %>% omega(tet=TRUE) 
round(rely_qse$alpha,digits = 3)
round(rely_qse$omega.tot, digits = 3)

glimpse(qse)
qse$nse <- qse %>% select(c(1,4,10,12,13)) %>% rowSums()

summary(qse$nse)

view(qse)
skew(qse$nse)
kurtosi(qse$nse)
sd(df2$nse)
mean(df2$nse)
# compilação de variáveis pra o df2 ======
glimpse(df2)

df2$qaes_projcar <- qaes$qaes_projcar
df2$qaes_adptsoc <- qaes$qaes_adptsoc
df2$qaes_apesemo <- qaes$qaes_apesemo
df2$qaes_adptest <- qaes$qaes_adptest
df2$qaes_adptins <- qaes$qaes_adptins

df2 <- df2 %>% mutate(qaes_totalsc = (qaes_projcar+qaes_adptsoc+(6-qaes_apesemo)+qaes_adptest+qaes_adptins)/5)

df2$tce <- tce$tce_escores

df2$rv <- rv$rv_escores

df2$tre_efi <- tre$tre_efi
df2$tre_ine <- tre$tre_ine

df2$extrover <- bfi$extrover
df2$neurotic <- bfi$neurotic
df2$conscien <- bfi$conscien
df2$abertura <- bfi$abertura
df2$amabilid <- bfi$amabilid

df2$nse <- qse$nse
glimpse(df2)
view(qse$nse)


# CORRELAÇÕES =================================================================

# rely_qaes_projcar - Projeto de Carreira
# rely_qaes_adptsoc - Adaptacao social
# rely_qaes_apesemo - Adaptacao pessoal e emocional
# rely_qaes_adptest - Adaptacao ao estudo
# rely_qaes_adptins - Adaptacao institucional

names(df2)

install.packages("corrplot")
library(corrplot)

matriz <- round(cor(df2[ ,c(10,171:184,186)], method = 'pearson'),4)
matriz$stars %>% kable()

corrplot(matriz, method = "color", type = 'lower', order = 'FPC',
                   addCoef.col = 'black')

names(df2) %>% data.frame()
library(pander)

corr_qaes <- corr.test(df2[c(177:187)],df2[c(171:176)], method = 'pearson')
corr_qaes$stars %>% kable()

corr_medg <- corr.test(df2[c(177:187)],df2$mediag, method = 'pearson')
corr_medg$stars %>% kable()

corr_qaes_medg <- corr.test(df2[c(171:176)],df2$mediag, method = 'pearson')
corr_qaes_medg$stars %>% kable()


# heatmap(correlations$r)
# round(correlations,digits = 2) %>% kable()
round(correlations$r, digits = 3) %>% kable()
round(correlations$p, digits = 3) %>% kable()

?corr.test

corrplot(correlations$r, method = "color", type = 'upper', order = 'hclust',
                   addCoef.col = 'black')
library(pander)
correlations$stars %>% pander()

corr.test(df2[171:176],df2[10], method = 'pearson') %>% pander::pander
library(pander)
corr.test(df2[,171:176], df2[,181:185])

names(df2) %>% as.data.frame()

qaes_projcar_reg <- lm(qaes_projcar ~ 
                         tre_efi+tre_ine + 
                         extrover + 
                         neurotic + 
                         conscien + 
                         amabilid, 
                       data = df2, 
                       na.action = na.omit)

summary(qaes_projcar_reg)

qaes_projcar_reg <- lm(qaes_projcar ~ extrover + neurotic, 
                       data = df2, 
                       na.action = na.omit)
summary(qaes_projcar_reg)

glimpse(df2)

# ==== ANÁLISES DA PROVA DE RV PARA TÉCNICAS PSICOMÉTRICAS

names(df2) %>% as.data.frame()

# Exclusão do item 1 (coluna 97) porque só duas pessoas erraram esse item

# SCREE-PLOT ==================================================================

df2 %>% select(98:108) %>% 
scree(factors = TRUE, pc = FALSE)

## O scree-plot mostrou apenas um fator com eigenvalue superior a 1

# ANÁLISE FATORIAL ============================================================
df2 %>% select(98:108) %>% 
  fa(nfactors = 1, rotate = "varimax", fm = "uls", cor = "poly")

## Eliminação dos itens 5, 8 e 11, por apresentarem problemas.

df2 %>% select(c(98:100,102,103,105,106,108)) %>% 
  fa(nfactors = 1, rotate = "varimax", fm = "uls", cor = "poly")

# FIDEDIGNIDADE ===============================================================

rv_cor_matrix <- df2 %>% select(c(98:100,102,103,105,106,108)) %>% polychoric()

alpha(rv_cor_matrix$rho, na.rm = TRUE, delete = TRUE)

# PADRONIZAÇÃO ================================================================

df2$rv_8itens <- df2 %>% select(c(98:100,102,103,105,106,108)) %>% rowSums()

normas_rv <- round(pnorm(seq(from=1,by=1,length=8),
                         mean(df2$rv_8itens),
                         sd(df2$rv_8itens))*100,2)

round(((df2$rv_8itens - mean(df2$rv_8itens))/sd(df2$rv_8itens))*15 + 100)

data.frame("Escores"   = c(1,2,3,4,5,6,7,8),
           "Percentis" = round(pnorm(seq(from=1,by=1,length=8),
                         mean(df2$rv_8itens),
                         sd(df2$rv_8itens))*100,0),
           RPadronizado= round(((c(1,2,3,4,5,6,7,8) - mean(df2$rv_8itens))/sd(df2$rv_8itens))*15 + 100))

describe(df2$rv_8itens, na.rm = TRUE) %>% pander::pander()

mean(df2$rv_8itens) + sd(df2$rv_8itens)
mean(df2$rv_8itens) - sd(df2$rv_8itens)
