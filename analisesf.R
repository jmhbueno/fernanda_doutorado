df1<- read_xlsx("Habilidades Socioemocioniais e Adaptação Acadêmica em Universitários  (respostas).xlsx")
names(df1)
view(df1)

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

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ESTATISTICAS DESCRITIVAS ==== 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

df2 %>%                                     # pega o objeto (dataframe) df2
  summarise(                                # e aplica a funcao summarise, que monta uma tabela com os dados solicitados
  média_idade = mean(df2$idade, na.rm = T), # que, nesse caso, são: 1) a media da idade, removendo os dados ausentes (NA)
  desvio_padrão = sd(df2$idade, na.rm = T)) # e o desvio padrao da idade, removendo os dados ausentes (NA)


names(df1[ ,1:10])
names(df2)
str(df2[ ,1:14]) 
view(df2)


df2$mediag <- as.numeric(df2$mediag)
df2$mediag <- df2$mediag %>% as.numeric()

amostra_nota_media <- df2 %>% 
  summarise(
    média_idade = mean(df2$mediag, na.rm = T),
    desvio_padrão = sd(df2$mediag, na.rm = T))


df2 %>% count(genero)
df2 %>% count(cor)

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

rely_qaes_projcar<-qaes %>% select(5,10,15,20,25,30,35,40) %>% omega(poly=TRUE)
rely_qaes_adptsoc<-qaes %>% select(2,7,12,17,22,27,32,37) %>% omega(poly=TRUE)
rely_qaes_apesemo<-qaes %>% select(4,9,14,19,39,29,34,24) %>% omega(poly=TRUE)
rely_qaes_adptest<-qaes %>% select(3,38,13,18,23,28,33,8) %>% omega(poly=TRUE)
rely_qaes_adptins<-qaes %>% select(1,6,11,16,21,26,36,31) %>% omega(poly=TRUE)

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

qaes$qaes_projcar <- qaes %>% select(5,10,15,20,25,30,35,40) %>% rowMeans()
qaes$qaes_adptsoc <- qaes %>% select(2, 7,12,17,22,27,32,37) %>% rowMeans()
qaes$qaes_apesemo <- qaes %>% select(4, 9,14,19,39,29,34,24) %>% rowMeans()
qaes$qaes_adptest <- qaes %>% select(3,38,13,18,23,28,33, 8) %>% rowMeans()
qaes$qaes_adptins <- qaes %>% select(1, 6,11,16,21,26,36,31) %>% rowMeans()

names(qaes)
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
