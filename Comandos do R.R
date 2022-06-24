# Manipulação de dados

search()

# ALT - : produz a seta <- 
# CTRL ENTER : roda a análise que está no script

# Objetos do R
# vetores (nos nossos bancos de dados, cada variável é um vetor)
# quando organizamos diversos vetores numa sequencia de colunas , criamos um dataframe
# dataframe é o objeto com que trabalhamos no R.

sexo <- c(1,2,1,1,2) # criando um vetor chamdo "sexo", que contem cinco observações
# OBS: a letra c concatena os valores dentro dos parênteses em um vetor.

sexo_cod <- c("m","f","m","m","f") # criar outro vetor semelhante, mas com letras no lugar dos números

sexo_cod_logica <- ifelse(sexo == 1,"m","f")  # funcao ifelse... no objeto sexo, se o valor for 1,
                                              # coloque a letra m, se não, coloque a letra f

ifelse(sexo_cod == "m", TRUE, FALSE) # palavras TRUE e FALSE sao palavras logicas

idade <- c(25, 32, 78, 12, NA)  # a palavra NA tambem e logica, indica ausencia de informacao.

sexo_idade <- data.frame(sexo_cod,idade)  # concatenacao dos vetores em um dataframe

count(sexo_idade,sexo_cod) # frequencias dos valores da variável sexo_cod no dataframe sexo_idade

mean(sexo_idade$idade,na.rm = TRUE)  # mean - funcao para calcular a media
                                     # o sinal de $ concatena um dataframe com uma variavel
                                     # o comando na.rm = TRUE diz que é para ignorar valores NA

media_idade <- mean(sexo_idade$idade, na.rm = TRUE)
sd(sexo_idade$idade,na.rm = TRUE)
sd(idade,na.rm=TRUE)
desvio_padrao_idade <- sd(idade,na.rm=TRUE)

rm(desvio_padrao_idade)
rm(media_idade)

names(sexo_idade)
view(sexo_idade)

# acrescentando uma variavel
id <- 1:5
sexo_idade <- cbind(id,             # vetor id
                    sexo_idade)     # juntando com o dataframe sexo_idade

escolaridade <- c("superior","medio","fundamental","fundamental","medio")

sexo_idade <- cbind(sexo_idade,escolaridade)

suj <- data.frame(id=6,sexo_cod="m",idade=34,escolaridade="superior")

sexo_idade <- rbind(sexo_idade,suj)

renda <- seq(1000,by=500,length=6)

sexo_idade <- cbind(sexo_idade,renda)

bonus <- rep(500,6)

sexo_idade <- cbind(sexo_idade,bonus)

renda_total <- sexo_idade$renda + sexo_idade$bonus

sexo_idade <- cbind(sexo_idade,renda_total)

df <- sexo_idade

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TIDYVERSE ====
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# filter    : FILTRAR LINHAS - filter(df, ...)
# slice     : SELECIONAR LINHAS - slice(df, ...)
# select    : SELECIONAR VARIAVEIS - select(df, ...)
# mutate    : MODIFICAR/CRIAR VARIAVEIS - mutate(df, ...)
# group_by  : AGRUPAR OBSERVACOES - group_by(df, ...)
# arrange   : REORGANIZA O DATAFRAME - arrange(df,...) 
# summarise : SUMARIZAR OBSERVACOES - summarise(df, ...)

df_sexo_masculino <- filter(df,sexo_cod == "m")
filter(df, idade<35)
filter(df,sexo_cod == "m", idade<35)

slice(df, 1:3)
slice(df, 1,4,6)
slice(df, 1,4:6)
slice(df, -c(2,5))

select(df,sexo_cod, idade)
select(df,2,3)
names(df)
df <- select(df,-7)

df <- mutate(df,renda_total = renda + bonus)

arrange(df,sexo_cod)

# Comando PIPE %>% (SHIFT+CTRL+M)====
# Ordena os comandos de forma diferente

df_masculino <- df %>% filter(sexo_cod == "m") %>% mutate(renda_total = renda + bonus)



########################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#***********************************************************************  
# comandos do R ====

df1 <- read_xlsx("Habilidades Socioemocioniais e Adapta??o Acad?mica em Universit?rios  (respostas).xlsx")

names(df1)         #funcao para ver os nomes das variaveis de df1
view(df1)          # funcao para visualizar df1 no formato planilha - abre uma aba nova.

names(df1[ ,1:10]) # funcao para ver os nomes das variaveis das colunas de 1 a 10

# for: realiza uma operacao repetidamente dentro de um escopo ====

for(i in 1:226){             # funcao for - realiza opera??o de forma sistem?tica
  Id <- paste0("s", i)       # comando para criar objeto colando a letra s mais os numeros dos sujeitos (i)
}

df1 <- cbind(df1,Id)

df1 <- df1[ ,-c(1,3)] # retirar colunas TCLE e hora/data
df1 <- df1[ ,-c(1)]   # retirar coluna 1 - pontua??o

#===================================================================================================
# renomear colunas
#===================================================================================================

df1 <- read_xlsx("Habilidades Socioemocioniais e Adaptação Acadêmica em Universitários  (respostas).xlsx")
names(df1)
df1 <- rename(df1,Id="Carimbo de data/hora")
df1 <- df1 %>% select(-c(2,3))
df1 <- df1 %>% rename(email="Email (Não obrigatório, mas recomendável. Após o período de coleta e análise dos dados, seus resultados serão enviados para este endereço. Não será divulgado a terceiros, nem usado para fins que não o desta pesquisa).")
df1 <- df1 %>% rename(email="Email (Não obrigatório, mas recomendável. Após o período de coleta e análise dos dados, seus resultados serão enviados para este endereço. Não será divulgado a terceiros, nem usado para fins que não o desta pesquisa).")
df1 <- df1 %>% rename(idade = 3)

rename_all()

df1_renomeado <- df1 %>% rename_all(list(~c(
  'Id',
  'email',
  'idade',
  'genero',
  'y5',
  'y6','y7','y8','y9',
  'y10','y11','y12','y13',
  'y14','y15','y16','y17',
  'y18','y19','y20','y21',
  'y22','y23','y24','y25',
  'y26','y27','y28','y29',
  'y30','y31','y32','y33',
  'y34','y35','y36','y37',
  'y38','y39','y40','y41',
  'y42','y43','y44','y45','y46',
  'y47','y48','y49','y50','y51',
  'y52','y53','y54','y55','y56',
  'y57','y58','y59','y60','y61','y62',
  'y63','y64','y65','y66','y67','y68',
  'y69','y70','y71','y72','y73','y74',
  'y75','y76','y77','y78','y79','y80',
  'y81','y82','y83','y84','y85','y86',
  'y87','y88','y89','y90','y91','y92',
  'y93','y94','y95','y96','y97','y98','y99',
  'y100','y101','y102','y103','y104','y105')))