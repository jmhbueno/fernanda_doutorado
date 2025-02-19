library(tidyverse)

# Caminhos até o arquivo --------------------------------------------------

# Diretório de trabalho
getwd()

# Caminhos absolutos
"/home/william/Documents/Curso-R/intro-programacao-em-r-mestre/dados/imdb.csv"

# Caminhos relativos
"dados/imdb.csv"

# (caro professor, favor lembrar de falar da dica de navegação entre as aspas)

# Tibbles -----------------------------------------------------------------

library(tibble)
airquality
class(as_tibble(airquality))
as_tibble(airquality)

# Lendo arquivos de texto -------------------------------------------------

# CSV, separado por vírgula
imdb_csv <- read_csv(file = "dados/imdb.csv")

# CSV, separado por ponto-e-vírgula
imdb_csv2 <- read_csv2(file = "dados/imdb2.csv")

# TXT, separado por tabulação (tecla TAB)
imdb_txt <- read_delim(file = "dados/imdb.txt", delim = "\t")

# A função read_delim funciona para qualquer tipo de separador
imdb_delim <- read_delim("dados/imdb.csv", delim = ",")
imdb_delim <- read_delim("dados/imdb2.csv", delim = ";")

# encoding
# Natação UTF8
# NataÃo!Â latin1
tb_candidatura_csv <- read_csv2(
  file = "dados/imdb2.csv", 
  locale = locale(encoding = "UTF-8")
)

# direto da internet
imdb_csv <- read_csv("https://raw.githubusercontent.com/curso-r/202005-r4ds-1/master/dados/imdb.csv")

# Interface point and click do RStudio também é útil!

# Lendo arquivos do Excel -------------------------------------------------

library(readxl)

imdb_excel <- read_excel("dados/imdb.xlsx")
excel_sheets("dados/imdb.xlsx")



# Parâmetros úteis --------------------------------------------------------

# col_types: para definir a classe das colunas
# skip: para pular linhas
# na: indica quais strings devem ser interpretadas como NA

# Primeira tentativa
readxl::read_excel(
  "dados/mtcars_desconfigurado.xlsx"
)

# Lendo aba certa
readxl::read_excel(
  "dados/mtcars_desconfigurado.xlsx",
  sheet = 2
)

# Pulando linhas com sujeira
readxl::read_excel(
  "dados/mtcars_desconfigurado.xlsx",
  sheet = 2,
  skip = 2
)

# Especificando NAs
readxl::read_excel(
  "dados/mtcars_desconfigurado.xlsx",
  sheet = 2,
  skip = 2,
  na = c("", "NT")
)

# Arrumando classe da coluna mpg
readxl::read_excel(
  "dados/mtcars_desconfigurado.xlsx",
  sheet = 2,
  skip = 2,
  na = c("", "NT"),
  col_types = c(
    "numeric", 
    "guess", 
    "guess", 
    "guess", 
    "guess", 
    "guess",
    "guess", 
    "guess", 
    "guess", 
    "guess", 
    "guess"
  )
)

# Arrumando classe da coluna mpg depois de carregar
mtcars_cru <- readxl::read_excel(
  "dados/mtcars_desconfigurado.xlsx",
  sheet = 2,
  skip = 2,
  na = c("", "NT")
)

mtcars$mpg <- as.numeric(mtcars$mpg)

# as.numeric()
# as.character()
# as.Date()

# Outros formatos ---------------------------------------------------------

library(jsonlite)
imdb_json <- read_json("dados/imdb.json")

library(haven)
imdb_sas <- read_sas("dados/imdb.sas7bdat")
imdb_spss <- read_spss("dados/imdb.sav")

# Gravando dados ----------------------------------------------------------

# As funções iniciam com 'write'

# CSV
write_csv(imdb, path = "imdb.csv")

# Excel
library(writexl)
write_xlsx(imdb, path = "imdb.xlsx")

# O formato rds -----------------------------------------------------------

# .rds são arquivos binários do R
# Você pode salvar qualquer objeto do R em formato .rds

imdb_rds <- readr::read_rds("dados/imdb.rds")
readr::write_rds(imdb_rds, path = "dados/imdb_rds.rds")

# Conexão com banco de dados e SQL ----------------------------------------

# install.packages("RSQLite")
library(RSQLite)
library(dplyr)

# Fazendo conexão com banco de dados
conexao <- dbConnect(SQLite(), "dados/imdb.sqlite")

dbListTables(conexao)

# Criando uma tabela a partir do banco de dados
imdb_sqlite <- dplyr::tbl(conexao, "imdb")

# Criando uma tabela usando instruções em SQL
instrucao <- dplyr::sql("SELECT titulo, ano, diretor FROM imdb")
imdb_select <- dplyr::tbl(conexao, instrucao)

# Trazer para a memória
dplyr::collect(imdb_sqlite)
dplyr::collect(imdb_select)

# Escrevendo
dbWriteTable(conexao, "mtcars", mtcars)
dbListTables(conexao)

# Mais informações: db.rstudio.com
