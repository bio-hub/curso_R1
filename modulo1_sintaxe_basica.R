####1  - Objetos####

#espaço na memória dedicada a armazenar um dado
#necessita ter um nome e um valor, mesmo que o valor seja nulo ou vazio

a <- "maçã"
b =  2

####2  - Variáveis####

#character: Essencialmente textos. Deve estar sempre contida dentro de aspas 
#simples ou duplas.

x = "banana"
x = '256'

#integer ou numeric: Números. Podem ser inteiros ou qualquer número real

y = 256L #usa o L para especificar número inteiro, se necessário
y = 4.25

#logical: operadores booleanos de verdadeiro ou falso

z = TRUE
z = FALSE

z = T
z = F

####3  - Estruturas de dados####

#vetor: conjunto de variáveis do mesmo tipo. Os valores devem estar contidos
#dentro da sintaxe c() e separados por vírgula. Cada elemento no vetor pode
#ser acessado através da sua posição entre [].

rhcp_nomes = c("Antony", "Flea", "John", "Chad")

rhcp_idade = c(61, 61, 54, 72)

rhcp_teste = c(TRUE, TRUE, TRUE, FALSE)

rhcp_nomes[3]

#list: Contém um ou mais vetores, podendo ser nomeados. Os items da lista são
#acessados por [] (caso queira manter em lista) ou [[]] (caso queira extrair o
#vetor armazenado no item correspondente), contendo o número correspondente do 
#item ou seu nome, quando houver.

rhcp_info = list(nomes = rhcp_nomes,
                 idade = rhcp_idade,
                 teste = rhcp_teste)

rhcp_info[1] #retorna o item da lista em formato de lista
rhcp_info[[1]] #retorna o item da lista em formato de vetor

rhcp_info[["idade"]][3] #retorna o teceiro elemento do vetor extraído

#matrix ou array: estrutura bidimensional, contendo valores do mesmo tipo 
#(character, numeric ou logic). Caso contenha valores de mais de um tipo, todos
#esses serão automaticamente convertidos em character. Necessita especificar 
#ao menos o número de linhas ou colunas.

rhcp_matrix = matrix(c(rhcp_nomes,
                       rhcp_idade,
                       rhcp_teste), 
                     nrow = 4, 
                     ncol = 3)


#data frame

rhcp_df = data.frame(nomes = rhcp_nomes,
                     idade = rhcp_idade,
                     teste = rhcp_teste)

####4  - coordenadas####

#[] 

rhcp_info[1]
rhcp_info["idade"]

#[[]]

rhcp_info[[2]]
rhcp_info[["idade"]]

#$

rhcp_df$nomes

#buscar elemento especifico dentro de um objeto

rhcp_idade[2] #vetores
rhcp_matrix[2,2] #matrizes
rhcp_df[2,3] #dataframe

rhcp_df[,3] #toda a coluna (dataframe e matriz)
rhcp_df[2,] #toda a linha (dataframe e matriz)

rhcp_info[[2]][3] #para listas

####5  - Operadores####

#atribuição: =, <-, ->, <<-,->>

rhcp_instrumentos =  c("vocal", "baixo", "guitarra", "bateria")
rhcp_instrumentos <- c("vocal", "baixo", "guitarra", "bateria")
c("vocal", "baixo", "guitarra", "bateria") -> rhcp_instrumentos

rhcp_df$idade[4] = 62
rhcp_df[4,3] <- TRUE

#aritméticos: +,-,*,/,^,%% (modulo ou resto da divisão),%/% (divisão inteira)

10 + 5
9 - 6
4/2
2^2
9%%4
9%/%4

sqrt(81)
log2(3)

#relacionais: >,>=,<,<=,==,!=, %in% (match), ! (opposite)

62 > 52
62 < 52
62 >= 52
62 <= 52
62 == 52
62 != 52

"John" %in% rhcp_df$nomes
match("John", rhcp_df$nomes)

"Jake" %in% rhcp_df$nomes
match("Jake", rhcp_df$nomes)

!"Jake" %in% rhcp_df$nomes
!match("Jake", rhcp_df$nomes)

#lógicos: &,|,!,&&,||

54 > 70 & 61 > 70
54 < 70 & 61 < 70
54 > 70 & 61 < 70
54 < 70 & 61 > 70

54 > 70 | 61 > 70
54 < 70 | 61 < 70
54 > 70 | 61 < 70
54 < 70 | 61 > 70

54 > 70 && 61 > 70
54 < 70 && 61 < 70
54 > 70 && 61 < 70
54 < 70 && 61 > 70

54 > 70 || 61 > 70
54 < 70 || 61 < 70
54 > 70 || 61 < 70
54 < 70 || 61 > 70

x = c(TRUE, FALSE, TRUE) 
y = c(FALSE, TRUE, TRUE) 
x &  y 
x && y 
x |  y 
x || y 

####6  - iterações####

#for

#para cada elemento deste vetor, faça isto {}

for (i in 1:4){
  j = i+10
  print(j)
}

#while

#enquanto esse objeto não for assim, faça isto {}

n = 1
while (n < 5) {
  print(n)
  n = n+1
}

####7  - controle de fluxo####

#if simples

#se esse objeto for assim, faça isto {} senão, faça isto {}

x = 0
if (x > 0) {
  print("número positivo")
} else {
  print("zero ou número negativo")
}

#if aninhado

#se esse objeto for assim, faça isto {} senão, faça isto {}, senão, faça isto {}...

x = -4
if (x > 0) {
  print("número positivo")
} else if (x == 0) {
  print("número é zero")
} else {
  print("número negativo")
}

#ifelse

#variação do "if" simples

y = "californication"
ifelse(is.character(y), "é texto", "é número")

y = 20
ifelse(is.character(y), "é texto", "é número")

####8  - funções####

#função para avaliar se o nome é de um integrante do RHCP

#name: objeto/variável do tipo character

is_rhcp = function(name) {
  if(name %in% rhcp_df$nomes) {
    return(print("é do red hot"))
  } else {
    return(print("não é do red hot"))
  }
}

is_rhcp("Antony")
is_rhcp("Dave Grohl")

is_rhcp("John")
is_rhcp("Freddy Mercury")


####9  - funções de ajuda####

?mean
help(sd)

####10 - funções de avaliação de variáveis####

#is.*
is.character("californication")
is.numeric(2)
is.logical(FALSE)

is.character(2)
is.numeric("Flea")
is.logical(52)


#class
class(2)
class(rhcp_df)

####funções de conversão de variáveis###

#as.*
as.character()
as.numeric()
as.logical()


as.character(156)
as.numeric("561")
as.numeric("Antony")
as.logical("TRUE")
as.logical(2)


####11 - funções de estrutura de objetos####

#str
str(rhcp_instrumentos)
str(rhcp_info)
str(rhcp_df)

####12 - funções de gestão de environment####

#ls, rm, getwd, setwd, list.files
ls()
ls(pattern = "rhcp")

#rm
rm(a)
rm(i,j,n)
rm(list = ls(pattern = "rhcp"))

#getwd
getwd

#setwd
setwd("c:/Users/ronal/Downloads/")

#list.files
list.files()
list.files(pattern = "spotify")

####13 - funções de instalar pacotes####

#instaladores de fontes externas ao CRAN
install.packages(c("devtools", "remotes", "BiocManager"))

#packages essenciais
install.packages(c("tidyverse",
                   "janitor",
                   "rstatix",
                   "gt",
                   "ggpubr"))

#graficos avancados
install.packages(c("ggspatial",
                   "ggalluvial",
                   "pheatmap"))

#packages para web scraping
install.packages(c("rvest",
                   "httr2",
                   "netstat",
                   "RSelenium"))

#packages para machine learning
install.packages(c("tidymodels",
                   "randomForest",
                   "ranger",
                   "kknn",
                   "xgboost"))

#packages para trabalhar com datasus
devtools::install_github("danicat/read.dbc")
remotes::install_github("rfsaldanha/microdatasus")
install.packages(c("geobr",
                   "sidrar"))

#packages do Bioconductor
BiocManager::install(c("biomaRt",
                       "DESeq2",
                       "DEP",
                       "org.Hs.eg.db",
                       "ReactomePA"))


####15 - Carregar um pacote para o environment####
library(tidyverse)
library(rstatix)

####FIM####
