install.packages("skimr")

library(tidyverse)
library(data.table)
library(skimr)

#carregar os dados
#https://www.kaggle.com/datasets/nasa/meteorite-landings
meteoritos = fread("datasets/meteorite_landings/meteorite-landings.csv")

#Visao das primeiras linhas
meteoritos

#visao geral dos dados (nome das colunas, tipo de variáveis, etc)
glimpse(meteoritos)

#descrição sumariza dos dados
skim(meteoritos)


