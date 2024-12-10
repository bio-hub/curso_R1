library(tidyverse)
library(janitor)
library(data.table)
library(skimr)

#carregar os dados
#https://www.kaggle.com/datasets/nasa/meteorite-landings
meteoritos = fread("datasets/meteorite_landings/meteorite-landings.csv")

#visão geral dos dados 
glimpse(meteoritos)
skim(meteoritos)

#remover todas linhas com algum dado faltante
meteoritos |> 
  drop_na() |> 
  skim()

#remover linhas com dados faltantes, baseado em uma coluna
meteoritos |> 
  drop_na(year, mass) |> 
  skim()

#remover linhas duplicadas
meteoritos |> 
  distinct(id) |> 
  glimpse()

#converter tipos de variáveis

meteoritos_clean = meteoritos |> 
  clean_names() |> 
  mutate(recclass = na_if(recclass, "Unknown")) |>
  mutate_at(.vars = vars(nametype, fall),
            .funs = ~ tolower(.x)) |> 
  mutate_at(.vars = vars(nametype, fall),
            .funs = ~ as.factor(.x)) |> 
  mutate(nametype = fct_relevel(nametype, c("valid", "relict"))) |> 
  mutate(fall = fct_relevel(fall, c("fell", "found"))) |> 
  filter(mass >= 1) |>
  mutate(mass_log2 = log2(mass)) |>
  relocate(mass_log2, .after = mass) |> 
  select(-geo_location) |> 
  drop_na()
  
  
