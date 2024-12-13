library(tidyverse)
library(janitor)
library(data.table)

library(rstatix)

#carregar os dados
#https://www.kaggle.com/datasets/nasa/meteorite-landings
meteoritos = fread("datasets/meteorite_landings/meteorite-landings.csv")

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

#### agregação dos dados ####

#contagens de meteoritos por tipo 
meteoritos_clean |> 
  group_by(nametype) |> 
  count()

#contagens de meteoritos por queda/encontrado
meteoritos_clean |> 
  group_by(fall) |> 
  count()

#agrupar por duas variáveis
meteoritos_clean |> 
  group_by(nametype, fall) |> 
  count()

#contagens de meteoritos por ano
meteoritos_clean |> 
  group_by(year) |> 
  count() |>
  arrange(desc(n)) |> 
  View()

#mínimos e máximas massa por classe de meteorito
meteoritos_clean |> 
  group_by(recclass) |> 
  summarise(minimo = min(mass_log2),
            maximo = max(mass_log2)) |> 
  View()

#top5 metoritos massivos
meteoritos_clean |> 
  select(year, name, mass) |> 
  slice_max(order_by = mass, 
            n = 5)

#tabela cruzada entre tipo de meteorito e se caiu ou foi encontrado
#função xtabs não funciona com o pipe nativo do R

meteoritos_clean %>% 
  xtabs(~ nametype + fall, data = .)

#medidas de dispersão e centralidade
meteoritos_clean |> 
  group_by(nametype, fall) |> 
  select(mass, mass_log2) |> 
  get_summary_stats() |> 
  View()




