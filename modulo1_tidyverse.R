#install.packages("tidyverse")
library(tidyverse)

####pipe####

# "|>": pipe nativo do R
# "%>%": pipe do pacote magrittr

####view dataset####
View(starwars)
glimpse(starwars)

####select####

#selecionar uma coluna
starwars |> 
  select(name)

starwars %>%
  select(name)

#selecionar pelo numero da coluna
starwars |> 
  select(1)

#selecionar um conjunto de colunas
starwars |>
  select(height, mass, name, hair_color, skin_color)

#selecionar um intervalo de colunas
starwars |>
  select(name:skin_color)

#selecionar o oposto de um intervalo
starwars |>
  select(!c(name:skin_color))

#selecionar com base em um padrao no nome das colunas
starwars |> 
  select(starts_with("eye"))

starwars |> 
  select(ends_with("color"))

starwars |> 
  select(contains("_"))

#selecionar com base no tipo de variável
starwars |> 
  select(where(is.numeric))

starwars |> 
  select(where(is.character))

starwars |> 
  select(where(is.list))

####mutate####

#criar nova variável com base em uma variável já existente
starwars |>
  mutate(altura_categ = if_else(height > 200, "alto", "baixo")) |>
  View()

#criar nova variável do zero
starwars |>
  mutate(index = seq(1:nrow(starwars))) |>
  View()

#alterar o tipo de variável
starwars |>
  mutate(skin_color = as.factor(skin_color)) |>
  glimpse()

#alterar multiplas colunas contiguas com uma única função
starwars |>
  mutate(across(hair_color:eye_color, 
                as.factor)) 

#alterar multiplas colunas espalhadas pelo dataset
starwars |>
  mutate_at(.vars = vars(sex, gender),
            .funs = ~ toupper(.x)) |> 
  glimpse()

#criar multiplas colunas, com base em colunas existentes, 
#espalhadas pelo dataset 
starwars |>
  mutate_at(.vars = vars(sex, gender),
            .funs = list(~ toupper(.x),
                         ~ as.factor(.x))) |> 
  glimpse()

####filter####

#utilizando operadores lógicos
starwars |> 
  filter(height > 150) |> 
  View()

starwars |> 
  #filter(height >= 180 & height <= 200) |> 
  filter(between(height, 180, 200)) |>   #utilizando a função between
  View()

starwars |> 
  filter(hair_color == "blonde" | hair_color == "blond") |> 
  View()

starwars |> 
  filter(!species == "Human") |> 
  View()

#utilizando filtro por matching
starwars |> 
  filter(eye_color %in% c("red", "blue")) |> 
  View()

####summarise####

#uma função
starwars |> 
  summarise(avg = mean(height, na.rm = TRUE))

#multiplas funções
starwars |> 
  summarise(avg = mean(height, na.rm = TRUE), 
            sd  = sd(height, na.rm = TRUE))

#nmesma função em variáveis de um tipo
starwars %>%
  summarise(across(where(is.numeric), 
                   ~ mean(.x, na.rm = TRUE))) 

#variaveis contíguas
starwars %>%
  summarise(across(height:mass, 
                   ~ mean(.x, na.rm = TRUE))) 

#sumarizar múltiplas variáveis dispersas
starwars |>
  summarise_at(.vars = vars(height, birth_year), 
               .funs = ~ mean(.x, na.rm = TRUE))

#sumarizar múltiplas funções
starwars |>
  summarise_at(.vars = vars(height, birth_year), 
               .funs = list(avg = ~ mean(.x, na.rm = TRUE),
                            sd  = ~ sd(.x, na.rm = TRUE)))

#dados agrupados por uma categoria
starwars |> 
  group_by(sex) |> 
  summarise(contagem = n())

#dados agrupados por múltiplas categoria(s)
starwars |> 
  group_by(hair_color, eye_color) |> 
  summarise(contagem = n())

#dados agrupados por múltiplas categoria(s) e múltiplas funções
starwars |> 
  group_by(hair_color, eye_color) |> 
  summarise_at(.vars = vars(height, birth_year), 
               .funs = list(avg = ~ mean(.x, na.rm = TRUE),
                            sd  = ~ sd(.x, na.rm = TRUE)))

####arrange####

#ordem crescente
starwars |> 
  arrange(mass) #NA fica por último

#ordem decrescente
starwars |> 
  arrange(desc(height))

####pivot####

#expandir para o lado
x = starwars |> 
  group_by(sex) |> 
  summarise(count = n()) |> 
  pivot_wider(id_cols = everything(),
              names_from = sex,
              values_from = count)

x

#expandir para baixo
x |> 
  pivot_longer(cols = everything(),
               names_to = "sex",
               values_to = "count")

rm(x)

####nest/unnest####

#nest
x = starwars |> 
  select(sex, contains("color")) |> 
  group_by(sex) |> 
  nest() 

x

#unnest
y = x |> 
  filter(sex == "female") |> 
  unnest(data)

y

#unnest longer
starwars |> 
  unnest_longer(films) 

#unnest wider
starwars |> 
  unnest_wider(starships, 
               names_sep = "_") |> 
  glimpse()

####joins####

#https://rpubs.com/jcross/joins

x = starwars |> 
  select(name, height, mass)

y = starwars |> 
  select(name, contains("color")) |> 
  filter(grepl("Skywalker", name))

x
y

z_left = left_join(x, y, join_by("name" == "name"))
z_left

z_right = right_join(x, y, join_by("name" == "name"))
z_right

z_full = full_join(x, y, join_by("name" == "name"))
z_full

z_inner = inner_join(x, y, join_by("name" == "name"))
z_inner

z_semi = semi_join(x, y, join_by("name" == "name"))
z_semi

z_anti = anti_join(x, y, join_by("name" == "name"))
z_anti

