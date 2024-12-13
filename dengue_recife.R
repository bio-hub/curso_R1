#### carregamento das bibliotecas ####

library(tidyverse)
library(skimr)
library(janitor)
library(rstatix)
library(ggpubr)

#### carregamento dos datasets ####

dengue_recife = read_delim("datasets/dengue_recife/dengue_recife.csv")
cnes_recife = read_delim("datasets/dengue_recife/cnes_recife.csv")

#### visão geral dos dados ####

glimpse(dengue_recife)
skim(dengue_recife)

glimpse(cnes_recife)
skim(cnes_recife)

#### limpeza e processamento ####

#separar as colunas relevantes estabelecimentos de saúde
cnes_recife_Clean = cnes_recife |>
  select(CNES, TPGESTAO, NAT_JUR, FANTASIA) |> 
  mutate(CNES = as.numeric(CNES)) |> 
  mutate(TPGESTAO = if_else(TPGESTAO == "Dupla", 
                            "dupla", 
                            "pública")) |> 
  distinct(CNES, .keep_all = TRUE) |> 
  clean_names() 
  

dengue_recife_clean = dengue_recife |> 
  select(DT_NOTIFIC, SEM_NOT, ID_UNIDADE, 
         DT_SIN_PRI:CS_RACA, SG_UF,
         FEBRE:AUTO_IMUNE, SOROTIPO, 
         HOSPITALIZ, DT_INTERNA,
         CLASSI_FIN, EVOLUCAO, DT_OBITO) |> 
  mutate(SEM_NOT = str_sub(SEM_NOT, -2, -1)) |>
  mutate(SEM_NOT = as.numeric(SEM_NOT)) |>
  mutate(SEM_PRI = str_sub(SEM_PRI, -2, -1)) |>
  mutate(SEM_PRI = as.numeric(SEM_PRI)) |>
  mutate(NU_IDADE_N_CODIGO = str_sub(NU_IDADE_N, 1, 1)) |> 
  mutate(NU_IDADE_N_CODIGO = case_when(NU_IDADE_N_CODIGO == 2 ~ "dias",
                                       NU_IDADE_N_CODIGO == 3 ~ "meses",
                                       NU_IDADE_N_CODIGO == 4 ~ "anos",
                                       TRUE ~ NA)) |>
  relocate(NU_IDADE_N_CODIGO, .before = NU_IDADE_N) |>
  mutate(NU_IDADE_N = str_sub(NU_IDADE_N, 2)) |>
  mutate(NU_IDADE_N = as.numeric(NU_IDADE_N)) |>
  mutate(CS_SEXO = case_when(CS_SEXO == "F" ~ "feminino",
                             CS_SEXO == "M" ~ "masculino",
                             TRUE ~ NA)) |> 
  mutate(CS_GESTANT = case_when(CS_GESTANT == 1 ~ "trimestre 1",
                                CS_GESTANT == 2 ~ "trimestre 2",
                                CS_GESTANT == 3 ~ "trimestre 3",
                                CS_GESTANT == 4 ~ NA,
                                CS_GESTANT == 5 ~ "não",
                                CS_GESTANT == 6 ~ NA,
                                CS_GESTANT == 9 ~ NA)) |>
  mutate(CS_RACA = case_when(CS_RACA == 1 ~ "branca",
                             CS_RACA == 2 ~ "preta",
                             CS_RACA == 3 ~ "amarela",
                             CS_RACA == 4 ~ "parda",
                             CS_RACA == 5 ~ "indígena",
                             CS_RACA == 9 ~ NA)) |>
  mutate(SG_UF = case_when(SG_UF == 26 ~ "não", 
                           TRUE ~ "sim")) |> 
  mutate(across(FEBRE:HOSPITALIZ, ~ case_when(.x == 1 ~ "sim",
                                              .x == 2 ~ "não",
                                              TRUE ~ NA))) |>
  mutate(SOROTIPO = case_when(SOROTIPO == 1 ~ "DENV1",
                              SOROTIPO == 2 ~ "DENV2",
                              SOROTIPO == 3 ~ "DENV3",
                              SOROTIPO == 4 ~ "DENV4",
                              TRUE ~ NA)) |> 
  mutate(CLASSI_FIN = case_when(CLASSI_FIN == "1"  ~ "dengue clássico",
                                CLASSI_FIN == "2"  ~ "dengue com complicações", 
                                CLASSI_FIN == "3"  ~ "febre hemorrágica do dengue", 
                                CLASSI_FIN == "4"  ~ "síndrome do choque do dengue", 
                                CLASSI_FIN == "5"  ~ "descartado", 
                                CLASSI_FIN == "8"  ~ "inconclusivo", 
                                CLASSI_FIN == "10" ~ "dengue", 
                                CLASSI_FIN == "11" ~ "dengue com sinais de alarme", 
                                CLASSI_FIN == "12" ~ "dengue grave", 
                                CLASSI_FIN == "13" ~ "chikungunya")) |> 
  mutate(EVOLUCAO = case_when(EVOLUCAO == "1" ~ "cura", 
                              EVOLUCAO == "2" ~ "óbito por dengue", 
                              EVOLUCAO == "3" ~ "óbito por outras causas", 
                              EVOLUCAO == "4" ~ "óbito em investigação", 
                              EVOLUCAO == "9" ~ "ignorado")) |> 
  clean_names()
  
  
  
  
############## Análise dos dados ##############

#### quantos casos confirmados de dengue foram notificados no ano? ####

#### qual a semana com mais notificações (gráfico)? ####

#### quantos casos confirmados por faixa etária? (< 18, 18-64, 65+)?

#### quantos casos confirmados por faixa etária? (< 18, 18-64, 65+)? ####
#### qual a distribuição dos casos confirmados por sintomatologia (gráfico)? ####

#### qual o sorotipo mais frequente (gráfico)?

#### existe diferenças na distribuição dos sintomas por sexo (estatística)?#### 
#### quantas pessoas foram hospitalizadas? ####

#### quais os cinco estabelecimentos de saúde com mais notificações? ####

#### qual a taxa de internação por dengue neste ano? ####

#### existe uma faixa etária com mais chances de falecer por dengue? ####

#### quantos óbitos por dengue neste ano? ####
