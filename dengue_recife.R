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
  
#limpeza e processamento dos dados de dengue
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

#total de casos confirmados: 4164
dengue_recife_clean |> 
  group_by(classi_fin) |>
  summarise(numero_casos = n())



#### qual a semana com mais notificações (gráfico)? ####

#semana com mais notificoes: semana 15
dengue_recife_clean |> 
  group_by(sem_not) |> 
  summarise(numero_casos = n()) |> 
  View()

#criando o grafico
p1 = dengue_recife_clean |> 
  group_by(sem_not) |> 
  summarise(numero_casos = n()) |> 
  ggplot(aes(x = sem_not,
             y = numero_casos,
             group = 1)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(breaks = seq(0, 350, by = 50),
                     limits = c(0, 350)) +
  labs(title = "Evolucao das notificacoes de dengue no Recife",
       subtitle = "dados de 2024",
       x = "semana de notificacao",
       y = "numero de casos",
       caption = "FONTE: SINAN-DataSUS")

#exportando o grafico
ggsave(p1,
       filename = "./fig1.jpg", 
       width = 1920*5, 
       height = 1080*5, 
       dpi = 600, 
       units = "px")



#### quantos casos confirmados por faixa etária? (< 18, 18-64, 65+)?

#### qual a distribuição dos casos confirmados por sintomatologia (gráfico)? ####

sintomatologia = dengue_recife_clean |> 
  filter(str_detect(classi_fin, "dengue") == TRUE) |> 
  select(febre:auto_imune) |> 
  pivot_longer(cols = everything(),
               names_to = "sintomas",
               values_to = "presenca") |> 
  group_by(sintomas, presenca) |> 
  summarise(numero_casos = n())

p2 = sintomatologia |> 
  mutate(presenca = fct_relevel(presenca, c("não", "sim"))) |> 
  ggplot(aes(x = numero_casos,
             y = sintomas,
             fill = presenca)) +
  geom_bar(stat = "identity") +
  theme_minimal(base_size = 18,
                base_family = "sans") +
  theme(panel.grid = element_blank()) +
  scale_fill_manual(values = c("cornflowerblue",
                               "tomato"))


#exportando o grafico
ggsave(p2,
       filename = "./fig2.jpg", 
       width = 1920*5, 
       height = 1080*5, 
       dpi = 600, 
       units = "px")

#### qual o sorotipo mais frequente (gráfico)?

#### existe diferenças na distribuição dos sintomas por sexo (estatística)?#### 

sintomatologia_sexo = dengue_recife_clean |> 
  filter(str_detect(classi_fin, "dengue") == TRUE) |> 
  select(cs_sexo, febre:auto_imune) |> 
  pivot_longer(cols = febre:auto_imune,
               names_to = "sintomas",
               values_to = "presenca")

sintomatologia_sexo |> 
  mutate(cs_sexo = fct_relevel(cs_sexo, c("feminino", "masculino"))) |> 
  mutate(presenca = fct_relevel(presenca, c("não", "sim"))) |> 
  filter(sintomas == "cefaleia") %>%
  xtabs(~ cs_sexo + presenca, data = .) |> 
  fisher_test(detailed = TRUE)


fisher_sintoma_sexo = function(sintoma){
  sintomatologia_sexo |> 
    mutate(cs_sexo = fct_relevel(cs_sexo, c("feminino", "masculino"))) |> 
    mutate(presenca = fct_relevel(presenca, c("não", "sim"))) |> 
    filter(sintomas == sintoma) %>%
    xtabs(~ cs_sexo + presenca, data = .) |> 
    fisher_test(detailed = TRUE) |>  
    mutate(sintoma = sintoma) |> 
    relocate(sintoma, .before = n) %>%
    return()
}

fisher_sintoma_sexo("vomito")

lista_sintomas = sintomatologia_sexo |> 
  distinct(sintomas) |> 
  pull()

for(i in lista_sintomas){
  print(fisher_sintoma_sexo(i))
}

x = NULL
for(i in lista_sintomas){
  x = bind_rows(x, fisher_sintoma_sexo(i))
}

x |> 
  mutate(p_adj_final = p.adjust(p, method = "fdr")) |> 
  filter(p_adj_final < 0.05) |> 
  View()

#### quantas pessoas foram hospitalizadas? ####

dengue_recife_clean |> 
  group_by(hospitaliz) |> 
  summarise(numero_casos = n())

#### quais os cinco estabelecimentos de saúde com mais notificações? ####

dengue_recife_clean |> 
  group_by(id_unidade) |> 
  summarise(numero_casos = n()) %>%
  left_join(., cnes_recife_Clean, join_by(id_unidade == cnes)) |> 
  select(id_unidade, fantasia, numero_casos) |> 
  slice_max(order_by = numero_casos,
            n = 5)

#### qual a taxa de internação por dengue neste ano? ####

dengue_recife_clean |> 
  filter(str_detect(classi_fin, "dengue") == TRUE) |> 
  group_by(hospitaliz) |>
  summarise(numero_casos = n())

356/(356+1980)

356/(356+1980+1828)

#### existe uma faixa etária com mais chances de falecer por dengue? ####

dengue_recife_clean |> 
  filter(str_detect(classi_fin, "dengue") == TRUE) |> 
  mutate(faixa_etaria = case_when(
    nu_idade_n_codigo %in% c("dias", "meses") | nu_idade_n < 18 ~ "< 18",
    between(nu_idade_n, 18, 64) ~ "18-64",
    nu_idade_n >= 65 ~ "65 +")) |>
  mutate(faixa_etaria = fct_relevel(faixa_etaria, c("< 18", "18-64", "65 +"))) |> 
  mutate(hospitaliz = fct_relevel(hospitaliz, c("não", "sim"))) %>%
  xtabs(~ faixa_etaria + hospitaliz, data = .) |> 
  pairwise_fisher_test(detailed = TRUE)

#### quantos óbitos por dengue neste ano? ####

dengue_recife_clean |> 
  group_by(evolucao) |> 
  summarise(numero_casos = n())
