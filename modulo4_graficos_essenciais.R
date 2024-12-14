library(tidyverse)
library(janitor)
library(rstatix)
library(ggpubr)

#### LINE CHART ####

meteoritos = read_delim("datasets/meteorite_landings/meteorite-landings.csv")
meteoritos_clean = meteoritos |> 
  filter(between(year, 1901, 2000)) |> 
  filter(mass >= 1) |> 
  filter(fall == "Fell") |>
  filter(nametype == "Valid") |>
  drop_na() |> 
  mutate(decade = floor(year / 10) * 10) |>
  relocate(decade, .after = year) 

p1 = meteoritos_clean |> 
  group_by(decade) |> 
  count() |> 
  ggplot(aes(x = decade, 
             y = n,
             group = 1)) +
  geom_line(linewidth = 1,
            color = "#073A60") +
  scale_x_continuous(breaks = seq(1900, 2000, by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), 
                     limits = c(0, 100)) +
  theme_minimal(base_size = 18, 
                base_family = "serif") +
  theme(panel.grid = element_blank(),
        axis.line = element_line(color = "black")) +
  labs(title = "Número de meteoritos que cairam na Terra",
       subtitle = "dados para o século XX",
       caption = "FONTE: NASA",
       x = NULL,
       y = "Número de meteoritos")

p1

#### BAR CHART ####

food_waste = read_delim("datasets/food_waste/Food Waste data and research - by country.csv")
p2 = food_waste |>
  clean_names() |> 
  filter(region == "Latin America and the Caribbean") |> 
  ggplot(aes(x = reorder(country, 
                         -household_estimate_kg_capita_year),
             y = household_estimate_kg_capita_year)) +
  geom_bar(stat = "identity",
           fill = "cornflowerblue") +
  theme_minimal(base_size = 18, 
                base_family = "sans") +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_label(aes(label = household_estimate_kg_capita_year), 
             show.legend = FALSE,
             size = 3,
             position = position_dodge2(),
             alpha = 0.8) + 
  rotate_x_text() +
  labs(x = "País", 
       y = "Desperdicio em casa (kg/per capita por ano)")
  
p2

#### BAR CHART WITH ERROR BAR ####

food_waste = read_delim("datasets/food_waste/Food Waste data and research - by country.csv")
p3 = food_waste |>
  clean_names() |> 
  arrange(region) |> 
  ggbarplot(x = "region",
            y = "household_estimate_kg_capita_year", 
            add = "mean_sd",
            fill = "#80A1BC") +
  theme_minimal(base_size = 18, 
                base_family = "sans") +
  theme(panel.grid = element_blank(),
        axis.line = element_line("black")) +
  labs(x = "Região", 
       y = "Desperdicio em casa (kg/per capita por ano)") +
  coord_flip()

p3

#### BAR CHART WITH FISHER'S TEST ####

coracao = read_delim("datasets/heart_disease_predition_dataset/dataset_heart.csv")
coracao_clean = coracao |> 
  clean_names() |> 
  mutate(sex = case_when(sex == 0 ~ "feminino",
                         sex == 1 ~ "masculino")) |> 
  mutate(sex = fct_relevel(sex, c("masculino", "feminino"))) |> 
  mutate(heart_disease = case_when(
    heart_disease == 1 ~ "não",
    heart_disease == 2 ~ "sim")) |> 
  mutate(heart_disease = fct_relevel(
    heart_disease, 
    c("não", "sim"))) |> 
  mutate(fasting_blood_sugar = case_when(
    fasting_blood_sugar == 0 ~ "normal",
    fasting_blood_sugar == 1 ~ "alta")) |> 
  mutate(fasting_blood_sugar = fct_relevel(
    fasting_blood_sugar, 
    c("normal", "alta"))) 

coracao_fisher = coracao_clean %>% 
  xtabs(~ sex + heart_disease, data = .) |> 
  fisher_test()

p4 = coracao_clean %>% 
  xtabs(~ sex + heart_disease, data = .) |>
  as_tibble() |> 
  ggbarplot(x = "sex",
            y = "n", 
            fill = "heart_disease",
            position = position_dodge2()) +
  geom_text(data = coracao_fisher, 
            aes(x = -Inf, y = Inf, 
                label = paste("p =", format(p, digits = 4))), 
            hjust = -0.1, 
            vjust = 1.5,
            size = 5,
            color = "#073A60",
            inherit.aes = FALSE) +
  geom_label(aes(label = n), 
             show.legend = FALSE,
             size = 5,
             position = position_dodge2(0.70),
             alpha = 0.8)  +
  theme_minimal(base_size = 18, 
                base_family = "sans") +
  theme(panel.grid = element_blank(),
        axis.line = element_line("black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  scale_fill_manual(values = c("#073A60", "#C9384B")) +
  labs(x = NULL,
       y = "contagem",
       fill = "doença cardiovascular",
       caption = "NOTE: p-value from Fisher's Exact Test")

p4
#### SCATTER PLOT ####

#https://archive.ics.uci.edu/dataset/73/mushroom
mushrooms = read_delim("datasets/mushroom_dataset/mushroom_cleaned.csv")

#ver combinacoes de cores possiveis no dataset
mushrooms |> 
  clean_names() %>%
  xtabs(~ gill_color + stem_color, data = .)

#selecionar uma combinacao
p5 = mushrooms |> 
  clean_names() |> 
  filter(gill_color == 1) |> 
  filter(stem_color == 1) |> 
  mutate(class = case_when(class == 0 ~ "comestível",
                           TRUE ~ "venenoso")) |> 
  filter(cap_diameter < 500) |> 
  ggplot(aes(x = stem_height,
             y = cap_diameter, 
             color = class)) +
  geom_point(aes(size = stem_width),
             alpha = 0.40) +
  geom_smooth(method = "lm",
              formula = y ~ x,
              se = FALSE,
              show.legend = FALSE) +
  theme_minimal(base_size = 18, 
                base_family = "sans") +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  scale_color_manual(values = c("#073A60", "#C9384B")) +
  scale_x_continuous(breaks = seq(0, 1.5, by = 0.25), limits = c(0, 1.25)) +
  scale_y_continuous(breaks = seq(0, 500, by = 50), limits = c(0, 500)) +
  labs(x = "altura do pé (cm)",
       y = "diâmetro do chapéu (cm)",
       color = "pode comer?") +
  guides(size = guide_none())

p5

#### HISTOGRAM ####

meteoritos = read_delim("datasets/meteorite_landings/meteorite-landings.csv")
p6 = meteoritos |> 
  filter(between(year, 1901, 2000)) |> 
  filter(mass >= 1) |> 
  filter(fall == "Fell") |>
  filter(nametype == "Valid") |>
  mutate(mass_log2 = log2(mass)) |> 
  drop_na() |> 
  ggplot(aes(x = mass_log2)) +
  geom_histogram(bins = 30,
                 fill = "#C9384B", 
                 color = "white") +
  theme_minimal(base_size = 18, 
                base_family = "sans") +
  theme(panel.grid.minor = element_blank()) +
  coord_cartesian(xlim = c(0, 18))  +
  scale_x_continuous(breaks = seq(0, 18, by = 1)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  labs(title = "distribuição da massa dos meteoritos que cairam na Terra",
       subtitle = "dados do século XX",
       caption = "FONTE: NASA",
       x = "massa do meteorito (em log2 gramas)",
       y = "número de meteoritos")

p6

#### DENSITY PLOT ####

smoke = read_delim("datasets/smokers_health_data/smoking_health_data_final.csv")
p7 = smoke |> 
  mutate(current_smoker = fct_relevel(current_smoker, c("no", "yes"))) |> 
  mutate(sex = fct_relevel(sex, c("male", "female"))) |> 
  separate(blood_pressure, into = c("sis", "dia"), sep = "/") |> 
  mutate(across(sis:dia,
                ~ as.numeric(.x))) |> 
  drop_na() |> 
  ggplot(aes(x = chol,
             fill = current_smoker)) + 
  geom_density(alpha = 0.5) +
  coord_cartesian(xlim = c(100, 400)) +
  theme_minimal(base_size = 18, 
                base_family = "sans") +
  theme(panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  facet_wrap(~ sex) +
  labs(title = "Proporção de fumantes e não-fumantes de acordo com o Colesterol total",
       subtitle = "dado desagregado por sexo",
       x = "colesterol total",
       y = NULL,
       fill = "fumante atual?") +
  scale_x_continuous(breaks = seq(100, 400, by = 50)) +
  scale_fill_manual(values = c("#073A60", "#C9384B"))
  
p7

#### DENSITY PLOT COM LINHAS DE MÉDIA E DESVIO PADRÃO ####

smoke = read_delim("datasets/smokers_health_data/smoking_health_data_final.csv")

smoke_media = smoke |> 
  summarize(mean = mean(chol, na.rm = TRUE),
            sd = sd(chol, na.rm = TRUE))

p8 = smoke |> 
  mutate(current_smoker = fct_relevel(current_smoker, c("no", "yes"))) |> 
  mutate(sex = fct_relevel(sex, c("male", "female"))) |> 
  separate(blood_pressure, into = c("sis", "dia"), sep = "/") |> 
  mutate(across(sis:dia,
                ~ as.numeric(.x))) |> 
  drop_na() |> 
  ggplot(aes(x = chol)) + 
  geom_density(alpha = 0.5,
               fill = "#073A60") +
  coord_cartesian(xlim = c(100, 400)) +
  geom_vline(data = smoke_media, 
             aes(xintercept = mean, 
                 color = "#C9384B"),
             linetype = "solid", 
             linewidth = 1, 
             show.legend = FALSE) +
  geom_vline(data = smoke_media, 
             aes(xintercept = mean - sd, 
                 color = "#C9384B"),
             linetype = "dashed", 
             linewidth = 1, 
             show.legend = FALSE) +
  geom_vline(data = smoke_media, 
             aes(xintercept = mean + sd, 
                 color = "#C9384B"),
             linetype = "dashed", 
             linewidth = 1, 
             show.legend = FALSE) +
  theme_minimal(base_size = 18, 
                base_family = "sans") +
  theme(panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  labs(title = "Proporção de Colesterol total no dataset",
       x = "colesterol total",
       y = NULL) +
  scale_x_continuous(breaks = seq(100, 400, by = 50))

p8

#### HEATMAP ####

arroz = read_delim("datasets/rice_pests_diseases/RICE.csv")

arroz |> 
  clean_names() %>%
  filter(pest_name == "Brownplanthopper") %>%
  xtabs(~ observation_year + location, data = .)

p9 = arroz |> 
  clean_names() |> 
  filter(observation_year == 2007) |>
  filter(pest_name == "Brownplanthopper") |> 
  ggplot(aes(x = standard_week, 
             y = location, 
             fill = pest_value)) +
  geom_tile(color = "white") +
  coord_equal() +
  scale_fill_gradient(low = "#D5E2E8", 
                      high = '#C9384B') +
  theme_minimal(base_size = 18, 
                base_family = "sans") +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top") +
  labs(title = "Ocorrência de Brownplanthopper por região",
       subtitle = "dados de 2007",
       x = "semana",
       y = element_blank(),
       fill = "pest value") +
  guides(fill = guide_colorbar(barwidth = unit(10, 'cm')))

p9

#### QQ Plot ####

test_normality = PlantGrowth |> 
  group_by(group) |> 
  shapiro_test(weight) |> 
  mutate(group = as.factor(group))

p10 = PlantGrowth %>%
  ggqqplot("weight",
           facet.by = "group",
           color = "#073A60",
           alpha = 0.8) +
  geom_text(data = test_normality, 
            aes(x = -Inf, y = Inf, 
                label = paste("p =", format(p, digits = 4))), 
            hjust = -0.1, 
            vjust = 1.5,
            size = 5,
            color = "#073A60",
            inherit.aes = FALSE) +
  theme_minimal(base_size = 18, 
                base_family = "sans") +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  labs(caption = "p-values from Shapiro Test for normality")

p10

#### BOXPLOT ####

PlantGrowth |> 
  group_by(group) |> 
  identify_outliers(weight)

res_aov = PlantGrowth |>  
  anova_test(weight ~ group)

pwc = PlantGrowth |> 
  tukey_hsd(weight ~ group) |> 
  add_xy_position(x = "group")

p11 = PlantGrowth |> 
  group_by(group) |> 
  ggboxplot(x = "group", 
            y = "weight") +
  labs(x = "grupo",
       y = "peso") +
  stat_pvalue_manual(pwc, hide.ns = TRUE, 
                     tip.length = 0.01, 
                     step.increase = 0.1) +
  labs(subtitle = get_test_label(res_aov, detailed = TRUE),
       caption = get_pwc_label(pwc))

p11

#### BOXPLOT WITH T TEST ####

library(datarium)
pwc_headache = headache |> 
  group_by(gender) |> 
  t_test(pain_score ~ treatment, p.adjust.method = "bonferroni") |> 
  add_xy_position(x = "treatment")

p12 = headache |> 
  ggboxplot(x = "treatment", 
            y = "pain_score", 
            fill = "#073A60", 
            facet.by = "gender") +
  stat_pvalue_manual(pwc_headache, 
                     color = "black", 
                     linetype = "solid", 
                     label = "p.adj",
                     hide.ns = FALSE,
                     tip.length = 0.01, 
                     step.increase = 0.1, 
                     step.group.by = "gender") +
  theme_bw(base_size = 18, 
           base_family = "sans") +
  theme(panel.grid = element_blank(),
        axis.line = element_line("black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  scale_fill_manual(values = c("#073A60", "#C9384B")) +
  labs(y = "pain score",
       caption = get_pwc_label(pwc_headache)) +
  guides(linetype = guide_none())

p12

#### VIOLIN PLOT ####

library(datarium)
pwc_headache = headache |> 
  group_by(gender) |> 
  t_test(pain_score ~ treatment, p.adjust.method = "bonferroni") |> 
  add_xy_position(x = "treatment") |> 
  mutate(y.position = y.position * 1.20)

p13 = headache |> 
  ggviolin(x = "treatment", 
           y = "pain_score", 
           color = "#073A60", 
           facet.by = "gender", 
           add = "boxplot") +
  stat_pvalue_manual(pwc_headache, 
                     color = "black", 
                     linetype = "solid", 
                     label = "p.adj",
                     hide.ns = FALSE,
                     tip.length = 0.01, 
                     step.increase = 0.1, 
                     step.group.by = "gender") +
  theme_bw(base_size = 18, 
           base_family = "sans") +
  theme(panel.grid = element_blank(),
        axis.line = element_line("black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  scale_fill_manual(values = c("#073A60", "#C9384B")) +
  labs(y = "pain score",
       caption = get_pwc_label(pwc_headache)) +
  guides(linetype = guide_none())

p13

#### DONUT CHART ####

coracao = read_delim("datasets/heart_disease_predition_dataset/dataset_heart.csv")
coracao_clean = coracao |> 
  clean_names() |> 
  mutate(sex = case_when(sex == 0 ~ "feminino",
                         sex == 1 ~ "masculino")) |> 
  mutate(sex = fct_relevel(sex, c("masculino", "feminino"))) |> 
  mutate(heart_disease = case_when(
    heart_disease == 1 ~ "não",
    heart_disease == 2 ~ "sim")) |> 
  mutate(heart_disease = fct_relevel(
    heart_disease, 
    c("não", "sim"))) |> 
  mutate(fasting_blood_sugar = case_when(
    fasting_blood_sugar == 0 ~ "normal",
    fasting_blood_sugar == 1 ~ "alta")) |> 
  mutate(fasting_blood_sugar = fct_relevel(
    fasting_blood_sugar, 
    c("normal", "alta")))

p14 = coracao_clean |> 
  group_by(sex) |>
  summarise(n = n()) |>
  mutate(fraction = n/sum(n)) |>
  mutate(ymax = cumsum(fraction)) |>
  mutate(ymin = c(0, head(ymax, n=-1))) |>
  mutate(labelPosition = (ymax + ymin)/2) |>
  mutate(label = sprintf("%s\n n = %d\n (%.2f%%)", sex, n, fraction * 100)) |> 
  mutate(label_position = (ymax + ymin)/2) |> 
  ggplot(aes(ymax = ymax, 
             ymin = ymin, 
             xmax = 4, 
             xmin = 3, 
             fill = sex)) +
  geom_rect() +
  geom_label(aes(x=4.75,
                 y=label_position, 
                 label=label),
             size = 5,
             color = "black",
             fill = NA) +
  coord_polar(theta="y") +
  xlim(c(2, 5)) +
  theme_minimal(base_size = 18, 
                base_family = "sans") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values = c("#073A60", "#C9384B")) +
  labs(title = "Contagem de pessoas do sexo masculino e feminino",
       x = NULL,
       y = NULL)

p14

#### TREEMAP CHART ####

library(treemapify)

mushrooms = read_delim("datasets/mushroom_dataset/mushroom_cleaned.csv")

total_cogumelos = mushrooms |> 
  nrow()

p15 = mushrooms |> 
  clean_names() |> 
  group_by(gill_color) |> 
  summarise(n = n()) |> 
  rename(gill_color_code = gill_color) |> 
  mutate(gill_color = case_when(
    gill_color_code == 0 ~ "black",
    gill_color_code == 1 ~ "brown",
    gill_color_code == 2 ~ "buff",
    gill_color_code == 3 ~ "chocolate",
    gill_color_code == 4 ~ "gray",
    gill_color_code == 5 ~ "green",
    gill_color_code == 6 ~ "orange",
    gill_color_code == 7 ~ "pink",
    gill_color_code == 8 ~ "purple",
    gill_color_code == 9 ~ "red",
    gill_color_code == 10 ~ "white",
    gill_color_code == 11 ~ "yellow")) |> 
  mutate(gill_color_hex = case_when(
    gill_color_code == 0  ~ "#000000",
    gill_color_code == 1  ~ "#A52A2A",
    gill_color_code == 2  ~ "#F0DC82",
    gill_color_code == 3  ~ "#D2691E",
    gill_color_code == 4  ~ "#808080",
    gill_color_code == 5  ~ "#008000",
    gill_color_code == 6  ~ "#FFA500",
    gill_color_code == 7  ~ "#FFC0CB",
    gill_color_code == 8  ~ "#800080",
    gill_color_code == 9  ~ "#FF0000",
    gill_color_code == 10 ~ "#FFFFFF",
    gill_color_code == 11 ~ "#FFFF00")) |> 
  ggplot(aes(area = n, 
             fill = gill_color_hex)) + 
  geom_treemap() +
  geom_treemap_text(aes(label =  sprintf("%s\n (%.0f)", gill_color, n)),
                    colour = "#073A60",
                    place = "centre",
                    size = 10) +
  theme_minimal(base_size = 18,
                base_family = "sans") +
  theme(legend.position = "none") +
  scale_fill_identity() +
  labs(title = "Distribuição dos cogumelos encontrados de acordo com a cor himênio",
       subtitle = sprintf("total de cogumelos estudados: %d", total_cogumelos))

p15

#### SALVAR OS PLOTS ####

ggsave(p1,
       filename = "./fig1.jpg", 
       width = 1920*5, 
       height = 1080*5, 
       dpi = 600, 
       units = "px")

