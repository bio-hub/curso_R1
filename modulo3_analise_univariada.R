library(tidyverse)
library(janitor)
library(skimr)
library(rstatix)

########## COMPARAÇÕES ENTRE PROPORÇÕES ####

#https://www.kaggle.com/datasets/utkarshx27/heart-disease-diagnosis-dataset
coracao = read_delim("datasets/heart_disease_predition_dataset/dataset_heart.csv")

coracao_clean = coracao |> 
  clean_names() |> 
  mutate(sex = case_when(sex == 0 ~ "feminino",
                         sex == 1 ~ "masculino")) |> 
  mutate(sex = fct_relevel(sex, c("feminino", "masculino"))) |> 
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


#### Teste de fisher####

coracao_clean %>% 
  xtabs(~ sex + heart_disease, data = .) |> 
  fisher_test(detailed = TRUE)

#fisher par-a-par
coracao_clean %>% 
  xtabs(~ major_vessels + heart_disease, data = .) |> 
  pairwise_fisher_test(detailed = TRUE, 
                       p.adjust.method = "fdr") |> 
  View()

#### Teste qui-quadrado ####
coracao_clean %>% 
  xtabs(~ sex + heart_disease, data = .) |> 
  chisq_test()

#qui-quadrado par-a-par
coracao_clean %>% 
  xtabs(~ resting_electrocardiographic_results + heart_disease, data = .) |> 
  pairwise_prop_test()

coracao_clean %>% 
  xtabs(~ resting_electrocardiographic_results + heart_disease, data = .) |> 
  pairwise_fisher_test()

#### Cochran's Q test e McNemar's Chi-squared Test ####

#qui-quadrado para dados pareados
mydata = tibble(
  outcome = c(0,1,1,0,0,1,0,1,1,1,1,1,0,0,1,1,0,1,0,1,1,0,0,1,0,1,1,0,0,1),
  treatment = gl(3,1,30,labels=LETTERS[1:3]),
  participant = gl(10,3,labels=letters[1:10])) |>
  mutate(outcome = case_when(outcome == 0 ~ "success",
                             TRUE ~ "failure")) |> 
  mutate(outcome = fct_relevel(outcome, c("success", "failure")))

mydata %>%
  xtabs(~ treatment +outcome, data = .)
 
mydata |>  
  cochran_qtest(outcome ~ treatment | participant)

mydata |> 
  pairwise_mcnemar_test(outcome ~ treatment|participant)

########## CORRELAÇÃO ####

#carregar os dados
#https://www.kaggle.com/datasets/nasa/meteorite-landings
caranguejo = read_csv("datasets/crab_age_prediction/CrabAgePrediction.csv")

#visao geral dos dados
glimpse(caranguejo)
skim(caranguejo)

#transformação dos dados
caranguejo_clean = caranguejo |> 
  clean_names() |> 
  mutate(sex = na_if(sex, "I")) |> 
  mutate(sex = fct_relevel(sex, c("M", "F"))) |> 
  mutate(age = as.integer(age)) |> 
  drop_na()

skim(caranguejo_clean)

caranguejo_clean |> 
  select(length:shell_weight) |>
  cor_mat(method = "pearson") |> 
  cor_plot()


########## TESTES DE NORMALIDADE ####

#testes de normalidade de Shapiro-Wilk
caranguejo_clean |> 
  shapiro_test(length,
               diameter, 
               height, 
               weight, 
               shucked_weight, 
               viscera_weight,
               shell_weight)

#teste de normalidade usando o Kolmogorov-Smirnov
caranguejo_clean |> 
  select(length:shell_weight) |>
  imap_dfr(~ tibble(
    variable = .y,
    p_value = ks.test(.x, "pnorm", mean(.x), sd(.x)) |> pluck("p.value")
  ))


########## COMPARAÇÕES ENTRE MÉDIAS/MEDIANAS (ATÉ DOIS GRUPOS) ####

#### Testes de wilcoxon e Mann-Whitney ####

#um grupo (Mann-Whitney)
ToothGrowth |> 
  wilcox_test(len ~ 1, mu = 0)

#dois grupos nao-pareados (Mann-Whitney)
ToothGrowth |> 
  wilcox_test(len ~ supp)

#dois grupos pareados (Wilcoxon)
ToothGrowth |> 
  wilcox_test(len ~ supp, paired = TRUE)

#dados agrupados
ToothGrowth |> 
  group_by(dose) |> 
  wilcox_test(len ~ supp) |> 
  adjust_pvalue(method = "bonferroni") |> 
  add_significance("p.adj") |> 
  View()

#teste par-a-par (faz automaticamente quando tem mais de dois grupos)
ToothGrowth |> 
  wilcox_test(len ~ dose)

#teste par-a-par assuindo um grupo de referencia
ToothGrowth |> 
  wilcox_test(len ~ dose, ref.group = "0.5") |> 
  View()

#### Teste t normal e pareado ####

#para o Teste t (normal ou pareado), basta alterar a função para t_test()
ToothGrowth |> 
  t_test(len ~ supp, paired = TRUE)

ToothGrowth |> 
  group_by(dose) |> 
  t_test(len ~ supp) |> 
  adjust_pvalue(method = "bonferroni") |> 
  add_significance("p.adj")

ToothGrowth |> 
  t_test(len ~ dose, ref.group = "0.5")


########## COMPARAÇÕES ENTRE MÉDIAS/MEDIANAS (TRÊS OU MAIS GRUPOS) ####

#### Kruskal Wallis e Dunn's test####

ToothGrowth |>
  mutate(dose = as.factor(dose)) |>
  kruskal_test(len ~ dose)

#grouped
ToothGrowth |>
  mutate(dose = as.factor(dose)) |>
  group_by(supp) |>
  kruskal_test(len ~ dose)

#post hoc test
ToothGrowth |> 
  mutate(dose = as.factor(dose)) |>
  dunn_test(len ~ dose)

# Grouped data
ToothGrowth |>
  mutate(dose = as.factor(dose)) |>
  group_by(supp) |>
  dunn_test(len ~ dose)

#### one-way ANOVA (Para amostras independentes) e tukey post-hoc tests####
library(emmeans)

ToothGrowth = ToothGrowth |>
  mutate(dose = as.factor(dose))

#verificar outliers
ToothGrowth |> 
  group_by(dose) |>
  identify_outliers(len)

#teste de normalidade
ToothGrowth |> 
  group_by(dose) |>
  shapiro_test(len)

#verificar a homonegeicidade das variâncias
ToothGrowth |>
  levene_test(len ~ dose)

#realizar a anova se os dados foram normais e a variância homogênia
ToothGrowth |> 
  anova_test(len ~ dose) |> 
  get_anova_table()

#post-hoc test
ToothGrowth |> 
  tukey_hsd(len ~ dose)

#realizar o Welch one-way test se os dados foram normais e a 
#variância não for homogênia
ToothGrowth |> 
  welch_anova_test(len ~ dose)

#post-hoc test
ToothGrowth |> 
  games_howell_test(len ~ dose)

#### two-way ANOVA (Para amostras independentes) e post-hoc tests####
library(emmeans)

ToothGrowth = ToothGrowth |>
  mutate(dose = as.factor(dose))

#verificar outliers
ToothGrowth |> 
  group_by(dose, supp) |>
  identify_outliers(len)

#teste de normalidade
ToothGrowth |> 
  group_by(dose, supp) |>
  shapiro_test(len)

#verificar a homonegeicidade das variâncias
ToothGrowth |>
  levene_test(len ~ dose*supp)

#realizar a anova se os dados foram normais e a variância homogênia
ToothGrowth |> 
  anova_test(len ~ dose*supp) |> 
  get_anova_table()

#post-hoc test para interações two-way significativas
#calcula a one-way ANOVA da resposta com a primeira variável,
#com os dados agrupados pela segunda variável
ToothGrowth |> 
  group_by(supp) |> 
  anova_test(len ~ dose)

#caso a one-way anova de significativa, realiza-se o post-hoc da one-way ANOVA
ToothGrowth |> 
  group_by(supp)  |> 
  emmeans_test(len ~ dose, 
               p.adjust.method = "bonferroni") 

#post-hoc test para interações two-way não-significativas
#Perform pairwise comparisons between education level groups to determine 
#which groups are significantly different. Bonferroni adjustment is applied.
ToothGrowth |> 
  pairwise_t_test(len ~ dose)

ToothGrowth |> 
  pairwise_t_test(len ~ supp)

#### three-way ANOVA (Para amostras independentes) e post-hoc tests####
library(emmeans)
library(datarium)

#verificar outliers
headache |> 
  group_by(gender, risk, treatment) |>
  identify_outliers(pain_score)

#teste de normalidade
headache |> 
  group_by(gender, risk, treatment) |>
  shapiro_test(pain_score)

#verificar a homonegeicidade das variâncias
headache |>
  levene_test(pain_score ~ gender*risk*treatment)

#realizar a anova se os dados foram normais e a variância homogênia
headache |> 
  anova_test(pain_score ~ gender*risk*treatment)

#post-hoc test para interações three-way significativas
#calcula a two-way ANOVA da resposta com a as duas primeiras variáveis,
#com os dados agrupados pela terceira variável
headache |> 
  group_by(treatment) |> 
  anova_test(pain_score ~ gender*risk)

#caso a two-way anova de significativa, realiza-se o post-hoc da one-way ANOVA
#filtrado apenas para as variáveis que foram significativas. No exemplo, 
#treatment == X
headache |> 
  filter(treatment == "X") |> 
  group_by(risk, treatment) |> 
  anova_test(pain_score ~ gender)

#como sobrou apenas um tipo de tratamento e um tipo de risco, nao é necessário
#agrupar os dados
headache |> 
  filter(treatment == "X") |>
  filter(risk == "high") |> 
  # group_by(risk, treatment) |> 
  emmeans_test(pain_score ~ gender, p.adjust.method = "bonferroni") 

#estimar as médias marginais
headache |> 
  filter(treatment == "X") |>
  filter(risk == "high") |> 
  # group_by(risk, treatment) |> 
  emmeans_test(pain_score ~ gender, p.adjust.method = "bonferroni")|> 
  get_emmeans()

#post-hoc test para interações two-way significativas
#proceder como no caso da two-way ANOVA
#calcula a one-way ANOVA da resposta com a primeira variável,
#com os dados agrupados pela segunda variável
headache |> 
  group_by(treatment) |> 
  anova_test(pain_score ~ gender)

#caso a one-way anova de significativa, realiza-se o post-hoc da one-way ANOVA
headache |> 
  group_by(treatment)  |> 
  emmeans_test(pain_score ~ gender, 
               p.adjust.method = "bonferroni") 

#estimar as médias marginais
headache |> 
  group_by(treatment)  |> 
  emmeans_test(pain_score ~ gender, 
               p.adjust.method = "bonferroni") |> 
  get_emmeans()

#post-hoc test para interações threw-way e two-way não-significativas
#Perform pairwise comparisons between education level groups to determine 
#which groups are significantly different. Bonferroni adjustment is applied.
headache |> 
  pairwise_t_test(pain_score ~ gender)

headache |> 
  pairwise_t_test(pain_score ~ risk)

headache |> 
  pairwise_t_test(pain_score ~ treatment)

#### one-way ANOVA para medidas repetidas (dados pareados) ####
library(datarium)

#converter id e time em fatores
selfesteem = selfesteem |> 
  pivot_longer(cols = t1:t3, 
               names_to = "time", 
               values_to = "score") |> 
  convert_as_factor(id, time)

#identificar outliers
selfesteem %>%
  group_by(time) %>%
  identify_outliers(score)

#teste de normalidade
selfesteem %>%
  group_by(time) %>%
  shapiro_test(score)

#Realizar o teste
#the assumption of sphericity will be automatically checked during the 
#computation of the ANOVA test using the R function anova_test(). 
#The Mauchly’s test is internally used to assess the sphericity assumption.
#By using the function get_anova_table() to extract the ANOVA table, 
#the Greenhouse-Geisser sphericity correction is automatically applied to 
#factors violating the sphericity assumption.
selfesteem |> 
  anova_test(dv = score, 
             wid = id, 
             within = time) |> 
  get_anova_table()

#pos-hoc test
selfesteem |> 
  pairwise_t_test(
    score ~ time, paired = TRUE,
    p.adjust.method = "bonferroni")


#### two-way ANOVA para medidas repetidas (dados pareados) ####
library(datarium)

selfesteem2 = selfesteem2 |> 
  pivot_longer(cols = t1:t3, 
               names_to = "time", 
               values_to = "score") |> 
  convert_as_factor(id, time)

#identificar outliers
selfesteem2 %>%
  group_by(treatment, time) %>%
  identify_outliers(score)

#teste de normalidade
selfesteem2 %>%
  group_by(treatment, time) %>%
  shapiro_test(score)

#Realizar o teste
#the assumption of sphericity will be automatically checked during the 
#computation of the ANOVA test using the R function anova_test(). 
#The Mauchly’s test is internally used to assess the sphericity assumption.
#By using the function get_anova_table() to extract the ANOVA table, 
#the Greenhouse-Geisser sphericity correction is automatically applied to 
#factors violating the sphericity assumption.
selfesteem2 |> 
  anova_test(dv = score, 
             wid = id, 
             within = c(treatment, time)) |> 
  get_anova_table()

#pos-hoc test
#for significant two-way interaction, run one-way model of the first 
#variable (factor A) at each level of the second variable (factor B) or 
#vice-versa. if the simple main effect is significant, run multiple pairwise 
#comparisons to determine which groups are different.
selfesteem2 |> 
  group_by(time) |> 
  anova_test(dv = score, 
             wid = id, 
             within = treatment) |> 
  get_anova_table() |> 
  adjust_pvalue(method = "bonferroni")

selfesteem2 %>%
  group_by(time) %>%
  pairwise_t_test(
    score ~ treatment, paired = TRUE,
    p.adjust.method = "bonferroni"
  )

#for non-significant two-way interaction you need to interpret the main effects 
#for each of the two variables: treatment and time. A significant main effect 
#can be followed up with pairwise comparisons.
selfesteem2 %>%
  pairwise_t_test(
    score ~ treatment, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )

selfesteem2 %>%
  pairwise_t_test(
    score ~ time, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )

#### three-way ANOVA para medidas repetidas (dados pareados)####
library(datarium)

#pivot longer and conver to factor
weightloss = weightloss |> 
  pivot_longer(cols = t1:t3, 
               names_to = "time", 
               values_to = "score") |> 
  convert_as_factor(id, time)

#outliers
weightloss |> 
  group_by(diet, exercises, time) |> 
  identify_outliers(score)

#normalidade
weightloss |> 
  group_by(diet, exercises, time) |> 
  shapiro_test(score)

#computação do teste
weightloss |> 
  anova_test(dv = score, 
             wid = id, 
             within = c(diet, exercises, time)) |> 
  get_anova_table()

#Post-hoc tests
#Simple two-way interaction: run two-way interaction at each level of 
#third variable;
#Simple simple main effect: run one-way model at each level of second variable; 
#simple simple pairwise comparisons: run pairwise or other post-hoc 
#comparisons if necessary.

#two-way ANOVA para cada nivel da variável diet
weightloss |> 
  group_by(diet) |> 
  anova_test(dv = score, wid = id, within = c(exercises, time)) |> 
  get_anova_table()

#como apenas a interação exercises:time ocorreu quando a dieta é "no", 
#calcularemos apenas o efeito do tempo versus exercícios para esse tipo de 
#dieta.
weightloss |> 
  filter(diet == "no") |> 
  group_by(diet, exercises) |> 
  anova_test(dv = score, wid = id, within = time) |> 
  get_anova_table()

#como apenas a interação exercises:time ocorreu quando a dieta é "no" e 
#o efeito do tempo foi apenas quando exercises é igual a "yes", calcularemos
#o teste pairwise apenas nesses casos.
weightloss %>%
  filter(diet == "no") |> 
  filter(exercises == "yes") |> 
  group_by(diet, exercises) |> 
  pairwise_t_test(score ~ time, 
                  paired = TRUE, 
                  p.adjust.method = "bonferroni")

#### Friedman test (não parametrico para one-way ANOVA repeated measures) ####
library(datarium)

#transformação nos dados
selfesteem = selfesteem |> 
  pivot_longer(cols = t1:t3, 
               names_to = "time", 
               values_to = "score") |>
  convert_as_factor(id, time)

#calculo do teste
selfesteem |>  
  friedman_test(score ~ time | id)

#calculo do effect size
#Kendall’s W uses the Cohen’s interpretation guidelines of 
#0.1 - < 0.3 (small effect), 
#0.3 - < 0.5 (moderate effect)
#>= 0.5 (large effect). 
#Confidence intervals are calculated by bootstrap.

selfesteem |> 
  friedman_effsize(score ~ time | id)

#pair-wise porst-hoc comparisions
# pairwise comparisons
selfesteem |>
  wilcox_test(score ~ time, paired = TRUE, p.adjust.method = "bonferroni")

########## FIM ####

