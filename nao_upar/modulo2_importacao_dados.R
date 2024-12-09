#### importação básica ####

#read_csv(): le arquivos .csv, separados por vírgula
idade_caranguejo = read_csv("datasets/crab_age_prediction/CrabAgePrediction.csv")

#read_csv2(): le arquivos .csv, separados por ponto e vírgula
food_waste = read_csv2("datasets/food_waste/Food Waste data and research - by country.csv")

#read_tsv(): le arquivos .csv, separados por tabulação (\t)
diabetes = read_tsv("datasets/healthcare_diabetes_dataset/Healthcare-Diabetes.tsv")

#read_delim(): função mais generalista
coracao = read_delim("datasets/heart_disease_predition_dataset/dataset_heart.csv")

#### importação de planilhas do excel####
library(readxl)

#escolher a planilha pelo numero
mental_health1 = read_excel("datasets/mental_health/mental_health.xlsx",
                            sheet = 1)

#escolher a planilha pelo nome
mental_health2 = read_excel("datasets/mental_health/mental_health.xlsx",
                            sheet = "Planilha2")

#ler todas as planilhas simultaneamente
path = "datasets/mental_health/mental_health.xlsx" 
path |>  
  excel_sheets() |> 
  set_names() |> 
  map(read_excel, path = path) |> 
  imap(~ assign(.y, 
                .x, envir = .GlobalEnv))

#### importação a partir de URL. Ex.: github #####
rice = read_delim("https://raw.githubusercontent.com/bio-hub/curso_R1/refs/heads/main/datasets/rice_pests_diseases/RICE.csv?token=GHSAT0AAAAAACW5UN5QUZG55EOJ53JO6CEOZ2V4H3Q")

#### importação a partir do Google Sheets #####
library(googlesheets4)

#requisitar autorizacao
gs4_auth()

#baixar a planilha
exames = read_sheet("https://docs.google.com/spreadsheets/d/1lj6w0owt2fXAoH03uKqQT0GxiecWu6TZoj7pMcVO3Ek/edit?gid=0#gid=0", sheet = 1)

#### importação de datasets grandes ####
library(data.table)
meteoritos = fread("datasets/meteorite_landings/meteorite-landings.csv")
####FIM####

