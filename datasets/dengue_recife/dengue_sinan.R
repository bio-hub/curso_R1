library(tidyverse)
library(read.dbc)
library(microdatasus)

#casos de dengue no DataSUS-SINAN no Brasil em 2024
dengue_24  = fetch_datasus(year_start = 2024,
                           year_end = 2024,
                           month_start = 1,
                           month_end = 12,
                           information_system = "SINAN-DENGUE",
                           timeout = 3600)

#filtrar para apenas casos no município do Recife - PE
#OBS: O código do município é de acordo com o IBGE
dengue_recife = dengue_24 |> 
  filter(ID_MUNICIP == "261160")

#salvar arquivo .csv
dengue_recife |> 
  write_csv2("datasets/dengue_recife/dengue_recife.csv")

#Estabelecimentos de saúde no Recife
cnes_recife = fetch_datasus(year_start = 2024,
                            year_end = 2024,
                            month_start = 1,
                            month_end = 12,
                            uf = "PE",
                            information_system = "CNES-ST") |> 
  filter(CODUFMUN == "261160") |> 
  process_cnes(information_system = "CNES-ST", 
               nomes = TRUE)

#salvar arquivo .csv
cnes_recife |> 
  write_csv2("datasets/dengue_recife/cnes_recife.csv")
