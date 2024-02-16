library(tidyverse)
library(readxl)
library(sidrar)

pwt1001 <- read_excel("pwt1001.xlsx", sheet = "Data")




pib_estados<- get_sidra(x = 5938, 
                           geo= "State",
                           variable = 496,
                        period = as.character(1991:2022))

pib_estados <- janitor::clean_names(pib_estados)

sum(pib_estados$valor)

pib_estados %>%
  arrange(desc(valor)) %>%
  select(unidade_da_federacao, valor)



populacao_municipios_1991_2022 <- read_csv("populacao_municipios_1991_2022.csv")