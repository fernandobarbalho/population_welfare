library(tidyverse)
library(readxl)
library(sidrar)
library(geobr)

pwt1001 <- read_excel("pwt1001.xlsx", sheet = "Data")



pib_ultimo_ano <- get_sidra(x=5938)

cnt_ultimo_ano <- get_sidra(x=1846)


pib_municipios<- get_sidra(x = 5938, 
                           geo= "City",
                           geo.filter = "2304400")

pib_municipios <- janitor::clean_names(pib_municipios)

pib_municipios %>%
  filter(variavel_codigo %in% c(37,498))

pib_estados<- get_sidra(x = 5938, 
                           geo= "State",
                           variable = 496,
                        period = as.character(1991:2022))

pib_estados <- janitor::clean_names(pib_estados)

pib_estados_va <- get_sidra(x = 5938, 
                            geo= "State",
                            variable = c(37,498),
                            period = as.character(1991:2022))


pib_estados_va <- janitor::clean_names(pib_estados_va)

populacao_municipios_1991_2022 <- read_csv("populacao_municipios_1991_2022.csv")

populacao_estados<-
  populacao_municipios_1991_2022 %>%
  summarise(populacao = sum(populacao, na.rm = TRUE),
            .by= c(id_uf, sigla_uf, ano))


dados_estados<-
populacao_estados %>%
  inner_join(
    pib_estados %>%
      mutate (id_uf = as.numeric(unidade_da_federacao_codigo),
              ano = as.numeric(ano))
  ) %>%
  select(id_uf, sigla_uf, ano, populacao, valor)


pwt1001_br <-
  pwt1001 %>%
  filter(countrycode == "BRA") %>%
  select(year, csh_c, csh_g, rgdpna )


pwt1001_usa <-
  pwt1001 %>%
  filter(countrycode == "USA") %>%
  mutate(dif_pe = rgdpna- cgdpe,
         dif_po = rgdpna- cgdpo) %>%
  select(year, ccon, rconna, csh_c, csh_g, rgdpna, pop, cgdpe, cgdpo, dif_pe, dif_po )


dados_estados_paper<-
dados_estados %>%
  inner_join(
    pwt1001_br %>%
      rename(ano = year)
  ) %>%
  mutate(valor  = valor /100,
         populacao = populacao/10^6) %>%
  mutate(consumo_uf = valor * ((csh_c+0)*rgdpna)) %>% #Cálculo do consumo estimado por UF em US$ 2017
  mutate(consumo_per_capita = consumo_uf/populacao)




dados_estados_paper %>%
  readr::write_csv("dados_estados_paper.csv")

dados_estados_paper%>%
  saveRDS("dados_estados_paper.RDS")

mapa_estados<-
  geobr::read_state()


mapa_estados %>% saveRDS("mapa_estados.rds")
