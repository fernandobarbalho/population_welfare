library(tidyverse)
library(readxl)

pwt1001 <- read_excel("pwt1001.xlsx", sheet = "Data")

cons_per_capita_ref <- 13573024/297.758969

const_u <- (185000/cons_per_capita_ref)  #4.87

const_c <- cons_per_capita_ref

paises<- c("USA","DEU","JPN","MEX","BRA", "ZAF", "CHN", "IND" , "ETH" )


pwt1001 %>%
  filter(countrycode %in% paises,
         year == 2019) %>%
  mutate(ct_current = (ccon)/(pop),
         ct_constant = (rconna)/(pop),
         vc = const_u + log(ct_constant/const_c)
  ) %>%
  select(year, country,  ccon, rconna, pop,  ct_current,   vc) %>%
  mutate(country = reorder(country,vc)) %>%
  ggplot() +
  geom_col(aes(x=vc, y= country), fill= "#0072bd") +
  theme_light() +
  theme(
    panel.grid = element_blank()
  )+
  labs(title = "v(c) accross countries in 2019",
       x= "years of per capita consumption",
       y= "") 



#Cálculo para todos os países a partir de 1960

df_trabalho<-
  pwt1001 %>%
  group_by(countrycode) %>%
  filter(year >= 1959) %>%
  mutate(gN =  ((pop/lag(pop))-1) *100,
         gC = ((rconna/lag(rconna))-1)*100,
         ct_constant = rconna / pop,
         vc = const_u + log(ct_constant/const_c),
         gN_vc = gN * vc,
         glambda = gN_vc + gC
         ) %>%
  ungroup() %>%
  select( countrycode, country, year, pop, gN, rconna, gC, ct_constant, vc, gN_vc,glambda ) %>%
  summarise(
    media_glambda = mean(glambda, na.rm = TRUE),
    media_gc = mean(gC, na.rm = TRUE),
    media_gN = mean(gN, na.rm = TRUE),
    media_vc = mean(vc, na.rm = TRUE),
    media_gN_vc = mean(gN_vc, na.rm = TRUE),
    .by = country
  )
