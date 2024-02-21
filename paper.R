library(tidyverse)
library(readxl)

pwt1001 <- read_excel("pwt1001.xlsx", sheet = "Data")

valores_referencia<-
  pwt1001 %>%
  filter(countrycode == "USA",
         year== 2005)

cons_per_capita_ref <- (valores_referencia$rgdpna*(valores_referencia$csh_c +0)) /valores_referencia$pop #desconsiderei o percenutal de gasto de governo para chegar nos 38k dolares per capita

const_u <-  4.87# (185000/cons_per_capita_ref)  #4.87

const_c <-   38000# cons_per_capita_ref

paises<- c("USA","DEU","JPN","MEX","BRA", "ZAF", "CHN", "IND" , "ETH" )


pwt1001 %>%
  filter(countrycode %in% paises,
         year == 2019) %>%
  mutate(ct_constant = (rgdpna*(csh_c+0))/(pop),
         vc = const_u + log(ct_constant/const_c)
  ) %>%
  select(year, country,  rgdpna, csh_c,  rconna, pop,  ct_constant,   vc) %>%
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

df_trabalho_v2<-
  pwt1001 %>%
  group_by(countrycode) %>%
  filter(year >= 1959) %>%
  mutate(gN =  ((pop/lag(pop))-1) *100,
         cons_per_capita = (rgdpna*(csh_c+csh_g)) /pop,
         cons_per_capita_s_gov = (rgdpna*(csh_c+0)) /pop,
         gC = (((cons_per_capita) /lag(cons_per_capita))-1)*100,
         vc = const_u + log(cons_per_capita_s_gov/const_c),
         gN_vc = gN * vc,
         glambda = gN_vc + gC,
         decada = case_when(
           between(year,1960, 1969) ~ "1960 - 1969",
           between(year,1970, 1979) ~ "1970 - 1979",
           between(year,1980, 1989) ~ "1980 - 1989",
           between(year,1990, 1999) ~ "1990 - 1999",
           between(year,2000, 2010) ~ "2000 - 2010",
           between(year,2011, 2019) ~ "2011 - 2019") 
  ) %>%
  ungroup() %>%
  select( countrycode, country, year, pop, gN, cons_per_capita, gC, vc, gN_vc,glambda,decada, rgdpna, csh_c, csh_g, ccon ) 







#Cáclulo para países selecionados




df_trabalho_agregado_v2<-
  df_trabalho_v2%>%
  summarise(
    media_gc = mean(gC, na.rm = TRUE),
    media_gN = mean(gN, na.rm = TRUE),
    media_vc = mean(vc),
    media_gN_vc = mean(gN_vc,na.rm = TRUE),
    glambda_periodo = media_gN_vc + media_gc ,
    pop_share = (media_gN_vc/glambda_periodo)*100,
    .by = c(country, countrycode)
  )

#Cáclulo para países selecionados
df_trabalho_agregado_v2 %>%
  filter( countrycode %in% paises ) %>%
  arrange(desc(glambda_periodo))%>%
  select(
    country,glambda_periodo, media_gc, media_gN, media_vc, media_gN_vc, pop_share
  )



