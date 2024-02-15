library(tidyverse)
library(readxl)

pwt1001 <- read_excel("pwt1001.xlsx", sheet = "Data")

valores_referencia<-
  pwt1001 %>%
  filter(countrycode == "USA",
         year== 2006)

cons_per_capita_ref <- valores_referencia$ccon/valores_referencia$pop

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
         cons_per_capita = ccon/pop, 
         gC = (((cons_per_capita) /lag(cons_per_capita))-1)*100,
         ct_constant = ccon / pop,
         vc = const_u + log(ct_constant/const_c),
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
  select( countrycode, country, year, pop, gN, ccon, cons_per_capita, gC, ct_constant, vc, gN_vc,glambda,decada ) 


df_trabalho %>%
  filter(countrycode %in% c("CHN", "ETH")) %>%
  ggplot(aes(x=year, y=gC)) +
  geom_line(aes(color=country, group = country))


fab<-
  pwt1001 %>%
  filter(countrycode %in% c("CHN"),
         year>=1959) %>%
  group_by(countrycode) %>%
  mutate(lag_ccon = ((ccon/lag(ccon))-1)*100,
         lag_cda = ((cda/lag(cda))-1)*100,
         lag_rconna = ((rconna/lag(rconna))-1)*100,
         lag_rdana = ((rdana/lag(rdana)-1))*100) %>%
  select(countrycode, year,ccon, cda, rconna, rdana, lag_ccon, lag_cda, lag_rconna, lag_rdana) %>%
  summarise(mean(lag_ccon, na.rm = TRUE),
            mean(lag_cda, na.rm = TRUE),
            mean(lag_rconna, na.rm = TRUE),
            mean(lag_rdana, na.rm = TRUE))

df_trabalho %>%
  filter(countrycode %in% c("CHN","ETH")) %>%
  summarise(
    mean(gC),
    .by = c(countrycode,decada)
    
  )
  



df_trabalho_agregado<-
df_trabalho%>%
  summarise(
    media_gc = mean(gC, na.rm = TRUE),
    media_gN = mean(gN, na.rm = TRUE),
    media_vc = mean(const_u + log((ccon / pop)/const_c), na.rm = TRUE),
    media_gN_vc = media_gN * media_vc,
    glambda_periodo = media_gN_vc + media_gc ,
    pop_share = (media_gN_vc/glambda_periodo)*100,
    .by = c(country, countrycode)
  )

#Cáclulo para países selecionados
df_trabalho_agregado %>%
  filter( countrycode %in% paises ) %>%
  arrange(desc(glambda_periodo))

