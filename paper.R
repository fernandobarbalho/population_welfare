library(tidyverse)
library(readxl)
pwt1001 <- read_excel("pwt1001.xlsx", sheet = "Data")

const_u <- (185000/44432.93)  #4.87

const_c <- 44432.93

paises<- c("USA","DEU","JPN","MEX","BRA", "ZAF", "CHN", "IND" , "ETH" )


pwt1001 %>%
  filter(countrycode %in% paises,
         year == 2019) %>%
  mutate(ct_current = (ccon)/(pop),
         vc = const_u + log(ct_current/const_c)
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