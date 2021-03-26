# Paquetes
library(readr)
library(dplyr)
library(ggplot2)


# Datos
covid <- read_csv("./Data/covid_analysis.csv")

covid_tbl <- covid %>% 
  mutate(IP_mov_avg = case_when(
    COUNTRY == "BOLIVIA" & DIA %in% 118:125 ~ NA_real_,
    COUNTRY == "PERU" & DIA %in% c(139, 146) ~ NA_real_,
    COUNTRY == "MEXICO" & DIA %in% 116:125 ~ NA_real_,
    COUNTRY == "PUERTO RICO" & DIA %in% 1:3 ~ NA_real_,
    COUNTRY == "PARAGUAY" & DIA %in% 1:3 ~ NA_real_,
    TRUE ~ IP_mov_avg))

# EDA
covid_tbl %>% 
  filter(COUNTRY == "BOLIVIA") %>% 
  mutate(ola = ifelse(IP_mov_avg > 2000 & DIA < 40, '1', '0')) %>% 
  ggplot(aes(DIA, IP_mov_avg, col = ola)) +
  geom_point() 

covid %>% 
  filter(COUNTRY == "CHILE") %>% 
  mutate(ola = ifelse(IP_mov_avg > 250, '1', '0')) %>% 
  ggplot(aes(DIA, IP_mov_avg, col = ola)) +
  geom_point() 

# covid %>% 
#   filter(COUNTRY == "ECUADOR") %>% 
#   mutate(ola = ifelse(IP_mov_avg > 2000 & DIA < 40, '1', '0')) %>% 
#   ggplot(aes(DIA, IP_mov_avg, col = ola)) +
#   geom_point() 

covid %>% 
  filter(COUNTRY == "EL SALVADOR") %>% 
  mutate(ola = ifelse(IP_mov_avg > 250, '1', '0')) %>% 
  ggplot(aes(DIA, IP_mov_avg, col = ola)) +
  geom_point() 

covid %>% 
  filter(COUNTRY == "GUATEMALA") %>% 
  mutate(ola = ifelse(IP_mov_avg > 2000 & DIA < 40, '1', '0')) %>% 
  ggplot(aes(DIA, IP_mov_avg, col = ola)) +
  geom_point() 

covid %>% 
  filter(COUNTRY == "MEXICO") %>% 
  mutate(ola = ifelse(IP_mov_avg > 250, '1', '0')) %>% 
  ggplot(aes(DIA, IP_mov_avg, col = ola)) +
  geom_point() 

covid_tbl %>% 
  filter(COUNTRY == "PERU") %>% 
  mutate(ola = ifelse(IP_mov_avg > 2000 & DIA < 40, '1', '0')) %>% 
  ggplot(aes(DIA, IP_mov_avg, col = ola)) +
  geom_point() 

covid %>% 
  filter(COUNTRY == "PUERTO RICO") %>% 
  mutate(ola = ifelse(IP_mov_avg > 250, '1', '0')) %>% 
  ggplot(aes(DIA, IP_mov_avg, col = ola)) +
  geom_point() 

covid %>% 
  filter(COUNTRY == "SPAIN") %>% 
  mutate(ola = ifelse(IP_mov_avg > 2000 & DIA < 40, '1', '0')) %>% 
  ggplot(aes(DIA, IP_mov_avg, col = ola)) +
  geom_point() 

covid %>% 
  filter(COUNTRY == "URUGUAY") %>% 
  mutate(ola = ifelse(IP_mov_avg > 250, '1', '0')) %>% 
  ggplot(aes(DIA, IP_mov_avg, col = ola)) +
  geom_point() 

covid %>% 
  filter(COUNTRY == "NETHERLAND") %>% 
  mutate(ola = ifelse(IP_mov_avg > 250, '1', '0')) %>% 
  ggplot(aes(DIA, IP_mov_avg, col = ola)) +
  geom_point() 

covid %>% 
  filter(COUNTRY == "COLOMBIA") %>% 
  mutate(ola = ifelse(IP_mov_avg > 2000 & DIA < 40, '1', '0')) %>% 
  ggplot(aes(DIA, IP_mov_avg, col = ola)) +
  geom_point() 

covid %>% 
  filter(COUNTRY == "BRAZIL") %>% 
  mutate(ola = ifelse(IP_mov_avg > 250, '1', '0')) %>% 
  ggplot(aes(DIA, IP_mov_avg, col = ola)) +
  geom_point() 

covid %>% 
  filter(COUNTRY == "PARAGUAY") %>% 
  mutate(ola = ifelse(IP_mov_avg > 250, '1', '0')) %>% 
  ggplot(aes(DIA, IP_mov_avg, col = ola)) +
  geom_point() 

# all countries IP
options(scipen = 999)

covid_tbl %>% 
  mutate(IP_mov_avg_pop = IP_mov_avg/POP) %>% 
  ggplot(aes(DIA, IP_mov_avg_pop)) +
  # geom_point(size = .5, alpha = .9) +
  geom_line(aes(col = COUNTRY), size = 1.5) + 
  facet_wrap(~COUNTRY, nrow = 2) +
  theme_bw() +
  theme(legend.position = "none")

covid_tbl %>% 
  ggplot(aes(DIA, R0_14)) +
  # geom_point(size = .5, alpha = .9) +
  geom_line(aes(col = COUNTRY), size = 1.5) + 
  facet_wrap(~COUNTRY, nrow = 2) +
  theme_bw() +
  theme(legend.position = "none")

covid_tbl %>% 
  mutate(CONTAGIADOS_MA_pop = CONTAGIADOS_MA/POP * 1000000) %>% 
  ggplot(aes(DIA, CONTAGIADOS_MA_pop)) +
  # geom_point(size = .5, alpha = .9) +
  geom_line(aes(col = COUNTRY), size = 1.5) + 
  facet_wrap(~COUNTRY, nrow = 2) +
  theme_bw() +
  theme(legend.position = "none")
