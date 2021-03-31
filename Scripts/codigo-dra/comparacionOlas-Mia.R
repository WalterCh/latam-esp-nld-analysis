# Paquetes
library(readr)
library(dplyr)
library(ggplot2)
library(gsheet)
library(lubridate)

source("./Scripts/R0-function.R")
source("./Scripts/Moving-average-function.R")

# Datos
# covid <- read_csv("./Data/covid_analysis.csv")
covid <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1YnsFPlJHJ9TQB5lZnqoEY6HZKkjRbns2kBr9I4HwNG4/edit?usp=sharing")

country_pops <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1YnsFPlJHJ9TQB5lZnqoEY6HZKkjRbns2kBr9I4HwNG4/edit#gid=1550796837", 
                           sheetid = 2)

covid_tbl <- covid %>% 
  left_join(country_pops) %>%
  group_by(COUNTRY) %>% 
  mutate(DIA = 1:n(),
         CONTAGIADOS_MA = mov_avg(CONTAGIADOS, 7),
         FALLECIDOS = case_when(
           COUNTRY == "BOLIVIA" & DIA %in% c(178:207) ~ NA_real_,
           COUNTRY == "PERU" & DIA %in% c(139, 146, 161) ~ NA_real_,
           TRUE ~ FALLECIDOS
         ),
         FALLECIDOS_MA = mov_avg(FALLECIDOS, 7),
         RECUPERADOS = case_when(
           COUNTRY == 'BOLIVIA' & DIA %in% 121:122 ~ NA_real_,
           TRUE ~ RECUPERADOS
         ),
         RECUPERADOS_MA = mov_avg(RECUPERADOS, 7),
         IP = CONTAGIADOS_MA + FALLECIDOS_MA - RECUPERADOS_MA,
         IP = case_when(
           COUNTRY == "PERU" & DIA %in% c(139, 146) ~ NA_real_,
           COUNTRY == "MEXICO" & DIA %in% 116:125 ~ NA_real_,
           TRUE ~ IP
         ),
         R0_14 = R0(CONTAGIADOS_MA, 14)
         ) %>% 
  ungroup()
  
  
  
  # mutate(CONTAGIADOS_MILL = CONTAGIADOS/POP * 1000000,
  #        FALLECIDOS_MILL = FALLECIDOS/POP * 1000000,
  #        COUNTRY = as.factor(COUNTRY),
  #        FECHA = dmy(FECHA),
  #        IP = CONTAGIADOS + FALLECIDOS - RECUPERADOS,
  #        IP_mov_avg = mov_avg(IP, 7),
  #        R0_14 = R0(CONTAGIADOS, 14)) %>%
  # group_by(COUNTRY) %>% 
  # mutate(CONTAGIADOS_MA = mov_avg(CONTAGIADOS, 7),
  #        CONTAGIADOS_MILL_MA = mov_avg(CONTAGIADOS_MILL, 7),
  #        FALLECIDOS_MA = mov_avg(FALLECIDOS, 7),
  #        FALLECIDOS_MILL_MA = mov_avg(FALLECIDOS_MILL, 7),
  #        RECUPERADOS_MA = mov_avg(RECUPERADOS, 7),
  #        DIA = 1:n(),
  #        IP_mov_avg = case_when(
  #          COUNTRY == "BOLIVIA" & DIA %in% 118:125 ~ NA_real_,
  #          COUNTRY == "PERU" & DIA %in% c(139, 146) ~ NA_real_,
  #          COUNTRY == "MEXICO" & DIA %in% 116:125 ~ NA_real_,
  #          COUNTRY == "PUERTO RICO" & DIA %in% 1:3 ~ NA_real_,
  #          COUNTRY == "PARAGUAY" & DIA %in% 1:3 ~ NA_real_,
  #          TRUE ~ IP_mov_avg),
  #        R0_MA = R0(CONTAGIADOS_MA, 14)) %>% 
  # ungroup()

# covid_tbl <- covid %>% 
#   mutate(IP_mov_avg = case_when(
#     COUNTRY == "BOLIVIA" & DIA %in% 118:125 ~ NA_real_,
#     COUNTRY == "PERU" & DIA %in% c(139, 146) ~ NA_real_,
#     COUNTRY == "MEXICO" & DIA %in% 116:125 ~ NA_real_,
#     COUNTRY == "PUERTO RICO" & DIA %in% 1:3 ~ NA_real_,
#     COUNTRY == "PARAGUAY" & DIA %in% 1:3 ~ NA_real_,
#     TRUE ~ IP_mov_avg),
#     R0_MA = R0(CONTAGIADOS_MA, 14))

# EDA
# covid_tbl %>% 
#   filter(COUNTRY == "BOLIVIA") %>% 
#   mutate(ola = ifelse(IP_mov_avg > 2000 & DIA < 40, '1', '0')) %>% 
#   ggplot(aes(DIA, IP_mov_avg)) +
#   geom_point() 
# 
# covid %>% 
#   filter(COUNTRY == "CHILE") %>% 
#   mutate(ola = ifelse(IP_mov_avg > 250, '1', '0')) %>% 
#   ggplot(aes(DIA, IP_mov_avg, col = ola)) +
#   geom_point() 

# covid %>% 
#   filter(COUNTRY == "ECUADOR") %>% 
#   mutate(ola = ifelse(IP_mov_avg > 2000 & DIA < 40, '1', '0')) %>% 
#   ggplot(aes(DIA, IP_mov_avg, col = ola)) +
#   geom_point() 

# covid %>% 
#   filter(COUNTRY == "EL SALVADOR") %>% 
#   mutate(ola = ifelse(IP_mov_avg > 250, '1', '0')) %>% 
#   ggplot(aes(DIA, IP_mov_avg, col = ola)) +
#   geom_point() 
# 
# covid %>% 
#   filter(COUNTRY == "GUATEMALA") %>% 
#   mutate(ola = ifelse(IP_mov_avg > 2000 & DIA < 40, '1', '0')) %>% 
#   ggplot(aes(DIA, IP_mov_avg, col = ola)) +
#   geom_point() 
# 
# covid %>% 
#   filter(COUNTRY == "MEXICO") %>% 
#   mutate(ola = ifelse(IP_mov_avg > 250, '1', '0')) %>% 
#   ggplot(aes(DIA, IP_mov_avg, col = ola)) +
#   geom_point() 
# 
# covid_tbl %>% 
#   filter(COUNTRY == "PERU") %>% 
#   mutate(ola = ifelse(IP_mov_avg > 2000 & DIA < 40, '1', '0')) %>% 
#   ggplot(aes(DIA, IP_mov_avg, col = ola)) +
#   geom_point() 
# 
# covid %>% 
#   filter(COUNTRY == "PUERTO RICO") %>% 
#   mutate(ola = ifelse(IP_mov_avg > 250, '1', '0')) %>% 
#   ggplot(aes(DIA, IP_mov_avg, col = ola)) +
#   geom_point() 
# 
# covid %>% 
#   filter(COUNTRY == "SPAIN") %>% 
#   mutate(ola = ifelse(IP_mov_avg > 2000 & DIA < 40, '1', '0')) %>% 
#   ggplot(aes(DIA, IP_mov_avg, col = ola)) +
#   geom_point() 
# 
# covid %>% 
#   filter(COUNTRY == "URUGUAY") %>% 
#   mutate(ola = ifelse(IP_mov_avg > 250, '1', '0')) %>% 
#   ggplot(aes(DIA, IP_mov_avg, col = ola)) +
#   geom_point() 
# 
# covid %>% 
#   filter(COUNTRY == "NETHERLAND") %>% 
#   mutate(ola = ifelse(IP_mov_avg > 250, '1', '0')) %>% 
#   ggplot(aes(DIA, IP_mov_avg, col = ola)) +
#   geom_point() 
# 
# covid %>% 
#   filter(COUNTRY == "COLOMBIA") %>% 
#   mutate(ola = ifelse(IP_mov_avg > 2000 & DIA < 40, '1', '0')) %>% 
#   ggplot(aes(DIA, IP_mov_avg, col = ola)) +
#   geom_point() 
# 
# covid %>% 
#   filter(COUNTRY == "BRAZIL") %>% 
#   mutate(ola = ifelse(IP_mov_avg > 250, '1', '0')) %>% 
#   ggplot(aes(DIA, IP_mov_avg, col = ola)) +
#   geom_point() 
# 
# covid %>% 
#   filter(COUNTRY == "PARAGUAY") %>% 
#   mutate(ola = ifelse(IP_mov_avg > 250, '1', '0')) %>% 
#   ggplot(aes(DIA, IP_mov_avg, col = ola)) +
#   geom_point() 

# all important variables
options(scipen = 999)

covid_tbl %>% 
  mutate(CONTAGIADOS_MA_MILL = CONTAGIADOS_MA/POP * 1000000) %>% 
  ggplot(aes(DIA, CONTAGIADOS_MA_MILL)) + # infectados x millon
  # geom_point(size = .5, alpha = .9) +
  geom_line(aes(col = COUNTRY), size = 1.5) + 
  facet_wrap(~COUNTRY, nrow = 2) +
  theme_bw() +
  theme(legend.position = "none")

covid_tbl %>% 
  mutate(FALLECIDOS_MA_MILL = FALLECIDOS_MA/POP * 1000000) %>% 
  # filter(COUNTRY != 'BOLIVIA') %>% 
  ggplot(aes(DIA, FALLECIDOS_MA_MILL)) + # infectados x millon
  # geom_point(size = .5, alpha = .9) +
  geom_line(aes(col = COUNTRY), size = 1.5) + 
  facet_wrap(~COUNTRY, nrow = 2) +
  theme_bw() +
  theme(legend.position = "none")

covid_tbl %>% 
  mutate(IP_pop = IP/POP) %>% 
  ggplot(aes(DIA, IP_pop)) +
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


