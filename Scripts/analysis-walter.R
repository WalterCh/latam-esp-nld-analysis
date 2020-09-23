## Covid-19 R0 Analysis
## Walter Chanava
## Aug 2020

# Packages ----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggrepel)
library(readr)
library(tidyr)

source("./Scripts/R0-function.R")
source("./Scripts/custom-ggplot2-theme.R")
source("./Scripts/Moving-average-function.R")

# Data --------------------------------------------------------------------

covid <- read_csv("./Data/VariosPaises_Covid19Actualizado.csv", na = "#VALUE!")

country_pops <- tibble(
  COUNTRY = unique(covid$COUNTRY),
  POP = c(11673021, 212559417, 212559417, 50882891, 17643054,
          6486205, 17915568, 128932753, 17134872, 7132538,
          32971854, 2860853, 46754778, 3473730)
)

covid_tbl <- covid %>% 
  left_join(country_pops) %>%
  mutate(CONTAGIADOS_POND = CONTAGIADOS/POP,
         CONTAGIADOS_MILL = CONTAGIADOS/1000000) %>%
  group_by(COUNTRY) %>% 
  mutate(CONTAGIADOS_MA = mov_avg(CONTAGIADOS, 7),
         FALLECIDOS_MA = mov_avg(FALLECIDOS, 7),
         RECUPERADOS = mov_avg(RECUPERADOS, 7)) %>% 
  mutate(DIA = 1:n()) %>% 
  ungroup() %>% 
  mutate(COUNTRY = as.factor(COUNTRY),
         FECHA = as.Date(FECHA, "%d/%m/%Y"),
         SEMANA = as.numeric(strftime(FECHA, format = "%V")),
         IP = CONTAGIADOS + FALLECIDOS - RECUPERADOS,
         IP_MA = CONTAGIADOS_MA + FALLECIDOS_MA - RECUPERADOS_MA,
         R0_3 = R0(CONTAGIADOS, 3),
         R0_7 = R0(CONTAGIADOS, 7),
         R0_14 = R0(CONTAGIADOS, 14),
         R0_14_MA = R0(CONTAGIADOS_MA, 14))


# EDA ---------------------------------------------------------------------


# Contagiados -------------------------------------------------------------

# Contagiados (hacer Y como Contagiados por millon)
# 1
ggplot(data = covid_tbl %>% 
         filter(COUNTRY %in% c("URUGUAY")), 
       aes(x = FECHA, y = CONTAGIADOS)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-04-01"), 
                                 to = as.Date("2020-08-01"), 
                                 by = "month")) +
  facet_wrap(~COUNTRY) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/grupo1.png", plot = last_plot(), width = 5, height = 4)

# 2
ggplot(data = covid_tbl %>% 
         filter(COUNTRY %in% c("EL SALVADOR", "PUERTO RICO", "PARAGUAY")), 
       aes(x = FECHA, y = CONTAGIADOS)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-04-01"), 
                                 to = as.Date("2020-08-01"), 
                                 by = "month")) +
  facet_wrap(~COUNTRY, nrow = 2) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/grupo2.png", plot = last_plot(), width = 10, height = 7)

# 3
ggplot(data = covid_tbl %>% 
         filter(COUNTRY %in% c("GUATEMALA", "NETHERLAND", "BOLIVIA",
                               "ECUADOR")), 
       aes(x = FECHA, y = CONTAGIADOS)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-04-01"), 
                                 to = as.Date("2020-08-01"), 
                                 by = "month")) +
  facet_wrap(~COUNTRY) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/grupo3.png", plot = last_plot(), width = 10, height = 7)

# 4
ggplot(data = covid_tbl %>% 
         filter(COUNTRY %in% c("CHILE", "SPAIN", "MEXICO", "COLOMBIA",
                               "PERU")), 
       aes(x = FECHA, y = CONTAGIADOS)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-04-01"), 
                                 to = as.Date("2020-08-01"), 
                                 by = "month")) +
  facet_wrap(~COUNTRY) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/grupo4.png", plot = last_plot(), width = 15, height = 7)

# 5
ggplot(data = covid_tbl %>% 
         filter(COUNTRY %in% c("BRAZIL")), 
       aes(x = FECHA, y = CONTAGIADOS)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-03-01"), 
                                 to = as.Date("2020-09-01"), 
                                 by = "month")) +
  facet_wrap(~COUNTRY) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/grupo5.png", plot = last_plot(), width = 5, height = 4)

# Contagiados ponderados por cantidad de habitantes
ggplot(data = covid_tbl, aes(x = FECHA, y = CONTAGIADOS_POND)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-03-01"), 
                                 to = as.Date("2020-09-01"), 
                                 by = "month")) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~COUNTRY) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/Confirmados/Facet/Ponderados-cantidad-habitantes.png",
       plot = last_plot(), width = 14, height = 8)

ggplot(data = covid_tbl, aes(x = FECHA, y = CONTAGIADOS)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-03-01"), 
                                 to = as.Date("2020-09-01"), 
                                 by = "month")) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~COUNTRY) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/Confirmados/Facet/No-Ponderados.png",
       plot = last_plot(), width = 14, height = 8)

# All-Together (daily)
ggplot(data = covid_tbl, 
       aes(x = FECHA, y = CONTAGIADOS, col = COUNTRY)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  geom_label_repel(data = . %>% filter(DIA == last(DIA),
                                       COUNTRY %in% c("BRAZIL", "PERU", 
                                                      "MEXICO", "COLOMBIA")), 
                   aes(label = COUNTRY), family = "Poppins", 
                   xlim = as.Date("2020-09-05"), ylim = c(3000, 40000)) +
  geom_label(aes(label = "*"), family = "Poppins", x = as.Date("2020-09-28"),
             y = 1500, col = "black") +
  scale_x_date(date_labels = "%b",
               limits = as.Date(c("2020-03-01", "2020-10-22")),
               breaks = seq.Date(from = as.Date("2020-03-01"),
                                 to = as.Date("2020-10-01"),
                                 by = "month")) +
  custom_theme 
  # labs(y = "CONTAGIADOS", caption = "* Países con número promedio de contagiados por semana menor a 5000: Chile,\nBolivia, Ecuador, Guatemala, Paraguay, España, Puerto Rico, El Salvador, Países Bajos y\nUruguay")
ggsave("./Outputs/Confirmados/Facet/Juntos x semana.png", plot = last_plot(),
       width = 5.5, height = 4.5)


# All-Together (week)
ggplot(data = covid_tbl %>% 
         group_by(COUNTRY, SEMANA) %>% 
         summarise(MEDIA = mean(CONTAGIADOS)) %>% 
         mutate(FECHA = as.Date("2020-01-01") + 7*SEMANA), 
       aes(x = FECHA, y = MEDIA, col = COUNTRY)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  geom_label_repel(data = . %>% filter(SEMANA == last(SEMANA),
                                       COUNTRY %in% c("BRAZIL", "PERU", 
                                                      "MEXICO", "COLOMBIA")), 
                   aes(label = COUNTRY), family = "Poppins", 
                   xlim = as.Date("2020-09-05"), ylim = c(3000, 40000)) +
  geom_label(aes(label = "*"), family = "Poppins", x = as.Date("2020-09-28"),
             y = 1500, col = "black") +
  scale_x_date(date_labels = "%b",
               limits = as.Date(c("2020-03-01", "2020-10-22")),
               breaks = seq.Date(from = as.Date("2020-03-01"),
                                 to = as.Date("2020-10-01"),
                                 by = "month")) +
  custom_theme +
  labs(y = "CONTAGIADOS", caption = "* Países con número promedio de contagiados por semana menor a 5000: Chile,\nBolivia, Ecuador, Guatemala, Paraguay, España, Puerto Rico, El Salvador, Países Bajos y\nUruguay")
ggsave("./Outputs/Confirmados/Facet/Juntos x semana.png", plot = last_plot(),
       width = 5.5, height = 4.5)

# All-Together (moving average by week)
covid_tbl %>% 
  select(COUNTRY, FECHA, CONTAGIADOS)

## Contagiados por millar de habitantes
# 1
ggplot(data = covid_tbl %>% 
         filter(COUNTRY %in% c("URUGUAY")), 
       aes(x = FECHA, y = CONTAGIADOS/1000)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-04-01"), 
                                 to = as.Date("2020-08-01"), 
                                 by = "month")) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~COUNTRY) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/Confirmados/Casos x mil/grupo1.png", plot = last_plot(),
       width = 5, height = 4)

# 2
ggplot(data = covid_tbl %>% 
         filter(COUNTRY %in% c("EL SALVADOR", "PUERTO RICO", "PARAGUAY")), 
       aes(x = FECHA, y = CONTAGIADOS/1000)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-04-01"), 
                                 to = as.Date("2020-08-01"), 
                                 by = "month")) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~COUNTRY, nrow = 2) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/Confirmados/Casos x mil/grupo2.png", plot = last_plot(), 
       width = 10, height = 7)

# 3
ggplot(data = covid_tbl %>% 
         filter(COUNTRY %in% c("GUATEMALA", "NETHERLAND", "BOLIVIA",
                               "ECUADOR")), 
       aes(x = FECHA, y = CONTAGIADOS/1000)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-04-01"), 
                                 to = as.Date("2020-08-01"), 
                                 by = "month")) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~COUNTRY) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/Confirmados/Casos x mil/grupo3.png", plot = last_plot(), 
       width = 10, height = 7)

# 4
ggplot(data = covid_tbl %>% 
         filter(COUNTRY %in% c("CHILE", "SPAIN", "MEXICO", "COLOMBIA",
                               "PERU")), 
       aes(x = FECHA, y = CONTAGIADOS/1000)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-04-01"), 
                                 to = as.Date("2020-08-01"), 
                                 by = "month")) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~COUNTRY) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/Confirmados/Casos x mil/grupo4.png", plot = last_plot(), 
       width = 15, height = 7)

# 5
ggplot(data = covid_tbl %>% 
         filter(COUNTRY %in% c("BRAZIL")), 
       aes(x = FECHA, y = CONTAGIADOS/1000)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-03-01"), 
                                 to = as.Date("2020-09-01"), 
                                 by = "month")) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~COUNTRY) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/Confirmados/Casos x mil/grupo5.png", plot = last_plot(), 
       width = 5, height = 4)

## Casos por mil facet
ggplot(data = covid_tbl, 
       aes(x = FECHA, y = CONTAGIADOS/1000)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-03-01"), 
                                 to = as.Date("2020-09-01"), 
                                 by = "month")) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~COUNTRY) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/Confirmados/Facet/Contagiados-mil-facet.png",
       plot = last_plot(), width = 14, height = 8)



# Fallecidos --------------------------------------------------------------

## Fallecidos por millar de habitantes
# 1
ggplot(data = covid_tbl %>% 
         filter(COUNTRY %in% c("URUGUAY")), 
       aes(x = FECHA, y = FALLECIDOS)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-04-01"), 
                                 to = as.Date("2020-08-01"), 
                                 by = "month")) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~COUNTRY) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/Fallecidos/Casos/grupo1.png", plot = last_plot(),
       width = 5, height = 4)

# 2
ggplot(data = covid_tbl %>% 
         filter(COUNTRY %in% c("EL SALVADOR", "PUERTO RICO", "PARAGUAY")), 
       aes(x = FECHA, y = FALLECIDOS)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-04-01"), 
                                 to = as.Date("2020-08-01"), 
                                 by = "month")) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~COUNTRY, nrow = 2) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/Fallecidos/Casos/grupo2.png", plot = last_plot(), 
       width = 10, height = 7)

# 3
ggplot(data = covid_tbl %>% 
         filter(COUNTRY %in% c("GUATEMALA", "NETHERLAND", "BOLIVIA",
                               "ECUADOR")), 
       aes(x = FECHA, y = FALLECIDOS)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-04-01"), 
                                 to = as.Date("2020-08-01"), 
                                 by = "month")) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~COUNTRY) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/Fallecidos/Casos/grupo3.png", plot = last_plot(), 
       width = 10, height = 7)

# 4
ggplot(data = covid_tbl %>% 
         filter(COUNTRY %in% c("CHILE", "SPAIN", "MEXICO", "COLOMBIA",
                               "PERU")), 
       aes(x = FECHA, y = FALLECIDOS)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-04-01"), 
                                 to = as.Date("2020-08-01"), 
                                 by = "month")) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~COUNTRY) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/Fallecidos/Casos/grupo4.png", plot = last_plot(), 
       width = 15, height = 7)

# 5
ggplot(data = covid_tbl %>% 
         filter(COUNTRY %in% c("BRAZIL")), 
       aes(x = FECHA, y = FALLECIDOS)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-03-01"), 
                                 to = as.Date("2020-09-01"), 
                                 by = "month")) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~COUNTRY) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/Fallecidos/Casos/grupo5.png", plot = last_plot(), 
       width = 5, height = 4)

## Casos por mil facet
ggplot(data = covid_tbl, 
       aes(x = FECHA, y = FALLECIDOS)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-03-01"), 
                                 to = as.Date("2020-09-01"), 
                                 by = "month")) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~COUNTRY) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/Fallecidos/Facet/Fallecidos-facet.png",
       plot = last_plot(), width = 14, height = 8)

ggplot(data = covid_tbl, 
       aes(x = FECHA, y = FALLECIDOS/POP)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-03-01"), 
                                 to = as.Date("2020-09-01"), 
                                 by = "month")) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~COUNTRY) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/Fallecidos/Facet/Fallecidos-habitantes-facet.png",
       plot = last_plot(), width = 14, height = 8)


# Hospitalizados
# ggplot(data = covid_tbl, aes(x = FECHA, y = RECUPERADOS)) +
#   geom_point() +
#   geom_line() +
#   facet_wrap(~COUNTRY) +
#   custom_theme

# UCIs
# ggplot(data = covid_tbl, aes(x = FECHA, y = UCIs)) +
#   geom_point() +
#   geom_line() +
#   facet_wrap(~COUNTRY) +
#   custom_theme

# IP
ggplot(data = covid_tbl, 
       aes(x = FECHA, y = IP)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~COUNTRY) +
  custom_theme
ggsave("./Outputs/IP/Facet/Todos.png", plot = last_plot(),
       width = 5, height = 4)

ggplot(data = covid_tbl %>% 
         filter(COUNTRY != "PERU"), 
       aes(x = FECHA, y = IP)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~COUNTRY) +
  custom_theme
ggsave("./Outputs/IP/Facet/Sin-Peru.png", plot = last_plot(),
       width = 5, height = 4)

# 1
ggplot(data = covid_tbl %>% 
         filter(COUNTRY %in% c("URUGUAY")), 
       aes(x = FECHA, y = IP)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-04-01"), 
                                 to = as.Date("2020-08-01"), 
                                 by = "month")) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~COUNTRY) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/IP/Casos/grupo1.png", plot = last_plot(),
       width = 5, height = 4)

# 2
ggplot(data = covid_tbl %>% 
         filter(COUNTRY %in% c("EL SALVADOR", "PUERTO RICO", "PARAGUAY")), 
       aes(x = FECHA, y = IP)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-04-01"), 
                                 to = as.Date("2020-08-01"), 
                                 by = "month")) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~COUNTRY, nrow = 2) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/IP/Casos/grupo2.png", plot = last_plot(), 
       width = 10, height = 7)

# 3
ggplot(data = covid_tbl %>% 
         filter(COUNTRY %in% c("GUATEMALA", "NETHERLAND", "BOLIVIA",
                               "ECUADOR")), 
       aes(x = FECHA, y = IP)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-04-01"), 
                                 to = as.Date("2020-08-01"), 
                                 by = "month")) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~COUNTRY) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/IP/Casos/grupo3.png", plot = last_plot(), 
       width = 10, height = 7)

# 4
ggplot(data = covid_tbl %>% 
         filter(COUNTRY %in% c("CHILE", "SPAIN", "MEXICO", "COLOMBIA",
                               "PERU")), 
       aes(x = FECHA, y = IP)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-04-01"), 
                                 to = as.Date("2020-08-01"), 
                                 by = "month")) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~COUNTRY) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/IP/Casos/grupo4.png", plot = last_plot(), 
       width = 15, height = 7)

# 5
ggplot(data = covid_tbl %>% 
         filter(COUNTRY %in% c("BRAZIL")), 
       aes(x = FECHA, y = FALLECIDOS)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-03-01"), 
                                 to = as.Date("2020-09-01"), 
                                 by = "month")) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~COUNTRY) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/IP/Casos/grupo5.png", plot = last_plot(), 
       width = 5, height = 4)

# Corregidos

# 1
ggplot(data = covid_tbl %>% 
         filter(COUNTRY %in% c("URUGUAY")), 
       aes(x = FECHA, y = IP)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-04-01"), 
                                 to = as.Date("2020-08-01"), 
                                 by = "month")) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~COUNTRY) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/IP/Casos/Corregidos/grupo1.png", plot = last_plot(),
       width = 5, height = 4)

# 2
ggplot(data = covid_tbl %>% 
         filter(COUNTRY %in% c("EL SALVADOR", "PUERTO RICO", "PARAGUAY")), 
       aes(x = FECHA, y = IP)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-04-01"), 
                                 to = as.Date("2020-08-01"), 
                                 by = "month")) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~COUNTRY, nrow = 2) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/IP/Casos/Corregidos/grupo2.png", plot = last_plot(), 
       width = 10, height = 7)

# 3
ggplot(data = covid_tbl %>% 
         filter(COUNTRY %in% c("GUATEMALA", "NETHERLAND", "BOLIVIA",
                               "ECUADOR")), 
       aes(x = FECHA, y = IP)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-04-01"), 
                                 to = as.Date("2020-08-01"), 
                                 by = "month")) +
  scale_y_continuous(labels = scales::comma, limits = c(-800, 1800)) +
  facet_wrap(~COUNTRY) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/IP/Casos/Corregidos/grupo3.png", plot = last_plot(), 
       width = 10, height = 7)

# 4
ggplot(data = covid_tbl %>% 
         filter(COUNTRY %in% c("CHILE", "SPAIN", "MEXICO", "COLOMBIA",
                               "PERU")), 
       aes(x = FECHA, y = IP)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-04-01"), 
                                 to = as.Date("2020-08-01"), 
                                 by = "month")) +
  scale_y_continuous(labels = scales::comma, limits = c(-2000, 8200)) +
  facet_wrap(~COUNTRY) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/IP/Casos/Corregidos/grupo4.png", plot = last_plot(), 
       width = 15, height = 7)

# 5
ggplot(data = covid_tbl %>% 
         filter(COUNTRY %in% c("BRAZIL")), 
       aes(x = FECHA, y = FALLECIDOS)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_x_date(date_labels = "%b",
               breaks = seq.Date(from = as.Date("2020-03-01"), 
                                 to = as.Date("2020-09-01"), 
                                 by = "month")) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~COUNTRY) +
  custom_theme +
  theme(strip.text = element_text(size = 15))
ggsave("./Outputs/IP/Casos/Corregidos/grupo5.png", plot = last_plot(), 
       width = 5, height = 4)

# R0 

# 1
ggplot(data = covid_tbl %>% 
         select(COUNTRY, FECHA, R0_7, R0_14) %>% 
         filter(COUNTRY == "URUGUAY") %>% 
         gather("R0", "value", -COUNTRY, -FECHA),
       aes(x = FECHA, y = value, color = R0)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~COUNTRY) +
  scale_x_date(limit = c(as.Date("2020-03-25"), as.Date("2020-08-23"))) +
  scale_y_continuous(limit = c(0, 25)) +
  labs(y = "R0") +
  custom_theme +
  theme(legend.position = "right",
        legend.title = element_blank(),
        strip.text = element_text(size = 15))
ggsave("./Outputs/R0/Casos/grupo1.png", plot = last_plot(), 
       width = 5.5, height = 4)

# 2
ggplot(data = covid_tbl %>% 
         select(COUNTRY, FECHA, R0_7, R0_14) %>% 
         filter(COUNTRY %in% c("EL SALVADOR", "PUERTO RICO", "PARAGUAY")) %>% 
         gather("R0", "value", -COUNTRY, -FECHA),
       aes(x = FECHA, y = value, color = R0)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~COUNTRY) +
  scale_x_date(limit = c(as.Date("2020-03-25"), as.Date("2020-08-23"))) +
  scale_y_continuous(limit = c(0, 25)) +
  labs(y = "R0") +
  custom_theme +
  theme(legend.position = "right",
        legend.title = element_blank(),
        strip.text = element_text(size = 15))
ggsave("./Outputs/R0/Casos/grupo2.png", plot = last_plot(), 
       width = 10.5, height = 4)

# 3
ggplot(data = covid_tbl %>% 
         select(COUNTRY, FECHA, R0_7, R0_14) %>% 
         filter(COUNTRY %in% c("GUATEMALA", "NETHERLAND", "BOLIVIA",
                               "ECUADOR")) %>% 
         gather("R0", "value", -COUNTRY, -FECHA),
       aes(x = FECHA, y = value, color = R0)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~COUNTRY) +
  scale_x_date(limit = c(as.Date("2020-03-25"), as.Date("2020-08-23"))) +
  scale_y_continuous(limit = c(0, 25)) +
  labs(y = "R0") +
  custom_theme +
  theme(legend.position = "right",
        legend.title = element_blank(),
        strip.text = element_text(size = 15))
ggsave("./Outputs/R0/Casos/grupo3.png", plot = last_plot(), 
       width = 10.5, height = 6)

# 4
ggplot(data = covid_tbl %>% 
         select(COUNTRY, FECHA, R0_7, R0_14) %>% 
         filter(COUNTRY %in% c("CHILE", "SPAIN", "MEXICO", "COLOMBIA",
                               "PERU")) %>% 
         gather("R0", "value", -COUNTRY, -FECHA),
       aes(x = FECHA, y = value, color = R0)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~COUNTRY) +
  scale_x_date(limit = c(as.Date("2020-03-25"), as.Date("2020-08-23"))) +
  scale_y_continuous(limit = c(0, 25)) +
  labs(y = "R0") +
  custom_theme +
  theme(legend.position = "right",
        legend.title = element_blank(),
        strip.text = element_text(size = 15))
ggsave("./Outputs/R0/Casos/grupo4.png", plot = last_plot(), 
       width = 12.5, height = 6)

# 5
ggplot(data = covid_tbl %>% 
         select(COUNTRY, FECHA, R0_7, R0_14) %>% 
         filter(COUNTRY %in% c("BRAZIL")) %>% 
         gather("R0", "value", -COUNTRY, -FECHA),
       aes(x = FECHA, y = value, color = R0)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~COUNTRY) +
  scale_x_date(limit = c(as.Date("2020-03-25"), as.Date("2020-08-23"))) +
  scale_y_continuous(limit = c(0, 25)) +
  labs(y = "R0") +
  custom_theme +
  theme(legend.position = "right",
        legend.title = element_blank(),
        strip.text = element_text(size = 15))
ggsave("./Outputs/R0/Casos/grupo5.png", plot = last_plot(), 
       width = 5.5, height = 4)
