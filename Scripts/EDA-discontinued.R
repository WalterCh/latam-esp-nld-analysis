## Countries Analysis
## Walter Chanava
## Jul 2020


# Packages ----------------------------------------------------------------

library(readr)
library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------

covid <- read_csv2("./Data/VariosPaises_Covid19.csv")
covid_tbl <- covid %>% 
  filter(COUNTRY %in% c("PERU", "PANAMA", "MEXICO", "GUATEMALA", 
                        "ELSALVADOR")) %>% 
  mutate(country = as.factor(COUNTRY),
         date = as.Date(FECHA, "%d/%m/%y"),
         cases = as.numeric(CONTAGIADOS),
         deaths = FALLECIDOS,
         recov = as.numeric(RECUPERADOS),
         hosps = as.numeric(HOSPITALIZADOS),
         ucis = as.numeric(UCIs),
         danger_idx = cases + deaths - recov) %>% 
  select(country, date, cases, deaths, recov, hosps, ucis, danger_idx)


# Plots -------------------------------------------------------------------

source("./Scripts/custom_ggplot2_theme.R") # comment this line to avoid problems

# Facet plot by country
deaths_plot <- ggplot(data = covid_tbl, 
                     aes(x = date, y = deaths, color = country)) +
  geom_point() +
  facet_wrap(~country, nrow = 3) +
  theme(legend.position = 'none') +
  custom_theme # comment this line to avoid 'fonts' problems

ggsave(filename = "./Outputs/1.Deaths_by_day.png", plot = deaths_plot,
       width = 6, height = 6)

# Danger index by country
danger_idx_plot <- ggplot(data = covid_tbl, 
                          aes(x = date, y = danger_idx, color = country)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~country, nrow = 3) +
  theme(legend.position = "none") +
  labs(x = "Date", y = "Danger index") +
  custom_theme # comment this line to avoid 'fonts' problems

ggsave(filename = "./Outputs/2.Danger-idx-plot.png", plot = danger_idx_plot,
       width = 6, height = 6)
