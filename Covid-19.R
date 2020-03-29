library(httr)
library(tidyverse)
library(cowplot)
#library(directlabels)

GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"),
    write_disk(tf <- tempfile(fileext = ".csv")))

data <- read_csv(tf,
                 col_types = cols(cases = col_integer(),
                 dateRep = col_date(format = "%d/%m/%Y"),
                 day = col_integer(), deaths = col_integer(),
                 month = col_integer(), year = col_integer()))

Countries <- c("AT","DE","ES","FR","IT","UK","US")

DataSelected <- data %>%
  arrange(dateRep) %>%
  filter (month >= "3") %>%
  filter (geoId %in% Countries) %>%
  group_by(geoId) %>%
  mutate(Population100k = popData2018/100000) %>%
  mutate(CumCases = cumsum(cases)) %>%
  mutate(CumDeaths = cumsum(deaths)) %>%
  mutate(Active = cases - deaths) %>%
  mutate(CumActive = CumCases - CumDeaths) %>%
  mutate(Mortality_naive = CumDeaths/CumCases) %>%
  mutate(CumCasesPer100k = CumCases/Population100k) %>%
  mutate(CumDeathsPer100k = CumDeaths/Population100k)

CasesByCountry <- ggplot(data = DataSelected, aes(x = dateRep)) +
  geom_line(aes (y = cases, color = geoId)) +
  labs(title = "Daily cases", x = NULL, y = NULL) +
  labs(colour = "Countries") +
  theme(legend.justification=c(0,1), legend.position=c(0.01,0.99),
        axis.text.x = element_blank())

CumCasesByCountry <- ggplot(data = DataSelected, aes(x = dateRep)) +
  geom_line(aes (y = CumCases, color = geoId)) +
  scale_y_log10() +
 # geom_dl(aes(y=CumCases,label=geoId),method="top.qp") +
  labs(title = "Cumulative cases (log)", x = NULL, y = NULL) +
  theme(legend.position = "none", axis.text.x = element_blank())

CumCasesPerPop <- ggplot(data = DataSelected, aes(x = dateRep)) +
  geom_line(aes (y = CumCasesPer100k, color = geoId)) +
  labs(title = "Cumulative cases per 100k Inhabitants", x = NULL, y = NULL) +
  theme(legend.position = "none", axis.text.x = element_blank())

DeathsByCountry <- ggplot(data = DataSelected, aes(x = dateRep)) +
  geom_line(aes (y = deaths, color = geoId)) +
  labs(title = "Daily deaths", x = NULL, y = NULL) +
  theme(legend.position = "none")

CumDeathsByCountry <- ggplot(data = DataSelected, aes(x = dateRep)) +
  geom_line(aes (y = CumDeaths, color = geoId)) +
  scale_y_log10() +
  labs(title = "Cumulative deaths (log)", x = NULL, y = NULL) +
  theme(legend.position = "none")

CumDeathsPerPop <- ggplot(data = DataSelected, aes(x = dateRep)) +
  geom_line(aes (y = CumDeathsPer100k, color = geoId)) +
  labs(title = "Cumulative deaths per 100k Inhabitants", x = NULL, y = NULL) +
  theme(legend.position = "none")

plot_grid(CasesByCountry, CumCasesByCountry, CumCasesPerPop,
          DeathsByCountry, CumDeathsByCountry, CumDeathsPerPop,
          labels = "AUTO")


# Plots in log Scale:

# CasesByCountry <- ggplot(data = DataSelected, aes(x = dateRep)) +
#   geom_line(aes (y = cases, color = geoId)) +
#   scale_y_log10() +
#   labs(title = "Daily cases (log Scale)", x = NULL, y = NULL) +
#   theme(legend.position = "none", axis.text.x = element_blank())
#
# CumCasesByCountry <- ggplot(data = DataSelected, aes(x = dateRep)) +
#   geom_line(aes (y = CumCases, color = geoId)) +
#   scale_y_log10() +
#   labs(title = "Cumulative cases (log Scale)", x = NULL, y = NULL) +
#   theme(legend.position = "none", axis.text.x = element_blank())
#
# CumCasesPerPop <- ggplot(data = DataSelected, aes(x = dateRep)) +
#   geom_line(aes (y = CumCasesPer100k, color = geoId)) +
#   scale_y_log10() +
#   labs(title = "Cumulative cases per 100k Inhabitants (log Scale)", x = NULL, y = NULL) +
#   labs(colour = "Countries", axis.text.x = element_blank())
#
# DeathsByCountry <- ggplot(data = DataSelected, aes(x = dateRep)) +
#   geom_line(aes (y = deaths, color = geoId)) +
#   scale_y_log10() +
#   labs(title = "Daily deaths (log Scale)", x = NULL, y = NULL) +
#   theme(legend.position = "none")
#
# CumDeathsByCountry <- ggplot(data = DataSelected, aes(x = dateRep)) +
#   geom_line(aes (y = CumDeaths, color = geoId)) +
#   scale_y_log10() +
#   labs(title = "Cumulative deaths (log Scale)", x = NULL, y = NULL) +
#   theme(legend.position = "none")
#
# CumDeathsPerPop <- ggplot(data = DataSelected, aes(x = dateRep)) +
#   geom_line(aes (y = CumDeathsPer100k, color = geoId)) +
#   scale_y_log10() +
#   labs(title = "Cumulative deaths per 100k Inhabitants (log Scale)", x = NULL, y = NULL) +
#   labs(colour = "Countries", caption = "(based on data from www.ecdc.europa.eu)")
#
# plot_grid(CasesByCountry, CumCasesByCountry, CumCasesPerPop,
#           DeathsByCountry, CumDeathsByCountry, CumDeathsPerPop,
#           labels = "AUTO")

