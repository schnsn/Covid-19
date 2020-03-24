library(readxl)
library(httr)
library(tidyverse)
library(cowplot)

#url <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-03-23.xlsx"
url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")
GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
data <- read_excel(tf)
Population <- read_csv("population-figures-by-country.csv")

Countries <- c("AT","DE","ES","FR","IT","UK","US")
PopCountries <- c("AUT","DEU","ESP","FRA","ITA","GBR","USA") # WHY HAVE STANDARDS?!

Population <- Population %>%
  filter (Country_Code %in% PopCountries) %>%
  select(Country_Code, ends_with("2016")) %>%
  mutate(Country_Code = countries) %>%
  rename(GeoId = Country_Code, Population100k = Year_2016) %>%
  mutate(Population100k = Population100k/100000)

DataSelected <- data %>%
  arrange(DateRep) %>%
  filter (Month >= "3") %>%
  filter (GeoId %in% countries) %>%
  inner_join(Population) %>%
  group_by(GeoId) %>%
  mutate(CumCases = cumsum(Cases)) %>%
  mutate(CumDeaths = cumsum(Deaths)) %>%
  mutate(Active = Cases - Deaths) %>%
  mutate(CumActive = CumCases - CumDeaths) %>%
  mutate(Mortality_naive = CumDeaths/CumCases) %>%
  mutate(CumCasesPer100k = CumCases/Population100k) %>%
  mutate(CumDeathsPer100k = CumDeaths/Population100k)


CasesByCountry <- ggplot(data = DataSelected, aes(x = DateRep)) +
  geom_line(aes (y = Cases, color = GeoId)) +
  labs(title = "Daily Cases", x = NULL, y = NULL) +
  theme(legend.position = "none", axis.text.x = element_blank())

CumCasesByCountry <- ggplot(data = DataSelected, aes(x = DateRep)) +
  geom_line(aes (y = CumCases, color = GeoId)) +
  labs(title = "Cumulative Cases", x = NULL, y = NULL) +
  theme(legend.position = "none", axis.text.x = element_blank())

CumCasesPerPop <- ggplot(data = DataSelected, aes(x = DateRep)) +
  geom_line(aes (y = CumCasesPer100k, color = GeoId)) +
  labs(title = "Cumulative Cases per 100k Inhabitants", x = NULL, y = NULL) +
  labs(colour = "Countries", axis.text.x = element_blank())

DeathsByCountry <- ggplot(data = DataSelected, aes(x = DateRep)) +
  geom_line(aes (y = Deaths, color = GeoId)) +
  labs(title = "Daily Deaths", x = NULL, y = NULL) +
  theme(legend.position = "none")

CumDeathsByCountry <- ggplot(data = DataSelected, aes(x = DateRep)) +
  geom_line(aes (y = CumDeaths, color = GeoId)) +
  labs(title = "Cumulative Deaths", x = NULL, y = NULL) +
  theme(legend.position = "none")

CumDeathsPerPop <- ggplot(data = DataSelected, aes(x = DateRep)) +
  geom_line(aes (y = CumDeathsPer100k, color = GeoId)) +
  labs(title = "Cumulative Deaths per 100k Inhabitants", x = NULL, y = NULL) +
  labs(colour = "Countries", caption = "(based on data from www.ecdc.europa.eu)")

plot_grid(CasesByCountry, CumCasesByCountry, CumCasesPerPop,
          DeathsByCountry, CumDeathsByCountry, CumDeathsPerPop,
          labels = "AUTO")


# Plots in log Scale:

# CasesByCountry <- ggplot(data = DataSelected, aes(x = DateRep)) +
#   geom_line(aes (y = Cases, color = GeoId)) +
#   scale_y_log10() +
#   labs(title = "Daily Cases (log Scale)", x = NULL, y = NULL) +
#   theme(legend.position = "none", axis.text.x = element_blank())
#
# CumCasesByCountry <- ggplot(data = DataSelected, aes(x = DateRep)) +
#   geom_line(aes (y = CumCases, color = GeoId)) +
#   scale_y_log10() +
#   labs(title = "Cumulative Cases (log Scale)", x = NULL, y = NULL) +
#   theme(legend.position = "none", axis.text.x = element_blank())
#
# CumCasesPerPop <- ggplot(data = DataSelected, aes(x = DateRep)) +
#   geom_line(aes (y = CumCasesPer100k, color = GeoId)) +
#   scale_y_log10() +
#   labs(title = "Cumulative Cases per 100k Inhabitants (log Scale)", x = NULL, y = NULL) +
#   labs(colour = "Countries", axis.text.x = element_blank())
#
# DeathsByCountry <- ggplot(data = DataSelected, aes(x = DateRep)) +
#   geom_line(aes (y = Deaths, color = GeoId)) +
#   scale_y_log10() +
#   labs(title = "Daily Deaths (log Scale)", x = NULL, y = NULL) +
#   theme(legend.position = "none")
#
# CumDeathsByCountry <- ggplot(data = DataSelected, aes(x = DateRep)) +
#   geom_line(aes (y = CumDeaths, color = GeoId)) +
#   scale_y_log10() +
#   labs(title = "Cumulative Deaths (log Scale)", x = NULL, y = NULL) +
#   theme(legend.position = "none")
#
# CumDeathsPerPop <- ggplot(data = DataSelected, aes(x = DateRep)) +
#   geom_line(aes (y = CumDeathsPer100k, color = GeoId)) +
#   scale_y_log10() +
#   labs(title = "Cumulative Deaths per 100k Inhabitants (log Scale)", x = NULL, y = NULL) +
#   labs(colour = "Countries", caption = "(based on data from www.ecdc.europa.eu)")
#
# plot_grid(CasesByCountry, CumCasesByCountry, CumCasesPerPop,
#           DeathsByCountry, CumDeathsByCountry, CumDeathsPerPop,
#           labels = "AUTO")

