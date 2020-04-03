library(httr)
library(tidyverse)
library(cowplot)

#Retrieve Data from ECDC
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"),
    write_disk(tf <- tempfile(fileext = ".csv")))

data <- read_csv(tf,
                 col_types = cols(cases = col_integer(),
                                  dateRep = col_date(format = "%d/%m/%Y"),
                                  day = col_integer(), deaths = col_integer(),
                                  month = col_integer(), year = col_integer()))

# Set Countries to include in Plots:
Countries <- c("AT","DE","ES","FR","IT","UK","US")


# Filter data for plotting and create cumulative and per 100k variables:
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


# Create plots:
CasesByCountry <- ggplot(data = DataSelected, aes(x = dateRep)) +
  geom_line(aes (y = cases, color = geoId)) +
  labs(title = "Daily cases", x = NULL, y = NULL) +
  labs(colour = "Countries") +
  scale_x_date(date_breaks = "5 days",
               date_minor_breaks = "1 days") +
  scale_y_continuous(labels = scales::unit_format(
    unit = "k",
    scale = 1e-3)) +
  theme(legend.justification=c(0,1), legend.position=c(0.01,0.99),
        axis.text.x = element_blank())

CumCasesByCountry <- ggplot(data = DataSelected, aes(x = dateRep)) +
  geom_line(aes (y = CumCases, color = geoId)) +
  scale_x_date(date_breaks = "5 days",
               date_minor_breaks = "1 days") +
  scale_y_continuous(labels = scales::unit_format(
    unit = "k",
    scale = 1e-3,
    accuracy = 0.01)) +
  labs(title = "Cumulative cases", x = NULL, y = NULL) +
  theme(legend.position = "none", axis.text.x = element_blank())

CumCasesPerPop <- ggplot(data = DataSelected, aes(x = dateRep)) +
  geom_line(aes (y = CumCasesPer100k, color = geoId)) +
  scale_x_date(date_breaks = "5 days",
               date_minor_breaks = "1 days") +
  scale_y_continuous() +
  labs(title = "Cumulative cases per 100k Inhabitants", x = NULL, y = NULL) +
  theme(legend.position = "none", axis.text.x = element_blank())

DeathsByCountry <- ggplot(data = DataSelected, aes(x = dateRep)) +
  geom_line(aes (y = deaths, color = geoId)) +
  labs(title = "Daily deaths", x = NULL, y = NULL) +
  scale_x_date(date_labels = "%b %d",
               date_breaks = "5 days",
               date_minor_breaks = "1 days") +
  scale_y_continuous() +
  theme(legend.position = "none")

CumDeathsByCountry <- ggplot(data = DataSelected, aes(x = dateRep)) +
  geom_line(aes (y = CumDeaths, color = geoId)) +
  scale_x_date(date_labels = "%b %d",
               date_breaks = "5 days",
               date_minor_breaks = "1 days") +
  scale_y_continuous(labels = scales::unit_format(
    unit = "k",
    scale = 1e-3)) +
  labs(title = "Cumulative deaths", x = NULL, y = NULL) +
  theme(legend.position = "none")

CumDeathsPerPop <- ggplot(data = DataSelected, aes(x = dateRep)) +
  geom_line(aes (y = CumDeathsPer100k, color = geoId)) +
  scale_x_date(date_labels = "%b %d",
               date_breaks = "5 days",
               date_minor_breaks = "1 days") +
  scale_y_continuous() +
  labs(title = "Cumulative deaths per 100k Inhabitants", x = NULL, y = NULL) +
  theme(legend.position = "none")


# Combine plots:
plot_grid(CasesByCountry, CumCasesByCountry, CumCasesPerPop,
          DeathsByCountry, CumDeathsByCountry, CumDeathsPerPop,
          labels = "AUTO")

