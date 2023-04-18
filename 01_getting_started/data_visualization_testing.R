library(tidyverse)
library(maps)
library(dplyr)
library(lubridate)
library(scales)
#Challenge 1 -----------------------------------------

#Imports ----
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
world <- map_data("world")

#Wrangling -----
covid_data_tbl <- covid_data_tbl %>%
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
    
  ))
covid_data_tbl <- covid_data_tbl %>% filter(
  countriesAndTerritories %in% c("Germany","France","UK","Spain","USA")
)


#change dateRep to date column type
covid_data_tbl <- covid_data_tbl %>%
  group_by(dateRep) %>%
  mutate(dateRep=as.Date(dateRep, format = "%d/%m/%Y"))

# sort by country then arrange by date in ascending order
covid_data_tbl <- covid_data_tbl %>%

  group_by(countriesAndTerritories) %>%
  arrange(dateRep, by_group = TRUE) %>%
  arrange(countriesAndTerritories) %>%
  ungroup()

#calculate cumulative sums
covid_data_tbl <- covid_data_tbl %>% 
  select(dateRep, cases, deaths, popData2019, countriesAndTerritories) %>%
  group_by(countriesAndTerritories)
covid_data_tbl$csum <- ave(covid_data_tbl$cases, covid_data_tbl$countriesAndTerritories, FUN=cumsum)

#Visualization ----
covid_data_tbl %>%
  
  ggplot(aes(x = dateRep, y = csum, color = countriesAndTerritories)) +
  
  geom_line(size = 1.2, linetype = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(n.breaks = 10, labels = unit_format(unit = "M", scale = 1e-6)) +
  scale_colour_manual(values = c("red", "blue", "green", "cyan", "orange")) +
  
labs(
  title = "Covid-19 confirmed cases worldwide",
  subtitle = "As of 03/12, USA is the leading country in number of cases",
  caption = "Data from https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
  x = "Year 2020",
  y = "Cumulitive cases",
  color = "Country" # Legend text
)

#Challenge 2 -------------------------------------------------------------------------------
#Imports ----
covid_data_tbl_2 <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
world <- map_data("world")

#Wrangling -----
covid_data_tbl_2 <- covid_data_tbl_2 %>%
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
    
  ))


#change dateRep to date column type
covid_data_tbl_2 <- covid_data_tbl_2 %>%
  group_by(dateRep) %>%
  mutate(dateRep=as.Date(dateRep, format = "%d/%m/%Y"))

# sort by country then arrange by date in ascending order
covid_data_tbl_2 <- covid_data_tbl_2 %>%
  
  group_by(countriesAndTerritories) %>%
  arrange(dateRep, by_group = TRUE) %>%
  arrange(countriesAndTerritories) %>%
  ungroup()

covid_data_deaths <- covid_data_tbl_2 %>%
group_by(countriesAndTerritories, popData2019) %>%
  summarize(total_deaths = sum(deaths)) %>%
  ungroup() %>%
  mutate(mortalityrate = (total_deaths / popData2019) *100) %>%
  mutate(mortalityrate_text = scales::dollar(mortalityrate, big.mark = ",", decimal.mark = ".", prefix = "", suffix = "%"))

world <- plyr::rename(
  world, 
  replace      = c(region="countriesAndTerritories", foo="missing_varible"),
  warn_missing = FALSE
)

covid_data_deaths <- covid_data_deaths %>%
  merge(y = world, by = "countriesAndTerritories", all.x = FALSE, all.y = FALSE)

world <- plyr::rename(
  world, 
  replace      = c(countriesAndTerritories="region", foo="missing_varible"),
  warn_missing = FALSE
)

covid_data_deaths %>%
  ggplot(aes(map_id = countriesAndTerritories )) +
  scale_fill_gradient(low="red", high="black", name = "Mortality Rate %", n.breaks = 6) +
  geom_map(aes(fill = mortalityrate), map = world) +
  expand_limits(x = covid_data_deaths$long, y = covid_data_deaths$lat) +

  labs(
    title = "Confirmed Covid-19 deaths relative to the size of the population",
    subtitle = "More than 1.2 Million confirmed deaths worldwide",
    caption = "Challenge 2 - Date as of 03/12/20",
    x = "",
    y = ""
  )
