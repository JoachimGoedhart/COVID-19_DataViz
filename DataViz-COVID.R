# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# DataViz-COVID: Script for plotting COVID-19 data, retrieved from CSSEGISandData
# Created by Joachim Goedhart (@joachimgoedhart), first version 2020
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(ggrepel)
library(gganimate)
library(lubridate)
library(gifski)

#From Color Universal Design (CUD): https://jfly.uni-koeln.de/color/
Okabe_Ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
newColors <- Okabe_Ito

#This number defines the threshold number of cases to define 'days of onset'
cutoff <- 100

################################# Data loading and wrangling ########################
#Data wrangling adapted from: https://rviews.rstudio.com/2020/03/05/covid-19-epidemiology-with-r/

#Define URL
jhu_cases_url <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
                 "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
                 "time_series_covid19_confirmed_global.csv", sep = "")
jhu_deaths_url <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
                 "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
                 "time_series_covid19_deaths_global.csv", sep = "")

#Read the data from the URL
df_cases <- read.csv(jhu_cases_url,check.names=FALSE)
df_deaths <- read.csv(jhu_deaths_url,check.names=FALSE)

#Reshape the data into a long/tidy format
df_long_cases <- df_cases %>% rename(province = "Province/State", country_region = "Country/Region") %>%
  pivot_longer(-c(province, country_region, Lat, Long), names_to = "Date", values_to = "cumulative_cases")
df_long_deaths <- df_deaths %>% rename(province = "Province/State", country_region = "Country/Region") %>%
  pivot_longer(-c(province, country_region, Lat, Long), names_to = "Date", values_to = "cumulative_deaths") %>% select('province', 'country_region', 'Date', 'cumulative_deaths')

#Combine 'confirmed cases' and 'deaths' in a single dataframe
df_cum <- df_long_cases %>% left_join(df_long_deaths, by=c('country_region','Date','province'))

# Change date format, sort by date, remove cruise ship data
df_cum <- df_cum %>% mutate(Date = mdy(Date) ) %>% 
#  filter(country_region == "US") %>% 
  arrange(province, Date) %>% 
  ungroup() %>% filter(str_detect(province, "Diamond Princess", negate = TRUE))
# %>% select(-c(country_region, Lat, Long, cumulative_cases)) 


#Collapse province data into data per country/region
df_cum <- df_cum %>% group_by(country_region,Date) %>% 
  summarise(cumulative_cases=sum(cumulative_cases), cumulative_deaths=sum(cumulative_deaths)) %>% ungroup()

#Calculate incidents per day
df_cum <- df_cum %>% group_by(country_region) %>% mutate(incident_cases = c(0, diff(cumulative_cases))) %>% ungroup()

#Calculate deaths per day
df_cum <- df_cum %>% group_by(country_region) %>% mutate(incident_deaths = c(0, diff(cumulative_deaths))) %>% ungroup()

#Rename several countries for compatibility with the population data
df_cum <- df_cum %>%
  mutate(country_region = recode(country_region, 
                                 'Taiwan*' = "Taiwan",
                                 Czechia = "Czech Republic",
                                 US = "United States",
                                 'Korea, South' = "South Korea")
  ) 

#Read population data, source: https://worldpopulationreview.com
df_pop <- read.csv("https://raw.githubusercontent.com/JoachimGoedhart/COVID-19_DataViz/master/pop_data.csv") %>% select('name','pop2020') %>% rename(country_region="name")

#Add population data
df_cum <- left_join(df_cum,df_pop, by="country_region")

#Calculate cases per 100,000 inhabitants
df_cum <- df_cum %>% mutate(cases_per_100k = cumulative_cases/pop2020*100, deaths_per_100k = cumulative_deaths/pop2020*100, inc_deaths_per_100k = incident_deaths/pop2020*100)

#Generate a dataframe that synchronizes data by 'date of onset', defined by cutoff
df_sync <- df_cum %>% filter(cases_per_100k >= 0.1) %>%  group_by(country_region) %>%
  mutate(days_after_onset = row_number()) %>% ungroup()

# Read a list of countries that belong to Europe
countries_of_europe <- read.csv("https://raw.githubusercontent.com/JoachimGoedhart/COVID-19_DataViz/master/countries_of_europe.csv", header = FALSE) %>% unlist(use.names = FALSE)

#Filter the dataframe, leaving only European countries
df_sync_eu <- df_sync %>% filter(country_region %in% countries_of_europe)

# Save the dataframe in CSV format
write.csv(df_sync_eu,"COVID_EU.csv")

################################# Plot cases vs days of onset ########################

source("line_plot.R")

#Select a number of countries
df_sync_eu_selected <- df_sync_eu %>% filter(country_region %in% c('Italy', 'France', 'Spain','Germany','United Kingdom', 'Netherlands','Norway'))

onset_plot <- line_plot(df=df_sync_eu_selected, x_var=days_after_onset,y_var=cases_per_100k, group_var=country_region)

#Define labels
onset_plot <- onset_plot + labs(title = 'Total number of confirmed cases versus days after case #100', subtitle  = "Data from: https://github.com/CSSEGISandData/COVID-19", y="Cases per 100.000 inhabitants (log-scale)", x="Days after confirmed case #100")

png(file="COVID_EU_cases_onset.png", height = 600, width = 600)
print(onset_plot)
dev.off()


################################# Plot deaths per day corrected for pop ########################

#Filter for larger countries and date
df_cum_eu <- df_cum %>% filter(country_region %in% countries_of_europe) %>% filter(pop2020>10100) %>% filter(Date > "2020-03-01") 

#Order
reordered_list <- reorder(df_cum_eu$country_region, df_cum_eu$inc_deaths_per_100k, max, na.rm = TRUE)
ordered_list <- levels(reordered_list)
df_cum_eu$country_region <- factor(df_cum_eu$country_region, levels = ordered_list, ordered = TRUE)

source("small_multiple.R")

incidence_plot <- small_multiple(df_cum_eu, Date, inc_deaths_per_100k, country_region)
incidence_plot <- incidence_plot + labs(title = 'Number of new COVID-19 related deaths per day (corrected for population)', subtitle  = "Data from: https://github.com/CSSEGISandData/COVID-19", y="Deaths per 100.000 inhabtitants", x="Days")
incidence_plot <- incidence_plot + ylim(0,3)
  
  png(file="COVID_EU_deaths.png", height = 600, width = 800)
  print(incidence_plot)
  dev.off()

################################# Generate animation ########################

#Filter the dataframe, leaving only European countries
df_cum_eu <- df_cum %>% filter(country_region %in% countries_of_europe)
  
  #Order
  reordered_list <- reorder(df_cum_eu$country_region, df_cum_eu$cumulative_cases, max, na.rm = TRUE)
  ordered_list <- levels(reordered_list)
  #Set new order
  df_cum_eu$country_region <- factor(df_cum_eu$country_region, levels = ordered_list, ordered = TRUE)


source("animated_bars.R")

#Set the starting date
df_cum_filtered <-  df_cum_eu %>% filter(Date > "2020-03-01") 

anim <- animated_bars(df_cum_filtered, cumulative_cases, 20)

#Define labels
anim <- anim + labs(title = 'Total number of confirmed cases on: {closest_state}', subtitle  = "Data from: https://github.com/CSSEGISandData/COVID-19", y="", x="")
  

#Save the animation as a GIF
animate(anim, 200, fps = 10,  width = 800, height = 800, renderer = gifski_renderer("COVID_EU.gif"))




