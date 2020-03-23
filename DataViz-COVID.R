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
                 "time_series_19-covid-Confirmed.csv", sep = "")
jhu_deaths_url <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
                 "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
                 "time_series_19-covid-Deaths.csv", sep = "")

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

#Calculate cases per 10,000 inhabitants
df_cum <- df_cum %>% mutate(cases_per_10k = cumulative_cases/pop2020*10, deaths_per_10k = cumulative_deaths/pop2020*10)

#Generate a dataframe that synchronizes data by 'date of onset', defined by cutoff
df_sync <- df_cum %>% filter(cases_per_10k >= 0.01) %>%  group_by(country_region) %>%
  mutate(days_after_onset = row_number()) %>% ungroup()

# Read a list of countries that belong to Europe
countries_of_europe <- read.csv("https://raw.githubusercontent.com/JoachimGoedhart/COVID-19_DataViz/master/countries_of_europe.csv", header = FALSE) %>% unlist(use.names = FALSE)

#Filter the dataframe, leaving only European countries
df_sync_eu <- df_sync %>% filter(country_region %in% countries_of_europe)


################################# Plot cases vs days of onset ########################

#Select a number of countries
df_sync_eu_selected <- df_sync_eu %>% filter(country_region %in% c('Italy', 'France', 'Spain','Germany','United Kingdom', 'Netherlands','Norway'))

#Generate dataframe for labels
df_label <- df_sync_eu_selected %>% group_by(country_region) %>% filter(days_after_onset==last(days_after_onset))


#Plot number of cases per 10,000 inhabitants versus date of onset
onset_plot <- ggplot(df_sync_eu_selected, aes(days_after_onset,cases_per_10k,color=country_region))+geom_line(size=1, alpha=.8) +geom_point(size=2)+
  geom_point(data=df_label, aes(x=days_after_onset,y=cases_per_10k,color=country_region), size =4)+
  scale_color_manual(values=newColors)+
  scale_fill_manual(values=newColors)+
  #Log-scale
  scale_y_log10() +

  #add_labels
  geom_label_repel(data = df_label, aes_string(label='country_region', x='days_after_onset', y='cases_per_10k', fill='country_region'),
                   fontface = 'bold', color = 'white', size=6,
                   nudge_x      = 10,
                   # direction    = "y",
                   hjust        = 0,
                  point.padding = unit(.5, 'lines'),
                   segment.color = 'grey50',
                   segment.size = 0.5)+

  
  #Define labels
  labs(title = 'Number of confirmed cases versus days after case #100', subtitle  = "Data from: https://github.com/CSSEGISandData/COVID-19", y="Cases per 10.000 inhabitants (log-scale)", x="Days after confirmed case #100")+
  
  #Define theme and fontsize
  theme_classic(base_size = 18) +
  #Remove Legend
  theme(legend.position="none") +
  #Styling the (sub)title
  theme(plot.subtitle=element_text(size=12, face="italic", color="grey70"))+
  theme(plot.title=element_text(size=20, face="bold", colour="grey20"), plot.title.position = "plot")+
  
  NULL
  
  #Save plot

png(file="COVID_EU_cases_onset.png", height = 600, width = 600)
print(onset_plot)
dev.off()


################################# Generate animation ########################

#Filter the dataframe, leaving only European countries
df_cum_eu <- df_cum %>% filter(country_region %in% countries_of_europe)

# Save the dataframe in CSV format
write.csv(df_sync_eu,"COVID_EU.csv")

#Rank the top-20 countries based on number of confirmed cases
df_cum_ranked <-  df_cum_eu %>% filter(Date > "2020-03-01") %>%
  group_by(Date)%>%      
  mutate(rank = rank(-cumulative_cases),
         Value_rel = cumulative_cases/cumulative_cases[rank==1],
         Value_lbl = paste0(" ",cumulative_cases)) %>%
  group_by(country_region) %>%
  filter(rank <= 20)

#Generate bars over time
anim <- ggplot(df_cum_ranked, aes(rank, group = country_region, fill = as.factor(country_region), color = as.factor(country_region)))+
  geom_tile(aes(y = cumulative_cases/2,
                height = cumulative_cases,
                width = 0.8), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(country_region, " ")), vjust = 0.2, hjust = 1, size = 7) + #determine size of the label
  geom_text(aes(y=cumulative_cases,label = Value_lbl, hjust=0),size = 8 ) +  #determine size of the value label
  coord_flip(clip = "off", expand = TRUE) +
  scale_x_reverse() +
  theme_light(base_size = 32)+
  
  #Remove grid
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  
  #Remove upper, left and right axis
  theme(panel.border = element_blank(), axis.ticks = element_blank()) +
  theme(axis.line.x  = element_line(colour = "grey"), axis.ticks.x.bottom = element_line(colour = "grey"))+
  #Remove y-axis label
  theme(axis.text.y=element_blank())+
  #Remove legend
  theme(legend.position="none")+
  #Adjust margin
  theme(plot.margin = margin(1,4, 1, 4, "cm")) +
  #Adjust size/format of caption with the data source
  theme(plot.subtitle=element_text(size=16, face="italic", color="grey70"))+
  #Adjust size/format of title
  theme(plot.title=element_text(size=40, face="bold", colour="grey40"), plot.title.position = "panel")+
  #Define labels
  labs(title = 'Number of confirmed cases on: {closest_state}', subtitle  = "Data from: https://github.com/CSSEGISandData/COVID-19", y="Cases", x="")+
  #Define transition
  transition_states(Date, transition_length = 4, state_length = 2) +
  #define transition style (try 'elastic-in-out', 'cubic-in-out', 'sine-in-out')
  ease_aes('sine-in-out')+
 
  NULL

#Save the animation as a GIF
animate(anim, 100, fps = 10,  width = 1200, height = 1000, 
        renderer = gifski_renderer("COVID_EU.gif"))




