library(tidyverse)
library(ggrepel)
library(gganimate)
library(lubridate)
library(gifski)

#This number defines the threshold number of cases to define 'days of onset'
cutoff <- 100

#Data wrangling adapted from: https://rviews.rstudio.com/2020/03/05/covid-19-epidemiology-with-r/

#Define URL
jhu_url <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
                 "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
                 "time_series_19-covid-Confirmed.csv", sep = "")

#Read the data from the URL
df <- read.csv(jhu_url,check.names=FALSE)

#Reshape the data into a long/tidy format
df_long <- df %>% rename(province = "Province/State", country_region = "Country/Region") %>%
  pivot_longer(-c(province, country_region, Lat, Long), names_to = "Date", values_to = "cumulative_cases")

# Change date format, sort by date, remove cruise ship data
df_cum <- df_long %>% mutate(Date = mdy(Date) ) %>% 
#  filter(country_region == "US") %>% 
  arrange(province, Date) %>% 
  ungroup() %>% filter(str_detect(province, "Diamond Princess", negate = TRUE))
# %>% select(-c(country_region, Lat, Long, cumulative_cases)) 


#Collapse province data into data per country/region
df_cum <- df_cum %>% group_by(country_region,Date) %>% 
  summarise(cumulative_cases=sum(cumulative_cases)) %>% ungroup()

#Calculate incidents per day
df_cum <- df_cum %>% group_by(country_region) %>% mutate(incident_cases = c(0, diff(cumulative_cases))) %>% ungroup()

#Rename several countries for compatibility with the population data
df_cum <- df_cum %>%
  mutate(country_region = recode(country_region, 
                                 'Taiwan*' = "Taiwan",
                                 Czechia = "Czech Republic",
                                 US = "United States",
                                 'Korea, South' = "South Korea")
  ) 

#Read population data, source: https://worldpopulationreview.com
df_pop <- read.csv("pop_data.csv") %>% select('name','pop2020') %>% rename(country_region="name")

#Add population data
df_cum <- left_join(df_cum,df_pop, by="country_region")

#Calculate cases per 10,000 inhabitants
df_cum <- df_cum %>% mutate(cases_per_10k = cumulative_cases/pop2020*10)

#Generate a dataframe that synchronizes data by 'date of onset', defined by cutoff
df_sync <- df_cum %>% filter(cases_per_10k >= 0.01) %>%  group_by(country_region) %>%
  mutate(days_after_onset = row_number()) %>% ungroup()

# Optional: save the dataframe in CSV format
# write.csv(df_sync,"COVID.csv")

# Read a list of countries that belong to Europe
countries_of_europe <- read.csv("countries_of_europe.csv", header = FALSE) %>% unlist(use.names = FALSE)

#Plot number of cases per 10,000 inhabitants versus date of onset
# df_sync %>%
#   filter(country_region %in% countries_of_europe) %>%
# ggplot(aes(days_after_onset,cases_per_10k,color=country_region))+geom_line(size=1, alpha=.8) +geom_point()+
#   #Log-scale
#   scale_y_log10() +
#   #add_labels
#   geom_text(aes(x = days_after_onset, y=cases_per_10k, label = country_region, color = country_region),
#                    hjust = 1,alpha=0.8,
# #                  direction    = "y",
#                     nudge_x = -0.05,nudge_y = 0.05,
# #                   fontface = 'bold',
# #                 fill = 'white',
#                     size=5) +
#   theme_classic(base_size = 16) +
# 
#   #This is where the magic happens
#   transition_reveal(days_after_onset) +
#   #Remove Legend
#   theme(legend.position="none") +
#   view_follow(fixed_y = F,fixed_x = T) +
#   NULL
  
  
  

#Plot number of cases per 10,000 inhabitants versus date
# df_cum %>%  filter(country_region %in% countries_of_europe) %>%
#   ggplot(aes(Date,cases_per_10k,color=country_region))+geom_line(size=2) + coord_cartesian(xlim = c(mdy("02-20-2020"),NA))+
#   #add_labels
#   geom_label(aes(x = Date, y=cases_per_10k, label = country_region, color = country_region),
#                      hjust = 1,
#              # nudge_x = 5,
#                    #                   fontface = 'bold',
#                    fill = 'white',
#              size=5) +
# 
#   theme_classic(base_size = 16) +
#   theme(plot.margin = margin(5.5, 5.5, 5.5, 5.5))+
#   
#   #This is where the magic happens       
#   transition_reveal(Date) + 
#   #Remove Legend
#   theme(legend.position="none") +
#   view_follow(fixed_y = FALSE, fixed_x = TRUE) +
#   NULL



#Plot number of total cases versus date
# df_cum %>%  filter(country_region %in% countries_of_europe) %>%
#   ggplot(aes(Date,cumulative_cases,color=country_region))+geom_line(size=2) + coord_cartesian(xlim = c(mdy("02-20-2020"),NA))+
#   #add_labels
#   geom_text(aes(x = Date, y=cumulative_cases, label = country_region, color = country_region),
#             hjust = 1,alpha=0.8,
#             #                  direction    = "y",
#             nudge_x = -0.05,nudge_y = 0.05,
#             #                   fontface = 'bold',
#             #                 fill = 'white',
#             size=5) +
#   theme_classic(base_size = 16) +
#   theme(plot.margin = margin(5.5, 5.5, 5.5, 5.5))+
#   
#   #This is where the magic happens
#   transition_reveal(Date) +
#   #Remove Legend
#   theme(legend.position="none") +
#   view_follow(fixed_y = F,fixed_x = T) +
#   NULL

#Filter the dataframe, leaving only Eurpoean countries
df_cum_eu <- df_cum %>% filter(country_region %in% countries_of_europe)

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
  theme(plot.caption=element_text(size=16, hjust=0.5, face="italic", color="grey20"))+
  #Adjust size/format of title
  theme(plot.title=element_text(size=40, face="bold", colour="grey40"), plot.title.position = "panel")+
  #Define labels
  labs(title = 'Number of confirmed cases on: {closest_state}', caption  = "Data from: https://github.com/CSSEGISandData/COVID-19", y="Cases", x="")+
  #Define transition
  transition_states(Date, transition_length = 2, state_length = 2) +
  #define transition style (try 'elastic-in-out', 'cubic-in-out', 'sine-in-out')
  ease_aes('cubic-in-out')+
 
  NULL

#Save the animation as a GIF
animate(anim, 100, fps = 10,  width = 1200, height = 1000, 
        renderer = gifski_renderer("COVID_EU.gif"))




