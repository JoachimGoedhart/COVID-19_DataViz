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
  summarise(cumulative_cases=sum(cumulative_cases), cumulative_deaths=sum(cumulative_deaths)) %>% rename(Country='country_region') %>% ungroup()

#Calculate incidents per day
df_cum <- df_cum %>% group_by(Country) %>% mutate(cases_per_day = c(0, diff(cumulative_cases))) %>% ungroup()

#Calculate deaths per day
df_cum <- df_cum %>% group_by(Country) %>% mutate(deaths_per_day = c(0, diff(cumulative_deaths))) %>% ungroup()

#Rename several countries for compatibility with the population data
df_cum <- df_cum %>%
  mutate(Country = recode(Country, 
                                 'Taiwan*' = "Taiwan",
                                 Czechia = "Czech Republic",
                                 US = "United States",
                                 'Korea, South' = "South Korea")
  ) 

#Read population data, source: https://worldpopulationreview.com
df_pop <- read.csv("https://raw.githubusercontent.com/JoachimGoedhart/COVID-19_DataViz/master/pop_data.csv") %>% select('name','pop2020') %>% rename(Country="name")

#Add population data
df_cum <- left_join(df_cum,df_pop, by="Country")

#Calculate cases per 100,000 inhabitants
df_cum <- df_cum %>% mutate(cases_per_100k = cumulative_cases/pop2020*100, deaths_per_100k = cumulative_deaths/pop2020*100, inc_deaths_per_100k = deaths_per_day/pop2020*100)

#Generate a dataframe that synchronizes data by 'date of onset', defined by cutoff
df_sync <- df_cum %>% filter(cases_per_100k >= 0.1) %>%  group_by(Country) %>%
  mutate(days_after_onset = row_number()) %>% ungroup()


################################# Plot cases vs days of onset ########################
#Order
reordered_list <- reorder(df_sync$Country, df_sync$cases_per_100k, max, na.rm = TRUE)
ordered_list <- levels(reordered_list)
df_sync$Country <- factor(df_sync$Country, levels = ordered_list, ordered = TRUE)

#Select a number of countries
df_sync_selected <- df_sync %>% filter(Country %in% c('Italy', 'France', 'Spain','Germany','United Kingdom','China', 'United States','South Korea'))

#Generate dataframe for labels
df_label <- df_sync_selected %>% group_by(Country) %>% filter(days_after_onset==last(days_after_onset))


#Plot number of cases per 100,000 inhabitants versus date of onset
onset_plot <- ggplot(df_sync_selected, aes(days_after_onset,cases_per_100k,color=Country))+geom_line(size=1, alpha=.8) +geom_point(size=2)+
  geom_point(data=df_label, aes(x=days_after_onset,y=cases_per_100k,color=Country), size =4)+
  scale_color_manual(values=newColors)+
  scale_fill_manual(values=newColors)+
  #Log-scale
  scale_y_log10() +

  #add_labels
  geom_label_repel(data = df_label, aes_string(label='Country', x='days_after_onset', y='cases_per_100k', fill='Country'),
                   fontface = 'bold', color = 'white', size=6,
                   nudge_x      = 10,
                    # direction    = "y",
                   hjust        = 0,
                  point.padding = unit(.5, 'lines'),
                   segment.color = 'grey50',
                   segment.size = 0.5)+

  
  #Define labels
  labs(title = 'Number of confirmed cases versus days after case #100', subtitle  = "Data from: https://github.com/CSSEGISandData/COVID-19", y="Cases per 100.000 inhabitants (log-scale)", x="Days after confirmed case #100")+
  
  #Define theme and fontsize
  theme_classic(base_size = 18) +
  #Remove Legend
  theme(legend.position="none") +
  #Styling the (sub)title
  theme(plot.subtitle=element_text(size=12, face="italic", color="grey70"))+
  theme(plot.title=element_text(size=20, face="bold", colour="grey20"), plot.title.position = "plot")+
  
  NULL
  
  #Save plot

png(file="COVID_world_cases_onset.png", height = 800, width = 800)
print(onset_plot)
dev.off()


################################# Plot deaths per day corrected for pop ########################

#Filter for larger countries and date
df_cum <- df_cum %>% filter(pop2020>9000) %>% filter(Date > "2020-02-01")
df_max <- df_cum %>% group_by(Country) %>% summarise(max=max(inc_deaths_per_100k)) %>% arrange(-max) %>% top_n(20) %>% ungroup()
countries_selected <- df_max$Country

df_selected <- df_cum %>% filter(Country %in% countries_selected)

#Order
reordered_list <- reorder(df_selected$Country, df_selected$inc_deaths_per_100k, max, na.rm = TRUE)
ordered_list <- levels(reordered_list)
df_selected$Country <- factor(df_selected$Country, levels = ordered_list, ordered = TRUE)


#Generate dataframe for labels
df_label <- df_selected %>% group_by(Country) %>% filter(Date==(first(Date)))
df_lastday <- df_selected %>% group_by(Country) %>%
  # filter(Date!=(last(Date))) %>% 
  filter(Date==(last(Date)))


incidence_plot <- ggplot(df_selected, aes(Date,inc_deaths_per_100k))+geom_bar(stat='identity', alpha=.8, fill='grey80') +
    geom_bar(data=df_lastday, aes(Date,inc_deaths_per_100k), stat='identity',fill='orange')+
    
    #Define labels
  geom_label(data=df_label, aes(label=Country,x=Date,y=Inf), color='grey20', size =5,hjust=0,vjust=1)+

    #Small multiples
  facet_wrap(~Country) +
    
    #Define labels
    labs(title = 'Number of COVID-19 related deaths corrected for population', subtitle  = "Data from: https://github.com/CSSEGISandData/COVID-19", y="Deaths per 100.000 inhabtitants", x="Days")+
    
    
    #Define theme and fontsize
    theme_light(base_size = 18) +
    #Remove Legend
    theme(legend.position="none") +
    
    #Remove grid
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+

    #Styling the (sub)title
    theme(plot.subtitle=element_text(size=12, face="italic", color="grey70"))+
    theme(plot.title=element_text(size=20, face="bold", colour="grey20"), plot.title.position = "plot")+
    
    #Remove y-axis label
    theme(axis.text.x=element_blank())+
    #Remove the strip above the individual panels
    theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing.y = unit(.5, "lines"),panel.spacing.x = unit(.5, "lines")) +
    NULL
  
  png(file="COVID_world_deaths.png", height = 600, width = 800)
  print(incidence_plot)
  dev.off()

################################# Generate animation ########################


  
  #Rank the top-20 countries based on number of confirmed cases
  df_cum_ranked <-  df_cum %>% filter(Date > "2020-02-20") %>%
    group_by(Date)%>%      
    mutate(rank = rank(-cumulative_cases),
           Value_rel = cumulative_cases/cumulative_cases[rank==1],
           Value_lbl = paste0(" ",cumulative_cases)) %>%
    group_by(Country) %>%
    filter(rank <= 20) %>% ungroup()
  
  #Order
  reordered_list <- reorder(df_cum_ranked$Country, df_cum_ranked$cumulative_cases, max, na.rm = TRUE)
  ordered_list <- levels(reordered_list)  
  #Set new order
  df_cum_ranked$Country <- factor(df_cum_ranked$Country, levels = ordered_list, ordered = TRUE)  






#Generate bars over time
anim <- ggplot(df_cum_ranked, aes(rank, group = Country, fill = as.factor(Country), color = as.factor(Country)))+
  geom_tile(aes(y = cumulative_cases/2,
                height = cumulative_cases,
                width = 0.8), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Country, " ")), vjust = 0.2, hjust = 1, size = 7) + #determine size of the label
  geom_text(aes(y=cumulative_cases,label = Value_lbl, hjust=0),size = 8 ) +  #determine size of the value label
  coord_flip(clip = "off", expand = TRUE) +
  scale_x_reverse() +
  theme_light(base_size = 32)+
  scale_color_viridis_d(direction = -1) + scale_fill_viridis_d(direction=-1)+
  
  
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
  theme(plot.margin = margin(1,3, 1, 5, "cm")) +
  #Adjust size/format of caption with the data source
  theme(plot.subtitle=element_text(size=16, face="italic", color="grey70"))+
  #Adjust size/format of title
  theme(plot.title=element_text(size=24, face="bold", colour="grey40"), plot.title.position = "plot")+
  #Define labels
  labs(title = 'Number of confirmed cases on: {closest_state}', subtitle  = "Data from: https://github.com/CSSEGISandData/COVID-19", y="Cases", x="")+
  #Define transition
  transition_states(Date, transition_length = 4, state_length = 2) +
  #define transition style (try 'elastic-in-out', 'cubic-in-out', 'sine-in-out')
  ease_aes('sine-in-out')+
 
  NULL


#Save the animation as a GIF
animate(anim, 200, fps = 10,  width = 800, height = 800, 
        renderer = gifski_renderer("COVID_world.gif"))


################################# Generate animation - deaths ########################



#Rank the top-20 countries based on number of confirmed cases
df_cum_ranked <-  df_cum %>% filter(Date > "2020-02-20") %>%
  group_by(Date)%>%      
  mutate(rank = rank(-cumulative_deaths),
         Value_rel = cumulative_deaths/cumulative_deaths[rank==1],
         Value_lbl = paste0(" ",cumulative_deaths)) %>%
  group_by(Country) %>%
  filter(rank <= 20) %>% ungroup()

#Order
reordered_list <- reorder(df_cum_ranked$Country, df_cum_ranked$cumulative_deaths, max, na.rm = TRUE)
ordered_list <- levels(reordered_list)  
#Set new order
df_cum_ranked$Country <- factor(df_cum_ranked$Country, levels = ordered_list, ordered = TRUE)  






#Generate bars over time
anim_deaths <- ggplot(df_cum_ranked, aes(rank, group = Country, fill = as.factor(Country), color = as.factor(Country)))+
  geom_tile(aes(y = cumulative_deaths/2,
                height = cumulative_deaths,
                width = 0.8), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Country, " ")), vjust = 0.2, hjust = 1, size = 7) + #determine size of the label
  geom_text(aes(y=cumulative_deaths,label = Value_lbl, hjust=0),size = 8 ) +  #determine size of the value label
  coord_flip(clip = "off", expand = TRUE) +
  scale_x_reverse() +
  theme_light(base_size = 32)+
  scale_color_viridis_d(direction = -1) + scale_fill_viridis_d(direction=-1)+
  
  
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
  theme(plot.margin = margin(1,3, 1, 5, "cm")) +
  #Adjust size/format of caption with the data source
  theme(plot.subtitle=element_text(size=16, face="italic", color="grey70"))+
  #Adjust size/format of title
  theme(plot.title=element_text(size=24, face="bold", colour="grey40"), plot.title.position = "plot")+
  #Define labels
  labs(title = 'Number of COVID-19 related deaths on: {closest_state}', subtitle  = "Data from: https://github.com/CSSEGISandData/COVID-19", y="", x="")+
  #Define transition
  transition_states(Date, transition_length = 4, state_length = 2) +
  #define transition style (try 'elastic-in-out', 'cubic-in-out', 'sine-in-out')
  ease_aes('sine-in-out')+
  
  NULL


#Save the animation as a GIF
animate(anim_deaths, 200, fps = 10,  width = 800, height = 800, 
        renderer = gifski_renderer("COVID_world_deaths.gif"))






