require(gganimate)
require(tidyverse)
library(lubridate)

window_size <- 7

df_cum_eu <- read.csv('COVID_EU.csv')

df <- df_cum_eu %>% group_by (country_region) %>% mutate(inc_cases_1M=incident_cases*10/pop2020)

#Sort by Date
df$Date <- lubridate::ymd(df$Date)
df <- df[(order(df$Date)),]

#Define size of rolling average
f7=rep(1/window_size,window_size)

#Calculate rolling average
df <- df %>% group_by(country_region) %>% mutate(incident_cases_avg=stats::filter(incident_cases, f7, sides=1)) %>% filter(!is.na(incident_cases_avg))
df <- df %>% group_by(country_region) %>% mutate(inc_cases_1M_avg=stats::filter(inc_cases_1M, f7, sides=1)) %>% filter(!is.na(inc_cases_1M_avg))

#Generate a dataframe that defines today as t=0
df <- df[rev(order(df$Date)),]
df <- df %>% group_by(country_region) %>% mutate(day = -row_number()+1) %>% ungroup()

#Filter for larger countries and date
df_max <- df %>% group_by(country_region) %>% summarise(max=max(inc_cases_1M_avg)) %>% arrange(-max) %>% top_n(9) %>% ungroup()
countries_selected <- df_max$country_region

df <- df %>% filter(country_region %in% countries_selected)

# Filter NL
# df <- df %>% filter(country_region=='Netherlands')

#Need to duplicate the dataframe to be able to combine a static with dynamic plot
#https://github.com/thomasp85/gganimate/issues/94
df2 <- df
#Need to change name and delete column 'days_after_onset' to combine a static with dynamic plot
df2$d2 <- df$day
df2$day <- NULL

#Plot animation with rolling average
p <- ggplot(data=df , aes(x=day,y=inc_cases_1M_avg)) +
  geom_line(size=1) + geom_tile(data=df2,aes(x=d2,y=inc_cases_1M/2,height=inc_cases_1M, width=1), fill='grey70')+

  # geom_tile(data=df,aes(x=day-window_size/2,y=5000, height=10000, width=window_size), fill='blue', alpha=0.2)+
  # geom_point()+
  geom_line(data=df, aes(x = day, y = inc_cases_1M_avg, group = country_region), size=2) + 
  geom_point(data=df,aes(x=day,y=inc_cases_1M_avg),color='black',shape=21, fill='lightblue',size=4)+
  # ylim(0,max(df$incident_cases))+
  coord_cartesian(xlim = c(NA,0), ylim = c(0,5))+facet_wrap(~country_region)+
  labs(x=NULL,y='new cases by day (per 1M people)')+
  # geom_line(data=dec,aes(x=x2,y=y))+geom_tile(data=inc,aes(x = x, y=y, height=Inf,width=.1),fill='blue',alpha=0.2)+
  transition_reveal(day)

p <- p + theme_light(base_size = 16)
# p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# p <- p + theme(legend.position="none")

animation <- animate(p, nframes = 100, renderer = magick_renderer())

require(magick)

#Save the GIF
image_write_gif(animation, 'rolling.gif')

