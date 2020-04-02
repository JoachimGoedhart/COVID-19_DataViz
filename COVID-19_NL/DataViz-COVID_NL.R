# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# DataViz-COVID: Script for plotting COVID-19 data, retrieved from https://zenodo.org/record/3725810/files/rivm_corona_in_nl.csv
# Created by Joachim Goedhart (@joachimgoedhart), first version 2020
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#From Color Universal Design (CUD): https://jfly.uni-koeln.de/color/
Okabe_Ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
newColors <- Okabe_Ito


################################# Data loading and wrangling ########################
#Data wrangling adapted from: https://rviews.rstudio.com/2020/03/05/covid-19-epidemiology-with-r/

#Define URL
rivm_cases_url <- "https://raw.githubusercontent.com/J535D165/CoronaWatchNL/master/data/rivm_NL_covid19_province.csv"

#Read the data from the URL
df_cases <- read.csv(rivm_cases_url,check.names=FALSE)
#df_deaths <- read.csv(jhu_deaths_url,check.names=FALSE)



df_cases <- df_cases %>% mutate(Datum = ymd(Datum)) %>% arrange(Provincienaam, Datum) %>% filter(!is.na(Provincienaam))

df_cum <- df_cases %>% group_by(Datum,Provincienaam) %>% summarise(totaal=sum(Aantal)) %>% filter(Provincienaam!="")

#Calculate incidents per day
df_cum_prov <- df_cum %>% group_by(Provincienaam) %>% mutate(per_dag = c(0, diff(totaal))) %>% ungroup()


################################# Plot cases vs days of onset ########################

#Define Order
reordered_list <- reorder(df_cum_prov$Provincienaam, df_cum_prov$totaal, max, na.rm = TRUE)
ordered_list <- levels(reordered_list)

#Set new order
df_cum_prov$Provincienaam <- factor(df_cum_prov$Provincienaam, levels = ordered_list, ordered = TRUE)
  

#Generate dataframe for labels
df_label <- df_cum_prov %>% group_by(Provincienaam) %>% filter(Datum==last(Datum))


#Plot number of cases versus date
aantal_plot <- ggplot(df_cum_prov, aes(Datum,totaal,color=Provincienaam))+geom_line(size=1, alpha=.8) +geom_point(size=2)+
  geom_point(data=df_label, aes(x=Datum,y=totaal,color=Provincienaam), size =4)+
  scale_color_viridis_d(direction = -1) + scale_fill_viridis_d(direction=-1)+

  #add_labels
  geom_text(data = df_label, aes_string(label='Provincienaam', x='Datum', y='totaal', color='Provincienaam'),
                   fontface = 'bold', size=4,

                   hjust= 0,
                   vjust=0,
            nudge_x = 1,
              check_overlap = TRUE,
                    )+

  
  
  #Define labels
  labs(title = 'Aantal geregistreerde gevallen COVID-10 per provincie', subtitle  = "Data from: https://github.com/J535D165/CoronaWatchNL", y="Aantal", x="Datum")+
  coord_cartesian(clip = 'off')+
  #Adjust margin

  #Define theme and fontsize
  theme_classic(base_size = 18) +
  #Remove Legend
  theme(legend.position="none") +
  #Styling the (sub)title
  theme(plot.subtitle=element_text(size=12, face="italic", color="grey70"))+
  theme(plot.title=element_text(size=20, face="bold", colour="grey20"), plot.title.position = "plot")+
  theme(plot.margin = margin(1,6, 1, 1, "cm")) +
  
  NULL
  
  #Save plot

png(file="COVID_NL_total.png", height = 600, width = 600)
print(aantal_plot)
dev.off()


#################################  ########################

#Generate dataframe for labels
df_label <- df_cum_prov %>% group_by(Provincienaam) %>% filter(Datum==first(Datum))
df_label$Datum <- min(df_cum_prov$Datum)
df_lastday <- df_cum_prov %>% group_by(Provincienaam) %>%
  filter(Datum==(last(Datum)))


incidence_plot <- ggplot(df_cum_prov, aes(Datum,per_dag))+geom_bar(stat='identity', alpha=.8, fill='grey80') +
    geom_bar(data=df_lastday, aes(Datum,per_dag), stat='identity',fill='orange')+
    
    #Define labels
  geom_label(data=df_label, aes(label=Provincienaam,x=Datum,y=Inf), color='grey20', size =5,hjust=0,vjust=1)+

    #Small multiples
  facet_wrap(~Provincienaam) +
    
    #Define labels
    labs(title = 'Aantal nieuwe COVID-19 gevallen per dag', subtitle  = "Data from: https://github.com/J535D165/CoronaWatchNL", y="Aantal", x="Dagen")+
    
    
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
  
  png(file="COVID_NL_new_cases.png", height = 600, width = 800)
  print(incidence_plot)
  dev.off()

################################# Generate animation ########################

  #Rank the provinces
  df_cum_ranked <-  df_cum_prov %>% filter(Datum > "2020-03-01") %>%
    group_by(Datum)%>%      
    mutate(rank = rank(-totaal),
           Value_rel = totaal/totaal[rank==1],
           Value_lbl = paste0(" ",totaal))



#Generate bars over time
anim <- ggplot(df_cum_ranked, aes(rank, group = Provincienaam, fill = as.factor(Provincienaam), color = as.factor(Provincienaam)))+
  geom_tile(aes(y = totaal/2,
                height = totaal,
                width = 0.8), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Provincienaam, " ")), vjust = 0.2, hjust = 1, size = 7) + #determine size of the label
  geom_text(aes(y=totaal,label = Value_lbl, hjust=0),size = 8 ) +  #determine size of the value label
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
  theme(plot.margin = margin(1,4, 1, 4, "cm")) +
  #Adjust size/format of caption with the data source
  theme(plot.subtitle=element_text(size=16, face="italic", color="grey70"))+
  #Adjust size/format of title
  theme(plot.title=element_text(size=24, face="bold", colour="grey40"), plot.title.position = "panel")+
  #Define labels
  labs(title = 'Aantal geregistreerde gevallen op: {closest_state}', subtitle  = "Data from: https://github.com/J535D165/CoronaWatchNL", y="Aantal", x="")+
  #Define transition
  transition_states(Datum, transition_length = 4, state_length = 2) +
  #define transition style (try 'elastic-in-out', 'cubic-in-out', 'sine-in-out')
  ease_aes('sine-in-out')+
 
  NULL

#Save the animation as a GIF
animate(anim, 200, fps = 10,  width = 800, height = 600, 
        renderer = gifski_renderer("COVID_NL.gif"))



####################### Save the dataframe ####################
# Save the dataframe in CSV format
write.csv(df_cum_prov,"COVID_NL.csv")


