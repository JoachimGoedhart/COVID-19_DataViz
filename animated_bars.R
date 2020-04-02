library(tidyverse)
library(gganimate)

animated_bars <- function(df, variable_to_plot, number_of_bars=20) {
  
  variable_to_plot <- enquo(variable_to_plot)

  df_cum_ranked <- df %>%
    group_by(Date)%>%      
    mutate(rank = rank(-!!variable_to_plot),
           Value_lbl = paste0(" ",!!variable_to_plot)) %>%
    group_by(country_region) %>%
    filter(rank <= number_of_bars)
  
  #Generate bars over time
  anim_bars <- ggplot(df_cum_ranked, aes(x = rank, group = country_region, fill = as.factor(country_region), color = as.factor(country_region)))+
    geom_tile(aes(x= rank, y = !!variable_to_plot/2,
                  height = !!variable_to_plot,
                  width = 0.8), alpha = 0.8, color = NA) +
    geom_text(aes(y = 0, label = paste(country_region, " ")), vjust = 0.2, hjust = 1, size = 7) + #determine size of the label
    geom_text(aes(y=!!variable_to_plot,label = Value_lbl, hjust=0),size = 8 ) +  #determine size of the value label
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
    #Define transition
    transition_states(Date, transition_length = 4, state_length = 2) +
    #define transition style (try 'elastic-in-out', 'cubic-in-out', 'sine-in-out')
    ease_aes('sine-in-out')+
    
    NULL
  
  
  
  
  return(anim_bars)
  
}