library(tidyverse)

small_multiple<- function(df, x_var, y_var, group_var) {
  
  x_var <- enquo(x_var)
  y_var <- enquo(y_var)
  group_var <- enquo(group_var)

  #Order
  # reordered_list <- reorder(df$country_region, df$y, max, na.rm = TRUE)
  # ordered_list <- levels(reordered_list)
  # df$country_region <- factor(df$country_region, levels = ordered_list, ordered = TRUE)
  
  
  #Generate dataframe for labels
  df_label <- df %>% group_by(!!group_var) %>% filter(!!x_var==(first(!!x_var)))
  df_lastday <- df %>% group_by(!!group_var) %>%
    # filter(x_var!=(last(x_var))) %>% 
    filter(!!x_var==(last(!!x_var)))



multiple_plot <- ggplot(df, aes(!!x_var,!!y_var))+geom_bar(stat='identity', alpha=.8, fill='grey80') +
  geom_bar(data=df_lastday, aes(!!x_var,!!y_var), stat='identity',fill='orange')+
  
  #Define labels
  geom_label(data=df_label, aes(label=!!group_var,x=!!x_var,y=Inf), color='grey20', size =5,hjust=0,vjust=1)+
  
  #Small multiples
  facet_wrap(vars(!!group_var)) +
  
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


  return(multiple_plot)



       
}

