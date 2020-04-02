library(tidyverse)
library(ggrepel)

line_plot <- function(df, x_var, y_var, group_var) {
  
  x_var <- enquo(x_var)
  y_var <- enquo(y_var)
  group_var <- enquo(group_var)
  
  #Generate dataframe for labels
  df_label <- df %>% group_by(!!group_var) %>% filter(!!x_var==last(!!x_var))
  
  
  #Plot number of cases per 100,000 inhabitants versus date of onset
  plot <- ggplot(df, aes(!!x_var,!!y_var,color=!!group_var)) + geom_line(size=1, alpha=.8) +geom_point(size=2)+
    geom_point(data=df_label, aes(x=!!x_var,y=!!y_var,color=!!group_var), size =4)+
    scale_color_manual(values=newColors)+
    scale_fill_manual(values=newColors)+
    #Log-scale
    scale_y_log10() +
    
    #add_labels
    geom_label_repel(data = df_label, aes(label=!!group_var, x=!!x_var, y=!!y_var, fill=!!group_var),
                     fontface = 'bold', color = 'white', size=6,
                     nudge_x      = 10,
                     # direction    = "y",
                     hjust        = 0,
                     point.padding = unit(.5, 'lines'),
                     segment.color = 'grey50',
                     segment.size = 0.5)+
    
    #Define theme and fontsize
    theme_classic(base_size = 18) +
    #Remove Legend
    theme(legend.position="none") +
    #Styling the (sub)title
    theme(plot.subtitle=element_text(size=12, face="italic", color="grey70"))+
    theme(plot.title=element_text(size=20, face="bold", colour="grey20"), plot.title.position = "plot")+
    
    NULL
  
  return(plot)
  
  
  
  
  
  
  
}
