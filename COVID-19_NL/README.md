# COVID-19 data visualizatie
Takes up-to-date COVID-19 data and generates a visualization and/or animation


## Input data
* Data on the number of confirmed COVID-19 cases is retrieved from: https://github.com/J535D165/CoronaWatchNL

## Data preparation
* COVID-19 Data wrangling is inspired by: https://rviews.rstudio.com/2020/03/05/covid-19-epidemiology-with-r/

## Prepare R environment
install the necessary packages (once, if necessary):
install.packages("tidyverse", "ggrepel", "gganimate","lubridate", "gifski")

## Output
* Plot with new cases per province per day: COVID_NL_new_cases.png
* Plot with total cases versus date: COVID_NL_total.png
* Animation of increased number of cases over time: COVID_NL.gif



## Output generated with the script (March 27th, 2020)




![Cases vs days](https://raw.githubusercontent.com/JoachimGoedhart/COVID-19_DataViz/master/COVID-19_NL/COVID_NL_total.png)



---


![Deaths per day](https://raw.githubusercontent.com/JoachimGoedhart/COVID-19_DataViz/master/COVID-19_NL/COVID_NL_new_cases.png)


---


![Animated bars](https://raw.githubusercontent.com/JoachimGoedhart/COVID-19_DataViz/master/COVID-19_NL/COVID_NL.gif)

    



	
	


