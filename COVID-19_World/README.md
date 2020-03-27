# COVID-19 data visualization - worldwide

Takes up-to-date COVID-19 data and generates a visualization and/or animation

## Input data
* Data on the number of confirmed COVID-19 cases is retrieved from JHU CCSE: https://github.com/CSSEGISandData/COVID-19
* Data on current world population is from: https://worldpopulationreview.com and included as pop_data.csv

## Data preparation
* COVID-19 Data wrangling is inspired by: https://rviews.rstudio.com/2020/03/05/covid-19-epidemiology-with-r/

## Prepare R environment
install the necessary packages (once, if necessary):
install.packages("tidyverse", "ggrepel", "gganimate","lubridate", "gifski")

## Output
* Animation of increased number of cases over time: COVID_world.gif
* Plot with cases (normalized to population) after detection of case #100: COVID_world_cases_onset.png
* Plot with COVID-19 related deaths per day versus date: COVID_world_deaths.png


## Output generated with the script (March 27th, 2020)




![Cases vs days](https://raw.githubusercontent.com/JoachimGoedhart/COVID-19_DataViz/master/COVID-19_World/COVID_world.gif)



---


![Deaths per day](https://raw.githubusercontent.com/JoachimGoedhart/COVID-19_DataViz/master/COVID-19_World/COVID_world_cases_onset.png)


---


![Animated bars](https://raw.githubusercontent.com/JoachimGoedhart/COVID-19_DataViz/master/COVID-19_World/COVID_world_deaths.png)

    



	
	


