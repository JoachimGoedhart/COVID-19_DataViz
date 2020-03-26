# COVID-19 DataViz script
Takes up-to-date COVID-19 data and generates a visualization and/or animation


## Input data
* Data on the number of confirmed COVID-19 cases is retrieved from JHU CCSE: https://github.com/CSSEGISandData/COVID-19
* Data on current world population is from: https://worldpopulationreview.com and included as pop_data.csv
* A list of Eurpean countries (included in this repository; countries_of_europe.csv) 

## Data preparation
* COVID-19 Data wrangling is inspired by: https://rviews.rstudio.com/2020/03/05/covid-19-epidemiology-with-r/

## Prepare R environment
install the necessary packages (once, if necessary):
install.packages("tidyverse", "ggrepel", "gganimate","lubridate", "gifski")

## Output
* Tidy dataframe with confirmed cases in Europe: COVID_EU.csv
* Plot of cases per 10k inhabitants versus days after case #100: COVID_EU_cases_onset.png
* Animation of number of confirmed cases over time for 20 highest ranking European countries : COVID_EU.gif


## Output generated with the script

* Confirmed cases per day (per 100.000 inhabitants on log-scale) after confirmation of the 100th case
![Cases vs days](https://github.com/JoachimGoedhart/COVID-19_DataViz/raw/master/COVID_EU_cases_onset.png)


* New deaths per day for the 12 most populated countries of the EU (numbers normalized for number of inhabitants)

![Deaths per day](https://github.com/JoachimGoedhart/COVID-19_DataViz/raw/master/COVID_EU_deaths.png)

*Animated cases per day for the top-ranking countries in Europe

![Animated bars](https://github.com/JoachimGoedhart/COVID-19_DataViz/raw/master/COVID_EU.gif)

    
## Future
* Filter based on different continents
* Switch between confirmed cases/deaths

	
	


