library(dplyr)
library(tidyverse)
library(ggplot2)

A2 <- read.csv("World_Development_Indicators.csv")

#Q1
#Number of Series values
unique_series <- unique(A2$Series.Name[nzchar(A2$Series.Name)])
length(unique_series)
#Display first 9 series
unique_series %>% head(9)
#Number of series contain fertility
fertility <- A2 %>%
  filter(grepl("fertility", as.character(Series.Name)), ignore.case = TRUE) %>%
  select(Series.Name) %>%
  unique()
fertility

#Q2
#those with Series Name as “GDP (current US$)”
gdp_UScur <- A2 %>%
  filter(Series.Name == 'GDP (current US$)') 

#top 5
top_gdp_country <- gdp_UScur %>%
  arrange(desc(X2019.00)) %>%
  select(Country.Name, X2019.00) %>%
  head(5)
top_gdp_country
#bot 5
bot_gdp_country <- gdp_UScur %>%
  arrange(X2019.00) %>%
  select(Country.Name, X2019.00) %>%
  head(5)
bot_gdp_country

combined_gdp_countries <- bind_rows(top_gdp_country, bot_gdp_country)
combined_gdp_countries

#Q3
coutries <- A2 %>%
  filter(Series.Name == 'GDP (current US$)', Country.Name %in% combined_gdp_countries$Country.Name) %>%
  select(Country.Name, X2010.00, X2011.00, X2012.00, X2013.00, X2014.00, X2015.00, X2016.00, X2017.00, X2018.00, X2019.00)

coutries_gdp_year <- coutries %>%
  pivot_longer(cols = -c(Country.Name), names_to = "Year", values_to = "GDP")
coutries_gdp_year

coutries_gdp_year %>%
  group_by(Year) %>%
  summarise(Min=min(GDP, na.rm = TRUE),
            Max=max(GDP, na.rm = TRUE),
            Mean=mean(GDP, na.rm = TRUE))
#Q4
gdp_China <- coutries_gdp_year %>%
  filter(Country.Name == 'China') %>%
  filter(GDP == max(GDP) | GDP == min(GDP))
gdp_China

#Q5
coutries_gdp_year %>%
  group_by(Country.Name, Year) %>%
  summarise(GDP = sum(GDP, na.rm = TRUE)) %>%
  ggplot(aes(x = Year, y = GDP, color = Country.Name, group = Country.Name)) +
  geom_line() +
  labs(x = 'Year', y = 'GDP', title = 'Top & Bottom Countries GDP Trend')
  
#Q6

countries_growth <- A2 %>%
  filter(Series.Name == 'GDP (current US$)', Country.Name %in% combined_gdp_countries$Country.Name) %>%
  pivot_longer(cols = -c(Country.Name, Country.Code, Series.Name, Series.Code), names_to = "Year", values_to = "GDP")
countries_growth$Year <- gsub("[^0-9.]", "",countries_growth$Year)
countries_growth$Year <- as.numeric(countries_growth$Year)
countries_growth$Period <- cut(
  countries_growth$Year,
  breaks = c(1959, 1969, 1979, 1989, 1999, 2009, 2019),
  labels = c("Period1", "Period2", "Period3", "Period4", "Period5", "Period6"),
  include.lowest = TRUE
)
countries_growth %>%
  group_by(Country.Name, Period) %>% 
  summarise(AVG_GDP = mean(GDP, na.rm = TRUE)) %>%
  arrange(desc(AVG_GDP))
#Q7
 gdp_USA <- countries_growth %>%
   filter(Country.Name == 'United States')
 library(dplyr)
 
 gdp_USA <- gdp_USA %>%
   left_join(countries_growth %>%
               group_by(Country.Name, Period) %>%
               summarise(AVG_GDP = mean(GDP, na.rm = TRUE)),
             by = c("Country.Name", "Period")) %>%
   mutate(GDP = ifelse(is.na(GDP), AVG_GDP, GDP)) %>%
   select(-AVG_GDP)
 print(gdp_USA, n=20)

#Q8
 inves <- A2 %>%
   filter(Series.Name == 'GDP (current US$)', Country.Name == 'Australia'|
          Country.Name =='New Zealand'|
          Country.Name =='India'| 
          Country.Name =='China') %>%
   pivot_longer(cols = -c(Country.Name, Country.Code, Series.Name, Series.Code), names_to = "Year", values_to = "GDP")

 inves$Year <- gsub("[^0-9.]", "", inves$Year) 
 inves$Year <- as.numeric(inves$Year)   
 inves$Year <- cut(
   inves$Year,
   breaks = c(1959, 1969, 1979, 1989, 1999, 2009, 2019),
   labels = c("Period1", "Period2", "Period3", "Period4", "Period5", "Period6"),
   include.lowest = TRUE)
 inves_summary <- inves %>%
   group_by(Country.Name, Year) %>% 
   summarise(AVG_GDP = mean(GDP, na.rm = TRUE)) %>%
   arrange(desc(AVG_GDP))

 ggplot(inves_summary, aes(x = Year, y = AVG_GDP, group = Country.Name, color = Country.Name)) +
   geom_line() +
   labs(title = "Average GDP by Australia and Major Migration Countries",
        x = "Year Period",
        y = "Average GDP") +
   theme_minimal()
 