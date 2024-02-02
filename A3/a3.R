library(tidyverse)
library(lubridate)

df <- read.csv("Eartquakes-1990-2023.csv")

df$date <- as.Date(df$date, format = '%Y-%m-%d')




df_filtered <- df %>% 
  filter(year(date) >= 2020 & year(date) <= 2022) %>%
  select(date, magnitudo)

ggplot(df_filtered, aes(x = date, y = magnitudo)) +
  geom_point(alpha = 0.5, color = "lightblue") +
  theme_minimal() +
  labs(title = "Magnitudo trend by year (2020-2022)", x = "Date", y = "Magnitudo") +
  theme(plot.title = element_text(hjust = 0.5))

#ggplot(df_filtered, aes(x = magnitudo)) +
  #geom_histogram(binwidth = (max(df$magnitudo) - min(df$magnitudo)) / 250, fill = "lightblue", color = "black", alpha = 0.7) +
  #theme_minimal() +
  #labs(title = "Magnitudo bimodal distribution", x = "Magnitudo", y = "Count") +
  #theme(plot.title = element_text(hjust = 0.5)) +
df$date <- as.Date(df$date, format="%Y-%m-%d")


df_filtered <- df %>% filter(year(date) >= 2010 & year(date) <= 2020)


ggplot(df_filtered, aes(x = date, y = magnitudo)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Magnitudo trend by year (2010-2020)", x = "Date", y = "Magnitudo") +
  theme(plot.title = element_text(hjust = 0.5))

p <- ggplot(df_filtered, aes(x = date, y = magnitudo)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Magnitudo trend by year (2010-2020)", x = "Date", y = "Magnitudo") +
  theme(plot.title = element_text(hjust = 0.5))

print(p)

ggplot(df_filtered, aes(x = date, y = magnitudo)) +
  geom_point(alpha = 0.5, color = "blue") +
  theme_minimal() +
  labs(title = "Magnitudo trend by year (2010-2020)", x = "Date", y = "Magnitudo") +
  theme(plot.title = element_text(hjust = 0.5))

df_filtered <- df %>% 
  filter(year(date) >= 2010 & year(date) <= 2020) %>% 
  select(date, magnitudo)


ggplot(df_filtered, aes(x = date, y = magnitudo)) +
  geom_point(alpha = 0.5, color = "blue") +
  theme_minimal() +
  labs(title = "Magnitudo trend by year (2010-2020)", x = "Date", y = "Magnitudo") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = magnitudo)) +
  geom_histogram(binwidth = (max(df$magnitudo) - min(df$magnitudo)) / 250, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Magnitudo bimodal distribution", x = "Magnitudo", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(figure_size = c(16, 7))

df_filtered <- df %>% 
  filter(year(date) >= 2020 & year(date) <= 2022)
earthquake_data <- data.frame(
  earthquake_name = c("2004 Sumatra - Andaman Islands", "2011 Great Tohoku, Japan", "36 km WNW of Quirihue, Chilie", "78 km WSW of Singkil, Indonesia", "off the west cost of northern Sumatra", "6KM SSW of Atico, Peru", "122 km SW of Bengkulu, Indonesia", "Kuril Islands 48 km E of Shikotan, Russia", "Sea of Okhotsk", "48 km W of Illapel, Chile", "267 km E of Levuka, Fiji", "55 km NNW of Reyes, Bolivia", "93 km NW of Iquique, Chile", "near the coast of Chiapas, Mexico", "off the west coast of northern Sumatra", "Alaska Peninsula", "134 km SSW of Kushiro,Japan"),
  magnitude = c(9.1, 9.1, 8.8, 8.6, 8.6, 8.4, 8.4, 8.3, 8.3, 8.3, 8.2, 8.2, 8.2, 8.2, 8.2, 8.2, 8.16)
)
ggplot(data = earthquake_data, aes(x = reorder(earthquake_name, -magnitude), y = magnitude)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Strongest earthquakes from 1990-2023", x = "Earthquake", y = "Magnitude") +
  theme_minimal()
ggplot(data = earthquake_data, aes(x = reorder(earthquake_name, -magnitude), y = magnitude)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Strongest earthquakes from 1990-2023", x = "Earthquake", y = "Magnitude") +
  theme_minimal()

ggplot(data = earthquake_data, aes(x = reorder(earthquake_name, magnitude), y = magnitude)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = magnitude), position = position_dodge(width = 0.9), vjust = -0.25) +
  coord_flip() +
  labs(title = "Strongest earthquakes from 1990-2023", x = "Earthquake", y = "Magnitude") +
  theme_minimal()

top_states <- earthquake_data %>%
  group_by(state) %>%
  summarize(count = n()) %>%
  arrange(-count) %>%
  head(10)

ggplot(top_states, aes(x = reorder(state, -count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 States with the Most Earthquakes", x = "State", y = "Number of Earthquakes") +
  theme_minimal()

yearly_avg <- earthquake_data %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(avg_magnitude = mean(magnitudo, na.rm = TRUE))

ggplot(yearly_avg, aes(x = year, y = avg_magnitude)) +
  geom_line(color = "steelblue") +
  geom_point(color = "red") +
  labs(title = "Average Earthquake Magnitude Per Year", x = "Year", y = "Average Magnitude") +
  theme_minimal()

yearly_data <- earthquake_data %>%
  mutate(year = year(date))

ggplot(yearly_data, aes(x = magnitudo)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~year, scales = "free_y", ncol = 4) +
  labs(title = "Magnitude of Earthquakes per Year (1990-2023)", x = "Magnitude", y = "Count") +
  theme_minimal()

countries <- c("Indonesia", "Japan", "Chile", "Papua New Guinea", "New Zealand", 
               "The Philippines", "Mexico", "Peru", "China", "Tajikistan")
earthquakes <- c(1509, 1148, 1031, 939, 676, 553, 532, 506, 490, 455)
strongest <- c(6.6, 7.0, 7.6, 7.9, 7.8, 6.3, 6.6, 6.2, 6.6, 6.6)

df <- data.frame(countries, earthquakes, strongest)

ggplot(df, aes(x = reorder(countries, -earthquakes), y = earthquakes)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0("M ", strongest)), vjust = -0.5, color = "black") +
  coord_flip() +
  labs(title = "2016: Top Countries by Number of Significant Earthquakes",
       x = "Country",
       y = "Number of Earthquakes") +
  theme_minimal()