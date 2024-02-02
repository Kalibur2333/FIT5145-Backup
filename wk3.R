# Load tidyverse
library(tidyverse)

# Read house price data
houses <- read_csv("Melbourne_housing_FULL.csv")

# Head of houses
head(houses)

# Glimpse of houses
glimpse(houses)

# Subsetting the data
houses_sub <- houses %>%
  select(Price, Rooms, Type, Distance, Bedroom2, Bathroom)

# Load GGally
library(GGally)

# Distribution of continuous numerical variables
houses_sub %>%
  select(Distance, Price) %>%
  ggpairs()

Bar plots of discrete numerical variables
houses_sub %>%
  select(-Distance, -Price, -Type) %>%
  gather(Variable, Value) %>%
  ggplot(aes(x = Value)) +
  geom_bar() +
  facet_wrap(~ Variable, scales = "free")

# Install and load the naniar package
# install.packages("naniar")
library(naniar)

# Map of data colored by variable type and NA
vis_miss(houses_sub)

# Missingness map
vis_miss(houses_sub)

# Install and load the naniar package
# install.packages("naniar")
library(naniar)

# Missing variables summary table
miss_var_summary <- vis_miss(houses_sub)
print(miss_var_summary)

# Remove missing house price values
houses_sub <- houses_sub %>%
  filter(!is.na(Price))

# Load necessary libraries
library(naniar)
library(ggplot2)

# Bind the shadow matrix houses_sub
houses_sub_shadow <- bind_shadow(houses_sub)

# Scatter plot of Bath vs. Bed colored by missingness in Bed
ggplot(houses_sub_shadow, aes(x = Bathroom, y = Bedroom2, color = Bedroom2_NA)) +
  geom_point() +
  labs(title = "Scatter plot of Bathroom vs. Bedroom2 with Missingness on Bedroom2",
       color = "Missing Bedroom2") +
  theme_minimal()

# Missing values don't show because all missing values in Bedroom2 are also missing in Bathroom
# If Bedroom2 is missing, Bathroom will also be missing

# Missingness map with just Bathroom and Bedroom2
ggplot(houses_sub_shadow, aes(x = Bathroom, y = Bedroom2)) +
  geom_miss_point()

# Missingness map based on missings in Bedroom2
ggplot(houses_sub_shadow, aes(x = Bathroom, y = Bedroom2)) +
  geom_miss_point(aes(color = missings(Bedroom2)))
