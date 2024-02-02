
#Task B
library(rvest)
library(ggplot2)
Eq_url <- "https://earthquakelist.org/reports/top-100-countries-most-earthquakes/"
Eq_page <- read_html(Eq_url)
Eq_page

Eq_tables <- html_table(Eq_page, fill = TRUE) [[2]]
Eq_tables
library(dplyr)
library(stringr)

Eq_tables <- Eq_tables %>%
  mutate(across(everything(), ~str_replace_all(., "\\(.*?\\)", "")))
Eq_tables

Eq_tables <- Eq_tables %>%
  mutate(Rank = as.integer(Rank),
         Earthquakes = as.integer(Earthquakes))
# Bar Plot for Number of Earthquakes by Country
p1 <- ggplot(Eq_tables[1:10,], aes(x = reorder(Country, Earthquakes), y = Earthquakes)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Number of Earthquakes by Country 2023", x = "Country", y = "Number of Earthquakes") +
  theme_minimal()

print(p1)


Eq_url2 <- "https://earthquakelist.org/reports/top-100-countries-most-earthquakes/#2022"
Eq_page2 <- read_html(Eq_url2)
Eq_page2

Eq_tables2 <- html_table(Eq_page2, fill = TRUE) [[2]]
Eq_tables2
Eq_tables2 <- Eq_tables2 %>%
  mutate(across(everything(), ~str_replace_all(., "\\(.*?\\)", "")))
Eq_tables2

Eq_tables2 <- Eq_tables %>%
  mutate(Rank = as.integer(Rank),
         Earthquakes = as.integer(Earthquakes))
install.packages("ggrepel")
library(ggrepel)
p2 <- ggplot(Eq_tables2[1:10,], aes(x = Earthquakes, y = as.numeric(gsub("M ", "", Strongest)), label = Country)) +
  geom_point(aes(color = Country), size = 3) +
  geom_text_repel() +
  labs(title = "Number of Earthquakes vs. Magnitude of Strongest Earthquake 2022", 
       x = "Number of Earthquakes", y = "Magnitude of Strongest Earthquake") +
  theme_minimal() +
  theme(legend.position = "none")

print(p2)

#Task C
library(lubridate)
library(tidyverse)

olympics_df <- read.csv("Olympics_tweets.csv")
olympics_df
# Convert to Date type
#2.1
olympics_df = olympics_df %>%
  mutate(user_created_at_year = year(dmy_hm(user_created_at)))
olympics_df$Date <- as.POSIXct(olympics_df$date, format="%d/%m/%Y %H:%M")
olympics_df$user_created_at <- as.POSIXct(olympics_df$user_created_at, format="%d/%m/%Y %H:%M")
# Convert to Integer type
int_columns <- c("user_friends", "retweet_count", "favorite_count", "user_followers")
olympics_df[int_columns] <- lapply(olympics_df[int_columns], as.integer)
# Convert to Character type
char_columns <- c("id", "text", "user_screen_name", "user_location", "user_description", "language")
olympics_df[char_columns] <- lapply(olympics_df[char_columns], as.character)
# Convert to Boolean type
olympics_df$favorited <- as.logical(olympics_df$favorited)
#2.2
olympics_df %>%
  ggplot(aes(x= user_created_at_year)) +
  geom_bar(fill="lightblue") +
  ggtitle("User Creation by Year")

#2.3
olympics_2.3 <- olympics_df %>%
  filter(user_created_at_year > 2010) %>%
  group_by(user_created_at_year) %>%
  summarise(avg_user = mean(user_followers))

ggplot(olympics_2.3, aes(x = user_created_at_year, y = avg_user)) +
  geom_bar(stat="identity", fill="lightblue") +
  labs(title = "Average Number of Followers for Users Created After 2010",
       x = "Year of Account Creation",
       y = "Average Number of Followers") +
  theme_minimal()

#2.4
olympics_2.4 <- olympics_df %>%
  filter(user_created_at_year > 2010) %>%
  group_by(user_created_at_year) %>%
  summarise(avg_user = mean(user_friends))

ggplot(olympics_2.4, aes(x = user_created_at_year, y = avg_user)) +
  geom_bar(stat="identity", fill="lightblue") +
  labs(title = "Average Number of Friends for Users Created After 2010",
       x = "Year of Account Creation",
       y = "Average Number of Friends") +
  theme_minimal()

#2.6
total_tweets_with_location <- sum(!is.na(olympics_df$user_location))

topfre_location <- olympics_df %>%
  filter(!is.na(user_location)) %>%
  group_by(user_location) %>%
  summarise(num_tweets = n()) %>%
  arrange(-num_tweets) %>%
  slice_head(n = 10) %>%
  mutate(proportion = num_tweets / total_tweets_with_location)

topfre_location

#3.1
library(dplyr)
library(stringr)

olympics_df <- olympics_df %>%
  mutate(date_extracted = str_extract(date, "\\d{1,2}/\\d{1,2}/\\d{2}"))
olympics_df
#3.2
tweet_counts <- olympics_df %>%
  filter(!is.na(date_extracted)) %>%
  group_by(date_extracted) %>%
  summarise(num_tweets = n()) %>%
  arrange(num_tweets)
ggplot(tweet_counts, aes(x = date_extracted, y = num_tweets)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Tweets per Date",
       
       x = "Date",
       y = "Number of Tweets") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

#3.3
olympics_3.3 <- olympics_df %>%
  mutate(text_length = str_length(text)) %>%
  mutate(lengths = cut (text_length,breaks = c(1,40,80,120,160,200,240,Inf),
                        labels = c('<40','41-80','81-120','121-160','161-200','201-240','241>')))

tweet_length_counts <- olympics_3.3 %>%
  group_by(lengths) %>%
  summarise(num_tweets = n())
ggplot(tweet_length_counts, aes(x = lengths, y = num_tweets)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Number of Tweets by Text Length Category",
       x = "Text Length Category",
       y = "Number of Tweets") +
  theme_minimal()

#3.4

olympics_df_mentions <- olympics_df %>%
  mutate(num_mentions = str_count(text, "@[a-zA-Z0-9_]+"))

num_tweets_mentions <- sum(olympics_df_mentions$num_mentions > 0)
num_tweets_3mentions <- sum(olympics_df_mentions$num_mentions >= 3)

list(
  tweets_with_mentions = num_tweets_mentions,
  tweets_with_3_or_more_mentions = num_tweets_3mentions
)

#Task 4
predict_train_df = read.csv("predictive_twitter_data.csv")
predict_train_df$relevanceJudge = as.factor(predict_train_df$relevanceJudge)
X_train = predict_train_df[1:24]
Y_train = predict_train_df[25]
library(rpart)
library(rpart.plot)
predict_model = rpart(predict_train_df$relevanceJudge ~ . ,data=predict_train_df,
                      method = 'class')
rpart.plot(predict_model, extra=106)
test1 = predict(predict_model, X_train,type='class')

library(caret)
preProcess_range <- preProcess(X_train, method='range')
X_train_norm <- predict(preProcess_range, X_train)

# Feature Importance
# Split data into predictors and response
X_train <- predict_train_df[, 1:24]
Y_train <- predict_train_df$relevanceJudge

# Build the model
fit <- rpart(relevanceJudge ~ . , data=predict_train_df, method = 'class')

# Extract variable importance
importance <- fit$variable.importance
print(importance)


importance <- fit$variable.importance
print(importance)

importance <- rpart.importance(predict_model)
important_vars <- rownames(importance)[order(-importance[, "Overall"])][1:10]

# Use only top 10 important features for the next model
X_train_important <- X_train[,important_vars]

# Handle Missing Values by replacing NA with median
for(i in 1:ncol(X_train_important)){
  X_train_important[is.na(X_train_important[,i]), i] <- median(X_train_important[,i], na.rm = TRUE)
}

# Rescale Data
library(caret)
preProcess_range <- preProcess(X_train_important, method='range')
X_train_norm <- predict(preProcess_range, X_train_important)

# Build New Model
predict_model_v2 = rpart(Y_train ~ . ,data=X_train_norm, method = 'class')
rpart.plot(predict_model_v2, extra=106)

# Predictions
test1_v2 = predict(predict_model_v2, X_train_norm, type='class')
