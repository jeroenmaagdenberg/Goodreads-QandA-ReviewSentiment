library(data.table)
library(tidyverse)
library(tidytext)
library(textdata)
library(dplyr)
library(vader)
library(tokenizers)
library(readr)


setwd('~/Documents/GitHub/Goodreads-QandA-ReviewSentiment')

# read amazon reviews 
amazon_reviews <- readRDS("gen/dataprep/amazon_reviews.RDS")

amazon_reviews <- amazon_reviews %>%
  select(!asin) %>%
  rename("review_id" = "Amazon_ReviewId")

# amazon_reviews <- head(amazon_reviews, 1000) # for quick changes to see if it works

# Tokenize the reviews using the unnest_tokens() function
am_reviews_tokens <- amazon_reviews %>%
  unnest_tokens(word, format = "text", review_text) 

# --- Remove Stopwords --- #
am_tidy_reviews <- am_reviews_tokens %>%
  anti_join((stop_words))
rm(am_reviews_tokens)

# Calculate the sentiment scores using afinn
am_tidy_afinn <-
  am_tidy_reviews %>%
  left_join(get_sentiments("afinn"))
rm(am_tidy_reviews)

# Calculate the AFINN sentiment score per review 
am_reviews_sentiment_afinn <- am_tidy_afinn %>%
  group_by(book_id, review_id, review_date) %>%
  summarise(sentiment_afinn = ifelse(is.nan(sum(value, na.rm = TRUE)), NA, sum(value, na.rm = TRUE)))


amazon_full <- na.omit(amazon_full, cols ="review_text") %>%
  rename("review_id" = "Amazon_ReviewId")

am_reviewsentiment <- full_join(amazon_full, am_reviews_sentiment_afinn, by = "review_id")
am_reviewsentiment <- am_reviewsentiment %>%
  group_by(Book_Id, review_id) %>%
  summarise(AFINN_score = sum(sentiment_afinn, na.rm = TRUE) 
  )


### create file for sentiment analyses per review
amazon_r_sentiment <- left_join(amazon_full, am_reviewsentiment, by = c("review_id", "Book_Id"))
amazon_r_sentiment <- amazon_r_sentiment %>%
  mutate(Likes = replace_na(Likes, 0)) %>%
  mutate(Number_of_Answers = replace_na(Number_of_Answers, 0)) %>%
  select(!review_text) %>%
  arrange(Book_Id) %>%
  mutate(source = "AM") %>%
  rename("date" = "review_date") %>%
  mutate(post_treatment = case_when(
    source == "GR" & post == 1 ~ 1,
    TRUE ~ 0
  ))


write.csv(amazon_r_sentiment, "gen/dataprep/amazon_r_sentiment.csv",  row.names = FALSE)

