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
  mutate(exact_question_timestamp = as.Date(exact_question_timestamp)) %>%
  mutate(review_date = as.Date(review_date)) %>%
  mutate(Year_Month = as.Date(Year_Month)) %>%
  mutate(source = "AM") %>%
  rename("date" = "review_date") %>%
  mutate(post_treatment = case_when(
    source == "GR" & post == 1 ~ 1,
    TRUE ~ 0
  ))


write.csv(amazon_r_sentiment, "gen/dataprep/amazon_r_sentiment.csv",  row.names = FALSE)





##### book #####
# Calculate the AFINN sentiment scores per book 
am_book_sentiment_afinn <- am_reviews_sentiment_afinn %>%
  rename("Book_Id" = "book_id") %>%
  group_by(Book_Id) %>% 
  summarise(AFINN_book_score = sum(sentiment_afinn, na.rm = TRUE),
            AFINN_book_mean = mean(sentiment_afinn, na.rm = TRUE)) 
#### should i do it per book with sum or with mean? 


### create file for sentiment analyses per book
amazon_b_sentiment <- left_join(goodreads_questions, am_book_sentiment_afinn, by = "Book_Id")
amazon_b_sentiment <- amazon_b_sentiment %>%
  mutate(Likes = replace_na(Likes, 0)) %>%
  mutate(Number_of_Answers = replace_na(Number_of_Answers, 0)) %>%
  select(!Date_of_Question) %>%
  select(!Scraping_Date) %>%
  select(!Question_Timestamp) %>%
  mutate(exact_question_timestamp = as.Date(exact_question_timestamp)) %>%
  arrange(Book_Id) %>%
  mutate(source = "AM") %>%
  mutate(AFINN_book_score = replace_na(AFINN_book_score, 0)) %>%
  mutate(AFINN_book_mean = replace_na(AFINN_book_mean, 0))


# filter amazon_b_sentiment based on the subset
amazon_booklist <- amazon_r_sentiment %>%
  select(Book_Id) %>%
  distinct()
amazon_b_sentiment <- filter(amazon_b_sentiment, Book_Id %in% amazon_booklist$Book_Id) 



write.csv(amazon_b_sentiment, "gen/dataprep/amazon_b_sentiment.csv", row.names = FALSE)


