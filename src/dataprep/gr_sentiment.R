library(data.table)
library(tidyverse)
library(tidytext)
library(textdata)
library(dplyr)
library(vader)
library(tokenizers)
library(readr)

setwd("~/Documents/GitHub/Goodreads-QandA-ReviewSentiment")
#reviews <- readRDS("C:/Users/u833934/Downloads/qa_subset_goodreads_text_merged.RDS") #for testing on university PC

reviews <- readRDS("gen/dataprep/goodreads_reviews.RDS")

reviews <- reviews %>%
  select(!date_added) %>%
  select(!date)

# reviews <- head(reviews, 1000) # for quick changes to see if it works

# Tokenize the reviews using the unnest_tokens() function
reviews_tokens <- reviews %>%
  unnest_tokens(word, format = "text", review_text) %>%
  rename("Book_Id" = "book_id")
rm(reviews)

# --- Remove Stopwords --- #
tidy_reviews <- reviews_tokens %>%
  anti_join((stop_words))
rm(reviews_tokens)

# Calculate the sentiment scores using afinn
tidy_afinn <-
  tidy_reviews %>%
  left_join(get_sentiments("afinn"))
rm(tidy_reviews)

# Calculate the AFINN sentiment score per review 
reviews_sentiment_afinn <- tidy_afinn %>%
  group_by(Book_Id, review_id) %>%
  summarise(sentiment_afinn = ifelse(is.nan(sum(value, na.rm = TRUE)), NA, sum(value, na.rm = TRUE)))

goodreads_full <- na.omit(goodreads_full, cols ="review_text")

gr_reviewsentiment <- full_join(goodreads_full, reviews_sentiment_afinn, by = "review_id")
gr_reviewsentiment <- gr_reviewsentiment %>%
  group_by(Book_Id.x, review_id) %>%
  summarise(AFINN_score = sum(sentiment_afinn, na.rm = TRUE) #,
            #AFINN_pretreat = sum(sentiment_afinn[treated == 0], na.rm = TRUE),
            #AFINN_posttreat = sum(sentiment_afinn[treated == 1], na.rm = TRUE) 
            ) %>%
  rename("Book_Id" = "Book_Id.x")


### create file for sentiment analyses per review
goodreads_r_sentiment <- left_join(goodreads_full, gr_reviewsentiment, by = c("review_id", "Book_Id"))
goodreads_r_sentiment <- goodreads_r_sentiment %>%
  mutate(Likes = replace_na(Likes, 0)) %>%
  mutate(Number_of_Answers = replace_na(Number_of_Answers, 0)) %>%
  select(!review_text) %>%
  arrange(Book_Id) %>%
  mutate(source = "GR") %>%
  mutate(post_treatment = case_when(
    source == "GR" & post == 1 ~ 1,
    TRUE ~ 0
  ))

write.csv(goodreads_r_sentiment, "gen/dataprep/goodreads_r_sentiment.csv", row.names = FALSE)


##### book #####
# Calculate the AFINN sentiment scores per book 
book_sentiment_afinn <- reviews_sentiment_afinn %>%
  group_by(Book_Id) %>% 
  summarise(AFINN_book_score = sum(sentiment_afinn, na.rm = TRUE),
            AFINN_book_mean = mean(sentiment_afinn, na.rm = TRUE))
#### should i do it per book with sum or with mean? 


### create file for sentiment analyses per book
goodreads_b_sentiment <- left_join(goodreads_questions, book_sentiment_afinn, by = "Book_Id")
goodreads_b_sentiment <- goodreads_b_sentiment %>%
  mutate(Likes = replace_na(Likes, 0)) %>%
  mutate(Number_of_Answers = replace_na(Number_of_Answers, 0)) %>%
  select(!Date_of_Question) %>%
  select(!Scraping_Date) %>%
  select(!Question_Timestamp) %>%
  arrange(Book_Id) %>%
  mutate(source = "GR")


write.csv(goodreads_b_sentiment, "gen/dataprep/goodreads_b_sentiment.csv", row.names = FALSE)

