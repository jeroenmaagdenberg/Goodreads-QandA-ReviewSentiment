library(data.table)
library(tidyverse)
library(tidytext)
library(textdata)
library(dplyr)
library(vader)
library(tokenizers)
library(readr)

# setwd("~/Documents/GitHub/Goodreads-QandA-ReviewSentiment")
# reviews <- readRDS("C:/Users/u833934/Downloads/qa_subset_goodreads_text_merged.RDS") #for testing on university PC

reviews <- readRDS("gen/dataprep/goodreads_reviews.RDS")

reviews <- reviews %>%
  select(!date_added) %>%
  select(!date)

reviews <- head(reviews, 100000)

# Tokenize the reviews using the unnest_tokens() function
reviews_tokens <- reviews %>%
  unnest_tokens(word, format = "text", review_text) %>%
  rename("Book_Id" = "book_id")

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
  summarise(sentiment_afinn_mean = ifelse(is.nan(mean(value, na.rm = TRUE)), NA, mean(value, na.rm = TRUE)))

goodreads_full <- na.omit(goodreads_full, cols ="review_text")

gr_reviewsentiment <- full_join(goodreads_full, reviews_sentiment_afinn, by = "review_id")
gr_reviewsentiment <- gr_reviewsentiment %>%
  group_by(Book_Id.x, review_id) %>%
  summarise(AFINN_mean = mean(sentiment_afinn_mean, na.rm = TRUE),
            AFINN_pretreat = mean(sentiment_afinn_mean[pre_treatment == 1], na.rm = TRUE),
            AFINN_posttreat = mean(sentiment_afinn_mean[post_treatment == 1], na.rm = TRUE)) %>%
  rename("Book_Id" = "Book_Id.x")


### create file for sentiment analyses per book
goodreads_r_sentiment <- left_join(goodreads_full, gr_reviewsentiment, by = "review_id")
goodreads_r_sentiment <- goodreads_r_sentiment %>%
  mutate(Likes = replace_na(Likes, 0)) %>%
  rename("Book_Id" = "Book_Id.x") %>%
  mutate(Number_of_Answers = replace_na(Number_of_Answers, 0)) %>%
  select(!review_text) %>%
  select(!Book_Id.y) %>%
  arrange(Book_Id) 





# Calculate the AFINN sentiment scores per book 
book_sentiment_afinn <- reviews_sentiment_afinn %>%
  group_by(Book_Id) %>% 
  summarise(AFINN_mean = mean(sentiment_afinn_mean, na.rm = TRUE))


gr_booksentiment <- full_join(goodreads_full, reviews_sentiment_afinn, by = "review_id")
gr_booksentiment <- gr_booksentiment %>%
  group_by(Book_Id.x) %>%
  summarise(AFINN_pretreat = mean(sentiment_afinn_mean[pre_treatment == 1], na.rm = TRUE),
            AFINN_posttreat = mean(sentiment_afinn_mean[post_treatment == 1], na.rm = TRUE)) %>%
  rename("Book_Id" = "Book_Id.x")

# add each book average to each book
gr_booksentiment <- full_join(book_sentiment_afinn, gr_booksentiment, by = "Book_Id")

### create file for sentiment analyses per book
goodreads_sentiment <- left_join(goodreads_questions, gr_booksentiment, by = "Book_Id")
goodreads_sentiment <- goodreads_sentiment %>%
  mutate(Likes = replace_na(Likes, 0)) %>%
  mutate(Number_of_Answers = replace_na(Number_of_Answers, 0)) %>%
  select(!Question) %>%
  select(!Date_of_Question) %>%
  select(!Scraping_Date) %>%
  select(!Question_Timestamp) %>%
  arrange(Book_Id)


write.csv(goodreads_sentiment, "gen/dataprep/goodreads_sentiment.csv")

#####
# vader # doesnt work correctly
vader_sent <- vader_df(reviews$review_text)
vader_sent <- vader_sent %>%
  mutate(
    vader_sent = case_when(
      compound > 0.05 ~ "positive",
      compound < -0.05 ~ "negative",
      TRUE ~ "neutral")
  )

book_sentiment_vader <- reviews %>%
  group_by(book_id) %>%
  mutate(vader_sent = vader_df(reviews$review_text))



### accuracy 
# library(yardstick)
# conf_mat(reviews_sentiment_afinn,
#          afinn,
#          afinn2,
#          dnn = c("afinn_sum","afinn_mean")
# )
# 
# accuracy(reviews_sentiment_afinn,
#          as.factor(afinn),
#          as.factor(afinn2)
# )
# 
# 
# ### check for removed reviews due to the cleaning process
# # check <- full_join(reviews, reviews_sentiment_afinn, by = "review_id") %>% filter(is.na(sentiment_afinn))
# 
