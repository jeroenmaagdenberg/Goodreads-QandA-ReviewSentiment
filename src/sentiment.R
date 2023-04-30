library(data.table)
library(tidyverse)
library(tidytext)
library(textdata)
library(dplyr)
library(vader)
library(stringr)
library(tokenizers)
library(readr)

# setwd("~/Documents/GitHub/Goodreads-QandA-ReviewSentiment")
reviews <- readRDS("qa_subset_goodreads_text_merged.RDS")

reviews <- head(goodreads_reviews, 10000)
# reviews <- goodreads_full # for full file
# reviews <- readRDS("C:/Users/u833934/Downloads/qa_subset_goodreads_text_merged.RDS") #for testing on university PC

# rm(goodreads_reviews,df_goodreads_reviews)

reviews <- reviews %>%
  mutate(
    # remove links
    review_text = str_remove_all(reviews$review_text, "https\\S*"),
    review_text = str_remove_all(reviews$review_text, "http\\S*"),
    review_text = str_remove_all(reviews$review_text, "goodreads.com*"),
    # remove html stuff
    review_text = str_remove_all(reviews$review_text, "amp"),
    review_text = str_remove_all(reviews$review_text, "&S*"),
    review_text = str_remove_all(reviews$review_text, "&#x27;|&quot;|&#x2F;"),
    review_text = str_remove_all(reviews$review_text, "<a(.*?)>"),
    review_text = str_remove_all(reviews$review_text, "<a[^>]*>[^<]*</a>"),
    review_text = str_remove_all(reviews$review_text, "&gt;|&lt;|&amp;"),
    review_text = str_remove_all(reviews$review_text, "&#[:digit:]+;"),
    review_text = str_remove_all(reviews$review_text, "<[^>]*>"),
    # remove numbers and special characters
    review_text = str_remove_all(reviews$review_text, "[:digit:]"),
    review_text = str_remove_all(reviews$review_text, "[^[:alpha:]\\s]+"),
    # remove excess whitespace
    review_text = str_squish(review_text),
    review_text = str_trim(review_text)
    )

# Tokenize the reviews using the unnest_tokens() function
reviews_tokens <- goodreads_full %>%
  unnest_tokens(word, format = "text", review_text)

# --- Remove Stopwords --- #
tidy_reviews <-
  reviews_tokens %>%
  anti_join((stop_words))

# Calculate the sentiment scores using afinn
tidy_afinn <-
  tidy_reviews %>%
  left_join(get_sentiments("afinn"))

# Calculate the AFINN sentiment score per review 
reviews_sentiment_afinn <- tidy_afinn %>%
  group_by(Book_Id, review_id) %>%
  summarise(sentiment_afinn_mean = ifelse(is.nan(mean(value, na.rm = TRUE)), NA, mean(value, na.rm = TRUE)))

# Calculate the AFINN sentiment scores per book 
book_sentiment_afinn <- reviews_sentiment_afinn %>%
  group_by(Book_Id) %>% 
  summarise(AFINN_mean = mean(sentiment_afinn_mean, na.rm = TRUE))

goodreads_full <- na.omit(goodreads_full, cols ="review_text")

gr_booksentiment <- full_join(goodreads_full, reviews_sentiment_afinn, by = "review_id")
gr_booksentiment <- gr_booksentiment %>%
  group_by(Book_Id.x) %>%
  summarise(AFINN_pretreat = mean(sentiment_afinn_mean[pre_treatment == 1], na.rm = TRUE),
            AFINN_posttreat = mean(sentiment_afinn_mean[post_treatment == 1], na.rm = TRUE)) %>%
  rename("Book_Id" = "Book_Id.x")

# add each book average to each book
gr_booksentiment <- full_join(book_sentiment_afinn, gr_booksentiment, by = "Book_Id")

### create file for sentiment analyses
goodreads_sentiment <- left_join(goodreads_questions, gr_booksentiment, by = "Book_Id")
goodreads_sentiment <- goodreads_sentiment %>%
  mutate(Likes = replace_na(Likes, 0)) %>%
  mutate(Number_of_Answers = replace_na(Number_of_Answers, 0)) %>%
  select(!Question) %>%
  select(!Date_of_Question) %>%
  select(!Scraping_Date) %>%
  select(!Question_Timestamp) %>%
  arrange(Book_Id)

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
