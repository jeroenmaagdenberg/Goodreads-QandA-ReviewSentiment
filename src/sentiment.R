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
# reviews <- fread("dat/goodreads_reviews_clean.csv")

reviews <- head(goodreads_reviews, 1000)
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
reviews_tokens <- reviews %>%
  unnest_tokens(word, format = "text", review_text)

# --- Remove Stopwords --- #
tidy_reviews <-
  reviews_tokens %>%
  anti_join((stop_words))

# Calculate the sentiment scores using afinn
tidy_afinn <-
  tidy_reviews %>%
  left_join(get_sentiments("afinn"))


# Calculate the sentiment scores per book using afinn

book_sentiment_afinn <-
  tidy_afinn %>%
  group_by(book_id) %>% 
  summarise(sentiment_afinn_mean = mean(value, na.rm = TRUE)) %>%
  mutate(
    afinn = case_when(
      sentiment_afinn_mean > 0 ~ "positive",
      sentiment_afinn_mean < 0 ~ "negative",
      TRUE ~ "neutral"
    )
  )

# add each average to each book
gr_questions_reviews_sent <- full_join(goodreads_full, book_sentiment_afinn, by = c("Book_Id" = "book_id"))




#####
# vader
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



#####
reviews_sentiment_afinn <- tidy_afinn %>%
  group_by(book_id, review_id) %>%
  summarise(sentiment_afinn = sum(value, na.rm = TRUE),
            sentiment_afinn_mean = mean(value, na.rm = TRUE)) %>%
  mutate(
    afinn = case_when(
      sentiment_afinn > 0 ~ "positive",
      sentiment_afinn < 0 ~ "negative",
      TRUE ~ "neutral"
    ),
    afinn2 = case_when(
      sentiment_afinn_mean > 0 ~ "positive",
      sentiment_afinn_mean < 0 ~ "negative",
      TRUE ~ "neutral"
    )
  )

gr_questions_reviews_sent2 <- full_join(goodreads_full, reviews_sentiment_afinn, by = "review_id")


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
