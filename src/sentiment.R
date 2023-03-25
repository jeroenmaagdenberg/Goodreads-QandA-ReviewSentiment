library(tidytext)
library(dplyr)
library(vader)
library(tokenizers)
library(readr)

setwd("..")
setwd("..")
setwd("..")
setwd("~/Downloads")

# Load the data frame of reviews
reviews <- fread("goodreads_reviews.csv", nrows = 10000)
reviews <- reviews %>%
  filter(review_text_incomplete != "")
reviews$review_text_incomplete <- tolower(as.character(reviews$review_text_incomplete))

get_sentiments("afinn")




# Tokenize the reviews using the unnest_tokens() function
reviews_tokens <- reviews %>%
  unnest_tokens(bigram, format = "text", review_text_incomplete)

# --- Remove Stopwords --- #
tidy_reviews <-
  reviews_tokens %>%
  anti_join((stop_words))

# Calculate the sentiment scores using afinn
tidy_afinn <-
  tidy_reviews %>%
  left_join(get_sentiments("afinn"))

reviews_sentiment_afinn <-
  tidy_afinn %>%
  group_by(review_id) %>% 
  summarise(sentiment_afinn = sum(value, na.rm = TRUE),
            sentiment_afinn2 = mean(value, na.rm = TRUE)) %>%
  mutate(
    afinn = case_when(
      sentiment_afinn > 0 ~ "positive",
      sentiment_afinn < 0 ~ "negative",
      TRUE ~ "neutral"
    ),
    afinn2 = case_when(
      sentiment_afinn2 > 0 ~ "positive",
      sentiment_afinn2 < 0 ~ "negative",
      TRUE ~ "neutral"
    )
  )


check <- full_join(reviews, reviews_sentiment_afinn, by = "review_id") %>%
  filter(is.na(sentiment_afinn))


### accuracy 
library(yardstick)
conf_mat(reviews_sentiment_afinn,
         afinn,
         afinn2,
         dnn = c("afinn_sum","afinn_mean")
)

accuracy(reviews_sentiment_afinn,
         as.factor(afinn),
         as.factor(afinn2)
)


#####
# vader
vader_sent <- vader_df(reviews$review_text_incomplete)
vader_sent <- vader_sent %>%
  mutate(
    vader_sent = case_when(
      compound > 0.05 ~ "positive",
      compound < -0.05 ~ "negative",
      TRUE ~ "neutral")
  )

