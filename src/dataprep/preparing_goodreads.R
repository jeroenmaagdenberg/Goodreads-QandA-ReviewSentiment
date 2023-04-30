library(tidyverse)
library(data.table)

goodreads_questions <- fread("gen/dataprep/goodreads_questions.csv")
goodreads_reviews <- readRDS("gen/dataprep/goodreads_reviews.RDS")

# merge questions and reviews
goodreads_full <- merge(goodreads_questions, goodreads_reviews, by.x = "Book_Id", by.y = "book_id", all.x = TRUE, all.y = TRUE)

# prepare merged dataframe for analyses
goodreads_full <- goodreads_full %>%
  mutate(Likes = replace_na(Likes, 0)) %>%
  mutate(Number_of_Answers = replace_na(Number_of_Answers, 0))

# create dummy for treatments
goodreads_full$date <- as.Date(goodreads_full$date)
goodreads_full$exact_question_timestamp <- as.Date(goodreads_full$exact_question_timestamp)

goodreads_full$pre_treatment <- ifelse(goodreads_full$date < goodreads_full$exact_question_timestamp, 1, 0)
goodreads_full$pre_treatment[is.na(goodreads_full$pre_treatment)] <- 0

goodreads_full$post_treatment <- ifelse(goodreads_full$date > goodreads_full$exact_question_timestamp, 1, 0)
goodreads_full$post_treatment[is.na(goodreads_full$post_treatment)] <- 0

# prepare
goodreads_full <- na.omit(goodreads_full, cols ="review_text")

goodreads_full <- goodreads_full %>%
  select(!Question) %>%
  select(!Date_of_Question) %>%
  select(!Scraping_Date) %>%
  select(!Question_Timestamp) %>%
  select(!Url_Timestamp) %>%
  select(!date_added)


# nas <- is.na(goodreads_full$post_treatment)
# na <- goodreads_full[nas, drop = FALSE]
# sum(duplicated(goodreads_full$review_id))
# dupes <- duplicated(goodreads_full$review_id)
# dupe <- goodreads_full[dupes, drop = FALSE]