library(tidyverse)
library(data.table)
library(tidyr)

df_goodreads_books <- fread("dat/2864_goodreads_com_book_full.csv")

goodreads_questions <- df_goodreads_books %>%
  filter(Date_of_Question != "")

### reduce size dataframe
# drop unnecessary columns
goodreads_final <- goodreads_questions %>%
  select(!Star_rating) %>%
  select(!Number_of_ratings) %>%
  select(!Genres_displayed) %>%
  select(!Kindle_Store_price) %>%
  select(!Answer_Url) %>%
  select(!Searched_Url) %>%
  select(!Number_of_Comments_for_Answer) %>%
  select(!Answer) %>%
  select(!Date_of_Answer) %>%
  select(!Answer_Likes)

goodreads_final <- goodreads_final %>%
  distinct(.keep_all = TRUE)

### prepare data for analyses
# NAs to 0s
goodreads_final <- goodreads_final %>%
  mutate(Likes = replace_na(Likes, 0)) %>%
  mutate(Number_of_Answers = replace_na(Number_of_Answers, 0)) %>%
  mutate(QA_present = ifelse(Total_Number_of_Questions  >= 1, 1, 0))

