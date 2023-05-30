# open the packages
library(tidyverse)
library(data.table)
library(dplyr)
library(gsubfn)
library(stringr)

setwd('~/Documents/GitHub/Goodreads-QandA-ReviewSentiment')

#### subset ####
### convert months to days
# read the dataset
goodreads_books <- fread("dat/2864_goodreads_com_book_full.csv")

# remove unnecessary columns and rename Scrapting_Date column
goodreads_books <- goodreads_books %>%
  select(!Star_rating) %>%
  select(!Question) %>%
  select(!Number_of_reviews) %>%
  select(!Total_Number_of_Questions) %>%
  select(!Number_of_ratings) %>%
  select(!Genres_displayed) %>%
  select(!Kindle_Store_price) %>%
  select(!Answer_Url) %>%
  select(!Searched_Url) %>%
  select(!Number_of_Comments_for_Answer) %>%
  select(!Answer) %>%
  select(!Date_of_Answer) %>%
  select(!Answer_Likes) %>%
  select(!Isbn13) %>%
  rename(Scraping_Date = Scrapting_Date)

# create a list with books and their respective likes and number of answers
book_likes_and_num_answers <- goodreads_books %>%
  select(!Scraping_Date) %>%
  select(!Title) %>%
  select(!Date_of_Question) %>%
  filter(Number_of_Answers >= 0) %>%
  mutate(Likes = replace_na(Likes, 0)) %>%
  mutate(Likes = as.numeric(Likes)) %>%
  mutate(Number_of_Answers = replace_na(Number_of_Answers, 0)) %>%
  mutate(Number_of_Answers = as.numeric(Number_of_Answers)) %>%
  arrange(Book_Id) %>%
  distinct(.keep_all = TRUE)

# remove all observations with no value in Date_of_Question column and all duplicates
subset_questions <- goodreads_books %>%
  filter(Date_of_Question != "") %>%
  select(!Likes) %>%
  select(!Number_of_Answers) %>%
  distinct(.keep_all = TRUE) 

# subset to rows that contain "day" or "month" in Date_of_Question
subset_questions <- subset_questions[grep("day|month", subset_questions$Date_of_Question)]

# replace "months" with equivalent number of days
subset_questions$Date_of_Question <- gsubfn("(\\d+) months ago", ~paste0(as.numeric(x) * 30, " days ago"), subset_questions$Date_of_Question)

# convert scraping_date to Date object
subset_questions$Scraping_Date <- as.Date(subset_questions$Scraping_Date)

# extract number of days from Date_of_Question and subtract from Scraping_Date for exact timestamp
subset_questions$exact_question_timestamp <- subset_questions$Scraping_Date - as.numeric(gsub("\\D", "", subset_questions$Date_of_Question))

#### web_archive ####
### convert years to days
# open file
web_archive <- fread("dat/2864_web_archive_org_one_year_level.csv")

# delete unnecessary columns and transform Scraping_Date column to date
web_archive <- web_archive %>%
  select(!Url) %>%
  select(!Question) %>%
  select(!Searched_Url) %>%
  select(!Isbn13) %>%
  mutate(Scraping_Date = as.Date(Scraping_Date))

# transform Url_timestamp to date
web_archive <- web_archive %>%
  mutate(Url_Timestamp = as.character(Url_Timestamp)) %>%
  mutate(Url_Timestamp = str_sub(Url_Timestamp, start = 1, end = 8)) %>%
  mutate(Url_Timestamp = strptime(Url_Timestamp, format = "%Y%m%d")) %>%
  mutate(Url_Timestamp = format(Url_Timestamp, "%Y-%m-%d"))
web_archive$Url_Timestamp <- as.Date(web_archive$Url_Timestamp)

# replace "months" with equivalent number of days
web_archive$Question_Timestamp <- gsubfn("(\\d+) months ago", ~paste0(as.numeric(x) * 30, " days ago"), web_archive$Question_Timestamp)

# replace "years" with equivalent number of days
web_archive$Question_Timestamp <- sub("one year ago", "365 days ago", web_archive$Question_Timestamp)

# extract number of days from Date_of_Question and subtract from Scraping_date for exact timestamp
web_archive$exact_question_timestamp <- web_archive$Url_Timestamp - as.numeric(gsub("\\D", "", web_archive$Question_Timestamp))


# Merge Questions
goodreads_questions <- bind_rows(subset_questions,web_archive)

# add the likes and number of answers to the questions
goodreads_questions <- goodreads_questions %>%
  left_join(book_likes_and_num_answers, by = "Book_Id") %>%
  arrange(Book_Id)

# clean environment
rm(subset_questions, web_archive, book_likes_and_num_answers)

# write merged file to csv
write.csv(goodreads_questions, "gen/dataprep/goodreads_questions.csv", row.names = FALSE)
