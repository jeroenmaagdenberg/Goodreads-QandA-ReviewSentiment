#####
# open the packages
library(tidyverse)
library(data.table)
library(dplyr)
library(gsubfn)
library(stringr)

setwd('~/Documents/GitHub/Goodreads-QandA-ReviewSentiment')

#####
### convert months to days
# read the dataset

df_goodreads_books <- fread("dat/2864_goodreads_com_book_full.csv")

# remove all observations with no value in Date_of_Question column
subset_answers <- df_goodreads_books %>%
  filter(Date_of_Question != "")

# compute unique book ids for day/month file
unique_book_Ids <- subset_answers[!duplicated(subset_answers$Book_Id),] %>%
  select(Book_Id)

# subset to rows that contain "day" or "month" in Date_of_Question
subset_answers <- subset_answers[grep("day|month", subset_answers$Date_of_Question)]

# replace "months" with equivalent number of days
subset_answers$Date_of_Question <- gsubfn("(\\d+) months ago", ~paste0(as.numeric(x) * 30, " days ago"), subset_answers$Date_of_Question)

# convert scraping_date to Date object
subset_answers$Scrapting_Date <- as.Date(subset_answers$Scrapting_Date)

# extract number of days from Date_of_Question and subtract from scrapting_date for exact timestamp
subset_answers$exact_question_timestamp <- subset_answers$Scrapting_Date - as.numeric(gsub("\\D", "", subset_answers$Date_of_Question))

#####
### convert years to days

web_archive <- fread("dat/2864_web_archive_org_one_year_level.csv")

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

# extract number of days from Date_of_Question and subtract from scrapting_date for exact timestamp
web_archive$exact_question_timestamp <- web_archive$Url_Timestamp - as.numeric(gsub("\\D", "", web_archive$Question_Timestamp))

# create df with just Ids and timestamps
cleaned_timestamps <- web_archive %>%
  select(Book_Id, exact_question_timestamp) %>%
  bind_rows(subset_answers %>%
              select(Book_Id, exact_question_timestamp))

#####
# --- MERGING --- #
binded <- bind_rows(subset_answers,web_archive)

merged <- full_join(df, cleaned_timestamps, by = "Book_Id", relationship = "many-to-many")
merged <- merged %>%
  drop_na(exact_question_timestamp)
merged <- merged[!duplicated(merged$Book_Id),]
