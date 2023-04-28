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
# --- MERGING Answers --- #
goodreads_binded <- bind_rows(subset_answers,web_archive)

# merged <- full_join(df, cleaned_timestamps, by = "Book_Id", relationship = "many-to-many")
# merged <- merged %>% drop_na(exact_question_timestamp)
# merged <- merged[!duplicated(merged$Book_Id),]


##### 
# read subsetgoodreads reviews
df_goodreads_reviews <- readRDS("dat/qa_subset_goodreads_text_merged.RDS")

# create new variable and convert string to date
gr_reviews_clean_timestamps <- df_goodreads_reviews %>%
  mutate(date_added = as.character(date_added)) %>%
  mutate(clean_added = str_sub(date_added, start = 5, end = 10)) %>%
  mutate(year_added = str_sub(date_added, start = 27, end = 30)) %>%
  mutate(clean_added = paste(clean_added, year_added)) %>%
  select(!year_added) %>%
  mutate(clean_added = str_replace(clean_added, "Jan", "01")) %>%
  mutate(clean_added = str_replace(clean_added, "Feb", "02")) %>%
  mutate(clean_added = str_replace(clean_added, "Mar", "03")) %>%
  mutate(clean_added = str_replace(clean_added, "Apr", "04")) %>%
  mutate(clean_added = str_replace(clean_added, "May", "05")) %>%
  mutate(clean_added = str_replace(clean_added, "Jun", "06")) %>%
  mutate(clean_added = str_replace(clean_added, "Jul", "07")) %>%
  mutate(clean_added = str_replace(clean_added, "Aug", "08")) %>%
  mutate(clean_added = str_replace(clean_added, "Sep", "09")) %>%
  mutate(clean_added = str_replace(clean_added, "Oct", "10")) %>%
  mutate(clean_added = str_replace(clean_added, "Nov", "11")) %>%
  mutate(clean_added = str_replace(clean_added, "Dec", "12")) %>%
  mutate(clean_added = strptime(clean_added, format = "%m%d%Y")) %>%
  mutate(clean_added = format(clean_added, format = "%m-%d-%Y"))

# i guess this merge is wrong and not necessary
# goodreads_full <- merge(goodreads_binded, gr_reviews_clean_timestamps, by.x = "Book_Id", by.y = "book_id", all.x = TRUE, all.y = TRUE)