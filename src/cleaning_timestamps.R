library(tidyverse)
library(data.table)
library(dplyr)
library(gsubfn)
library(stringr)

setwd('/Users/jeroenm/Documents/GitHub/Goodreads-QandA-ReviewSentiment')

df <- fread("dat/2864_goodreads_com_book_full.csv")

df <- df %>%
  filter(Date_of_Question != "")

# subset to rows that contain "day" or "month" in Date_of_Question
subset_df <- df[grep("day|month", df$Date_of_Question)]

# replace "months" with equivalent number of days
subset_df$Date_of_Question <- gsubfn("(\\d+) months ago", ~paste0(as.numeric(x) * 30, " days ago"), subset_df$Date_of_Question)

# convert scraping_date to Date object
subset_df$Scrapting_Date <- as.Date(subset_df$Scrapting_Date)

# extract number of days from Date_of_Question and subtract from scrapting_date for exact timestamp
subset_df$exact_question_timestamp <- subset_df$Scrapting_Date - as.numeric(gsub("\\D", "", subset_df$Date_of_Question))
#### or should i put this in the original date_of_question column? 

#####
# subset to rows that contain "year" in Date_of_Question
#subset2_df <- df[grep("year", df$Date_of_Question)] #not used

web_archive <- fread("dat/2864_web_archive_org_one_year_level.csv")
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
#### or should i put this in the original date_of_question column? 

#####
# --- MERGING --- #
merged <- left_join(web_archive, subset_df, by = "Book_Id")
View(merged)
