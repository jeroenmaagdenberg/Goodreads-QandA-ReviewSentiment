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
goodreads_books <- fread("dat/2864_goodreads_com_book_full.csv")

# remove unnecessary columns and rename Scrapting_Date column
goodreads_books <- goodreads_books %>%
  select(!Star_rating) %>%
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
  rename(Scraping_Date = Scrapting_Date)

# remove all observations with no value in Date_of_Question column
subset_answers <- goodreads_books %>%
  filter(Date_of_Question != "")

# compute unique book ids for day/month file
# unique_book_Ids <- subset_answers[!duplicated(subset_answers$Book_Id),] %>%
#   select(Book_Id)

# subset to rows that contain "day" or "month" in Date_of_Question
subset_answers <- subset_answers[grep("day|month", subset_answers$Date_of_Question)]

# replace "months" with equivalent number of days
subset_answers$Date_of_Question <- gsubfn("(\\d+) months ago", ~paste0(as.numeric(x) * 30, " days ago"), subset_answers$Date_of_Question)

# convert scraping_date to Date object
subset_answers$Scraping_Date <- as.Date(subset_answers$Scraping_Date)

# extract number of days from Date_of_Question and subtract from Scraping_Date for exact timestamp
subset_answers$exact_question_timestamp <- subset_answers$Scraping_Date - as.numeric(gsub("\\D", "", subset_answers$Date_of_Question))

subset_answers <- subset_answers %>%
  distinct(.keep_all = TRUE)

# dupes <- duplicated(subset_answers$Book_Id)
# dupe <- subset_answers[dupes, drop = FALSE]

#####
### convert years to days
# open file
web_archive <- fread("dat/2864_web_archive_org_one_year_level.csv")

# delete unnecessary columns and transform Scraping_Date column to date
web_archive <- web_archive %>%
  select(!Url) %>%
  select(!Searched_Url) %>%
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

# create df with just Ids and timestamps
cleaned_timestamps <- web_archive %>%
  select(Book_Id, exact_question_timestamp) %>%
  bind_rows(subset_answers %>%
              select(Book_Id, exact_question_timestamp))


#####
# --- MERGING Answers --- #
goodreads_questions <- bind_rows(subset_answers,web_archive)

# write merged file to csv
write.csv(goodreads_questions, "gen/dataprep/goodreads_questions.csv")




# merged <- inner_join(df, cleaned_timestamps, by = "Book_Id", relationship = "many-to-many")
# merged <- merged %>% drop_na(exact_question_timestamp)
# merged <- merged[!duplicated(merged$Book_Id),]


##### 
# read subset goodreads reviews
goodreads_reviews <- readRDS("dat/qa_subset_goodreads_text_merged.RDS")

# remove unnecessary columns
goodreads_reviews <- goodreads_reviews %>%
  select(!date_updated) %>%
  select(!user_id)

goodreads_reviews <- goodreads_reviews %>%
  mutate(
    # remove links
    review_text = str_remove_all(goodreads_reviews$review_text, "https\\S*"),
    review_text = str_remove_all(goodreads_reviews$review_text, "http\\S*"),
    review_text = str_remove_all(goodreads_reviews$review_text, "goodreads.com*"),
    # remove html stuff
    review_text = str_remove_all(goodreads_reviews$review_text, "amp"),
    review_text = str_remove_all(goodreads_reviews$review_text, "&S*"),
    review_text = str_remove_all(goodreads_reviews$review_text, "&#x27;|&quot;|&#x2F;"),
    review_text = str_remove_all(goodreads_reviews$review_text, "<a(.*?)>"),
    review_text = str_remove_all(goodreads_reviews$review_text, "<a[^>]*>[^<]*</a>"),
    review_text = str_remove_all(goodreads_reviews$review_text, "&gt;|&lt;|&amp;"),
    review_text = str_remove_all(goodreads_reviews$review_text, "&#[:digit:]+;"),
    review_text = str_remove_all(goodreads_reviews$review_text, "<[^>]*>"),
    # remove numbers and special characters
    review_text = str_remove_all(goodreads_reviews$review_text, "[:digit:]"),
    review_text = str_remove_all(goodreads_reviews$review_text, "[^[:alpha:]\\s]+"),
    # remove excess whitespace
    review_text = str_squish(review_text),
    review_text = str_trim(review_text)
  )

# replace review_text with only white spaces with NA and get rid of all NA reviews
goodreads_reviews$review_text[goodreads_reviews$review_text == ""] <- NA
goodreads_reviews <- na.omit(goodreads_reviews, cols ="review_text")

# create new variable and convert string to date
goodreads_reviews <- goodreads_reviews %>%
  mutate(date_added = as.character(date_added)) %>%
  mutate(date = str_sub(date_added, start = 5, end = 10)) %>%
  mutate(year_added = str_sub(date_added, start = 27, end = 30)) %>%
  mutate(date = paste(date, year_added)) %>%
  select(!year_added) %>%
  mutate(date = str_replace(date, "Jan", "01")) %>%
  mutate(date = str_replace(date, "Feb", "02")) %>%
  mutate(date = str_replace(date, "Mar", "03")) %>%
  mutate(date = str_replace(date, "Apr", "04")) %>%
  mutate(date = str_replace(date, "May", "05")) %>%
  mutate(date = str_replace(date, "Jun", "06")) %>%
  mutate(date = str_replace(date, "Jul", "07")) %>%
  mutate(date = str_replace(date, "Aug", "08")) %>%
  mutate(date = str_replace(date, "Sep", "09")) %>%
  mutate(date = str_replace(date, "Oct", "10")) %>%
  mutate(date = str_replace(date, "Nov", "11")) %>%
  mutate(date = str_replace(date, "Dec", "12")) %>%
  mutate(date = strptime(date, format = "%m%d%Y")) %>%
  mutate(date = format(date, format = "%Y-%m-%d"))
goodreads_reviews$date <- as.Date(goodreads_reviews$date)

write.csv(goodreads_reviews, "gen/dataprep/goodreads_reviews.csv")

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


# nas <- is.na(goodreads_full$post_treatment)
# na <- goodreads_full[nas, drop = FALSE]
# sum(duplicated(goodreads_full$review_id))
# dupes <- duplicated(goodreads_full$review_id)
# dupe <- goodreads_full[dupes, drop = FALSE]
