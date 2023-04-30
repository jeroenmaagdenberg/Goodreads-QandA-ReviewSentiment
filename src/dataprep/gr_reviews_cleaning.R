# open the packages
library(tidyverse)
library(stringr)
library(data.table)

setwd('~/Documents/GitHub/Goodreads-QandA-ReviewSentiment')

# read subset goodreads reviews
goodreads_reviews <- readRDS("dat/qa_subset_goodreads_text_merged.RDS")

# remove unnecessary columns
goodreads_reviews <- goodreads_reviews %>%
  select(!date_updated) %>%
  select(!user_id) %>%
  select(!rating)

# goodreads_reviews <- head(goodreads_reviews, 10000)

# replace review_text with only white spaces with NA and get rid of all NA reviews
goodreads_reviews$review_text[goodreads_reviews$review_text == ""] <- NA
goodreads_reviews <- na.omit(goodreads_reviews, cols ="review_text")

# clean review_text
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

# fwrite(goodreads_reviews, "gen/dataprep/goodreads_reviews.csv", row.names = FALSE)
saveRDS(goodreads_reviews, "gen/dataprep/goodreads_reviews.RDS", compress = TRUE)
