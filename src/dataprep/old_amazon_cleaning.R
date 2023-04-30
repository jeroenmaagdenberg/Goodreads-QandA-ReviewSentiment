#####
# open the packages
library(tidyverse)
library(data.table)
library(dplyr)
library(gsubfn)
library(stringr)

setwd('~/Documents/GitHub/Goodreads-QandA-ReviewSentiment')

##### 
# read subset goodreads
df_goodreads_reviews <- readRDS("dat/qa_subset_goodreads_text_merged.RDS")


# compute unique book ids
gr_unique_book_ids <- df_goodreads_reviews[!duplicated(df_goodreads_reviews$book_id),] %>%
  select(book_id)

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

# write cleaned df to csv
write.csv(gr_reviews_clean_timestamps, "~/Documents/GitHub/Goodreads-QandA-ReviewSentiment/dat/goodreads_reviews_clean.csv", row.names = FALSE)

#### amazon
# open subset amazon
df_amazon_reviews <- readRDS("dat/qa_subset_amazon_text_merged.RDS")
am_unique_book_ids <- df_amazon_reviews[!duplicated(df_amazon_reviews$asin),] %>%
  select(asin)