# open the packages
library(tidyverse)
library(stringr)
library(data.table)

setwd('~/Documents/GitHub/Goodreads-QandA-ReviewSentiment')

# read goodreads reviews 
df_amazon_reviews <- readRDS("dat/qa_subset_amazon_text_merged.RDS")

# look at the years of reviews
test <- df_amazon_reviews %>%
  select(unixReviewTime) %>%
  mutate(Year = as.POSIXct(unixReviewTime, origin = "1970-01-01", tz = "UTC")) %>%
  mutate(Year = str_sub(Year, start = 1, end = 4)) %>%
  select(!unixReviewTime)
table(test)

# read overlap file
overlap_titles <- read.table("dat/overlap_titles_amazon_gr.txt", header = TRUE)

overlap_titles <- overlap_titles %>%
  mutate(book_id = as.numeric(book_id)) %>%
  mutate(asin = as.character(asin)) %>%
  arrange(book_id) %>%
  rename("Book_Id" = "book_id")

booklist <- goodreads_questions %>%
  select(Book_Id) %>%
  distinct() 

# due to hardware constraints, take a proportion of the GR books 
amazon_booklist <- booklist %>%
  left_join(overlap_titles, by = "Book_Id") %>%
  select(Book_Id, asin) %>%
  rename("book_id" = "Book_Id") %>%
  distinct(asin, .keep_all = TRUE) %>%
  sample_frac(size = 0.5, replace = FALSE)


# keep only necessary columns and create review_ID
amazon_reviews <- df_amazon_reviews %>% 
  mutate(Amazon_ReviewId = row_number()) %>%
  select(!summary) %>%
  select(!overall) %>%
  rename("review_text" = "reviewText") %>%
  mutate(review_date = as.POSIXct(unixReviewTime, origin = "1970-01-01", tz = "UTC") %>% 
           format("%Y-%m-%d")) %>%
  select(!unixReviewTime) 

amazon_reviews <- left_join(amazon_booklist, amazon_reviews, by = "asin")
amazon_reviews <- amazon_reviews %>%
  distinct(review_text, reviewerID, .keep_all = TRUE) %>%
  select(!reviewerID)



# replace review_text with only white spaces with NA and get rid of all NA reviews
amazon_reviews$review_text[amazon_reviews$review_text == ""] <- NA
amazon_reviews <- na.omit(amazon_reviews, cols ="review_text")

amazon_reviews <- amazon_reviews %>%
  mutate(review_text = str_remove_all(review_text, "(http|https)://\\S+|Amazon\\.com|amazon\\.com|&\\S*;|<[^>]*>|[^[:alpha:]\\s]+|[[:digit:]]+")) %>%
  mutate(review_text = str_squish(review_text))


# replace review_text with only white spaces with NA and get rid of all NA reviews
amazon_reviews$review_text[amazon_reviews$review_text == ""] <- NA
amazon_reviews <- na.omit(amazon_reviews, cols ="review_text")

am_clean <- amazon_reviews 

saveRDS(am_clean, "gen/dataprep/amazon_reviews.RDS", compress = TRUE)




