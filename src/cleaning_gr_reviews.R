library(data.table)
library(dplyr)
library(stringr)
library(readr)

# Load the data frame of reviews
df_reviews <- fread("~/Downloads/goodreads_reviews.csv", nrows = 10000)
# df_reviews <- fread("goodreads_reviews_clean.csv")

reviews <- df_reviews %>%
  filter(review_text_incomplete != "")

reviews$review_text_incomplete <- tolower(as.character(reviews$review_text_incomplete))

# clean reviews
reviews <- reviews %>%
  mutate(
    # remove links
    review_text_incomplete = str_remove_all(reviews$review_text_incomplete, "https\\S*"),
    review_text_incomplete = str_remove_all(reviews$review_text_incomplete, "http\\S*"),
    review_text_incomplete = str_remove_all(reviews$review_text_incomplete, "goodreads.com*"),
    # remove html stuff
    review_text_incomplete = str_remove_all(reviews$review_text_incomplete, "amp"),
    review_text_incomplete = str_remove_all(reviews$review_text_incomplete, "&S*"),
    review_text_incomplete = str_remove_all(reviews$review_text_incomplete, "&#x27;|&quot;|&#x2F;"),
    review_text_incomplete = str_remove_all(reviews$review_text_incomplete, "<a(.*?)>"),
    review_text_incomplete = str_remove_all(reviews$review_text_incomplete, "<a[^>]*>[^<]*</a>"),
    review_text_incomplete = str_remove_all(reviews$review_text_incomplete, "&gt;|&lt;|&amp;"),
    review_text_incomplete = str_remove_all(reviews$review_text_incomplete, "&#[:digit:]+;"),
    review_text_incomplete = str_remove_all(reviews$review_text_incomplete, "<[^>]*>"),
    # remove numbers
    review_text_incomplete = str_remove_all(reviews$review_text_incomplete, "[:digit:]"),
    # remove excess whitespace
    review_text_incomplete = str_squish(review_text_incomplete),
    review_text_incomplete = str_trim(review_text_incomplete))


# write cleaned df to csv
write.csv(reviews, "~/Documents/GitHub/Goodreads-QandA-ReviewSentiment/dat/goodreads_reviews_clean.csv", row.names = FALSE)


