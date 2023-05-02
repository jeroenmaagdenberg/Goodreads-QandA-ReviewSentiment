
### accuracy 
# library(yardstick)
# conf_mat(reviews_sentiment_afinn,
#          afinn,
#          afinn2,
#          dnn = c("afinn_sum","afinn_mean")
# )
# 
# accuracy(reviews_sentiment_afinn,
#          as.factor(afinn),
#          as.factor(afinn2)
# )
# 
# 
# ### check for removed reviews due to the cleaning process
# # check <- full_join(reviews, reviews_sentiment_afinn, by = "review_id") %>% filter(is.na(sentiment_afinn))


#####
# vader # doesnt work correctly
vader_sent <- vader_df(reviews$review_text)
vader_sent <- vader_sent %>%
  mutate(
    vader_sent = case_when(
      compound > 0.05 ~ "positive",
      compound < -0.05 ~ "negative",
      TRUE ~ "neutral")
  )

book_sentiment_vader <- reviews %>%
  group_by(book_id) %>%
  mutate(vader_sent = vader_df(reviews$review_text))





# fwrite(goodreads_reviews, "gen/dataprep/goodreads_reviews.csv", row.names = FALSE) # just i