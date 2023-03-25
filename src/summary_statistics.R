

# summary statistics - AFINN

mean_val <- mean(reviews_sentiment_afinn$sentiment_afinn2, na.rm = TRUE)
sd_val <- sd(reviews_sentiment_afinn$sentiment_afinn2, na.rm = TRUE)
max_val <- max(reviews_sentiment_afinn$sentiment_afinn2, na.rm = TRUE)
min_val <- min(reviews_sentiment_afinn$sentiment_afinn2, na.rm = TRUE)

# Compute sample size
n <- nrow(reviews_sentiment_afinn)
