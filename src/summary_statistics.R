library(wordcloud)

# summary statistics - AFINN

mean_val <- mean(reviews_sentiment_afinn$sentiment_afinn2, na.rm = TRUE)
sd_val <- sd(reviews_sentiment_afinn$sentiment_afinn2, na.rm = TRUE)
max_val <- max(reviews_sentiment_afinn$sentiment_afinn2, na.rm = TRUE)
min_val <- min(reviews_sentiment_afinn$sentiment_afinn2, na.rm = TRUE)

mean_val
sd_val
max_val
min_val


# Compute sample size
n <- nrow(reviews_sentiment_afinn)


# plot- AFINN
word_counts <- reviews_tokens %>%
  count(word, sort = TRUE) %>%
  filter(n > 10)

ggplot(word_counts, aes(x = n, y = reorder(word, n))) +
  geom_point() +
  scale_x_log10() +
  xlab("Word Count (log scale)") +
  ylab("Words")


# summary statistics - Vader
mean_vader <- mean(vader_sent$compound, na.rm = TRUE)
sd_vader <- sd(c, na.rm = TRUE)
ax_val <- max(vader_sent$compound, na.rm = TRUE)
min_val <- min(vader_sent$compound, na.rm = TRUE)

n_vader <- table(vader_sent$vader_sent)
n_vader

# plot - vader



