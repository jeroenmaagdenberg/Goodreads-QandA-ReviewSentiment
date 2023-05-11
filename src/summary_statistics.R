library(wordcloud)

# summary statistics - Goodreads
goodreads_books <- fread("dat/2864_goodreads_com_book_full.csv")
web_archive <- fread("dat/2864_web_archive_org_one_year_level.csv")
test <- readRDS("dat/qa_subset_goodreads_text_merged.RDS")



# summary statistics

# compute unique book ids
unique_book_ids <- goodreads_books[!duplicated(goodreads_books$Book_Id),] %>%
  select(Book_Id) # 100034 books
unique_book_ids2 <- web_archive[!duplicated(web_archive$Book_Id),] %>%
  select(Book_Id) # 12139 books
gr_unique_book_ids <- goodreads_full[!duplicated(goodreads_full$Book_Id),] %>%
  select(Book_Id) # 14123 books with questions and reviews

tidy_afinn %>% count(word, sort = T)


# summary statistics - AFINN

gr_AFINN_mean <- round(mean(goodreads_r_sentiment$AFINN_score, na.rm = TRUE), 4)
gr_AFINN_sd <- round(sd(goodreads_r_sentiment$AFINN_score, na.rm = TRUE), 4)
gr_AFINN_max <- round(max(goodreads_r_sentiment$AFINN_score, na.rm = TRUE), 4)
gr_AFINN_min <- round(min(goodreads_r_sentiment$AFINN_score, na.rm = TRUE), 4)

# summary statistics - Likes

gr_likes_mean <- round(mean(goodreads_b_sentiment$Likes, na.rm = TRUE), 4)
gr_likes_sd <- round(sd(goodreads_b_sentiment$Likes, na.rm = TRUE), 4)
gr_likes_max <- round(max(goodreads_b_sentiment$Likes, na.rm = TRUE), 4)
gr_likes_min <- round(min(goodreads_b_sentiment$Likes, na.rm = TRUE), 4)


# summary statistics - Number of Answers

gr_num_answ_mean <- round(mean(goodreads_b_sentiment$Number_of_answers, na.rm = TRUE), 4)
gr_num_answ_sd <- round(sd(goodreads_b_sentiment$Number_of_answers, na.rm = TRUE), 4)
gr_num_answ_max <- round(max(goodreads_b_sentiment$Number_of_answers, na.rm = TRUE), 4)
gr_num_answ_min <- round(min(goodreads_b_sentiment$Number_of_answers, na.rm = TRUE), 4)

# overview treated vs non-treated (before / after top question)
table(goodreads_r_sentiment$treated )

# treated AFINN statistics (before / after top question)
AFINN_sd_pre <- round(sd(subset(goodreads_r_sentiment, treated == 0)$AFINN_score), 4)
AFINN_sd_post <- round(sd(subset(goodreads_r_sentiment, treated == 1)$AFINN_score), 4)

AFINN_var_pre <- round(var(subset(goodreads_r_sentiment, treated == 0)$AFINN_score), 4)
AFINN_var_post <- round(var(subset(goodreads_r_sentiment, treated == 1)$AFINN_score), 4)

AFINN_mean_pre <- round(mean(subset(goodreads_r_sentiment, treated == 0)$AFINN_score), 4)
AFINN_mean_post <- round(mean(subset(goodreads_r_sentiment, treated == 1)$AFINN_score), 4)


# plot- AFINN
# word_counts <- reviews_tokens %>%
#   count(word, sort = TRUE) %>%
#   filter(n > 10)
# 
# ggplot(word_counts, aes(x = n, y = reorder(word, n))) +
#   geom_point() +
#   scale_x_log10() +
#   xlab("Word Count (log scale)") +
#   ylab("Words")




