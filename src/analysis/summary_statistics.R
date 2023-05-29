library(knitr)
library(scales)

#### Goodreads ####
##### Summary Statistics - Goodreads #####
goodreads_books <- fread("dat/2864_goodreads_com_book_full.csv")
web_archive <- fread("dat/2864_web_archive_org_one_year_level.csv")

# compute unique book ids
unique_book_ids <- goodreads_books[!duplicated(goodreads_books$Book_Id),] %>%
  select(Book_Id) # 100034 books
unique_book_ids2 <- web_archive[!duplicated(web_archive$Book_Id),] %>%
  select(Book_Id) # 12139 books
gr_unique_book_ids <- goodreads_full[!duplicated(goodreads_full$Book_Id),] %>%
  select(Book_Id) # 14123 books with questions and reviews

tidy_afinn %>% count(word, sort = T)

# trying to find out how it is possible that there is some variation with Number of Answers
book_counts <- goodreads_r_sentiment %>%
  group_by(Book_Id) %>%
  summarize(num_answers = n_distinct(Number_of_Answers))

# Filter the books that have more than one unique value for Number_of_Answers
books_with_multiple_answers <- book_counts %>%
  filter(num_answers > 1)

# View the books with multiple Number_of_Answers
table(books_with_multiple_answers)   # gives me an empty table, so i conclude that there is no variation per book






# summary statistics - AFINN
goodreads_r_sentiment <- fread("gen/dataprep/goodreads_r_sentiment.csv")
goodreads_b_sentiment <- fread("gen/dataprep/goodreads_b_sentiment.csv")

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
gr_num_answ_mean <- round(mean(goodreads_b_sentiment$Number_of_Answers, na.rm = TRUE), 4)
gr_num_answ_sd <- round(sd(goodreads_b_sentiment$Number_of_Answers, na.rm = TRUE), 4)
gr_num_answ_max <- round(max(goodreads_b_sentiment$Number_of_Answers, na.rm = TRUE), 4)
gr_num_answ_min <- round(min(goodreads_b_sentiment$Number_of_Answers, na.rm = TRUE), 4)



# example reviews
example_reviews <- goodreads_full %>%
  select(review_text, review_id)
example_reviews <- merge(example_reviews, goodreads_r_sentiment, by = "review_id")
example_reviews <- example_reviews %>%
  select(Title, review_text, AFINN_score) %>%
  filter(nchar(review_text) < 200) 

# Select random positive, negative and neutral reviews
positive_sample <- example_reviews %>%
  filter(AFINN_score > 0) %>%
  sample_n(3) 
negative_sample <- example_reviews  %>%
  filter(AFINN_score < 0) %>%
  sample_n(3)
neutral_sample <- example_reviews  %>%
  filter(AFINN_score == 0) %>%
  sample_n(3)
sample_table <- bind_rows(positive_sample, negative_sample, neutral_sample)
rm(negative_sample, positive_sample, neutral_sample)


##### Graphs - Goodreads #####
# line graph for cumulitive number of reviews over time
gr_summary_data <- goodreads_r_sentiment %>%
  group_by(Year_Month) %>%
  summarize(Review_Count = sum(n()))
gr_summary_data$Cumulative_Count <- cumsum(gr_summary_data$Review_Count)
ggplot(gr_summary_data, aes(x = Year_Month, y = Cumulative_Count)) +
  geom_line() +
  labs(x = "Year", y = "Cumulative Number of Reviews") +
  ggtitle("Figure 2: Cumulative Number of Reviews Over Time on Goodreads") +
  scale_y_continuous(labels = comma_format()) +
  theme(plot.title = element_text(hjust = 0.5))


# scatter plot of AFINN_score
ggplot(goodreads_r_sentiment, aes(x = seq_along(AFINN_score), y = AFINN_score)) +
  geom_point(color = "steelblue") +
  labs(x = "Index", y = "AFINN Score") +
  ggtitle("Figure 3: Scatter Plot of AFINN Scores on Goodreads") +
  theme(plot.title = element_text(hjust = 0.5))


# line graph for average AFINN score over time
goodreads_avg_score <- goodreads_r_sentiment %>%
  group_by(Year_Month) %>%
  summarise(avg_score = mean(AFINN_score, na.rm = TRUE))
ggplot(goodreads_avg_score, aes(x = Year_Month, y = avg_score)) +
  geom_line() +
  labs(x = "Year", y = "Average AFINN Score") +
  ggtitle("Figure 4: Average AFINN Score on Goodreads") +
  theme(plot.title = element_text(hjust = 0.5))



#### GR corrected ####
goodreads_r_sentiment <- goodreads_r_sentiment %>%
  filter(AFINN_score <= 1000)

##### Summary Statistics - Corrected GR #####
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
gr_num_answ_mean <- round(mean(goodreads_b_sentiment$Number_of_Answers, na.rm = TRUE), 4)
gr_num_answ_sd <- round(sd(goodreads_b_sentiment$Number_of_Answers, na.rm = TRUE), 4)
gr_num_answ_max <- round(max(goodreads_b_sentiment$Number_of_Answers, na.rm = TRUE), 4)
gr_num_answ_min <- round(min(goodreads_b_sentiment$Number_of_Answers, na.rm = TRUE), 4)


# overview before vs after (before / after top question)
table(goodreads_r_sentiment$post )

# before / post AFINN statistics (before / after top question)
AFINN_mean_pre <- round(mean(subset(goodreads_r_sentiment, post == 0)$AFINN_score), 4)
AFINN_mean_post <- round(mean(subset(goodreads_r_sentiment, post == 1)$AFINN_score), 4)

AFINN_sd_pre <- round(sd(subset(goodreads_r_sentiment, post == 0)$AFINN_score), 4)
AFINN_sd_post <- round(sd(subset(goodreads_r_sentiment, post == 1)$AFINN_score), 4)

AFINN_var_pre <- round(var(subset(goodreads_r_sentiment, post == 0)$AFINN_score), 4)
AFINN_var_post <- round(var(subset(goodreads_r_sentiment, post == 1)$AFINN_score), 4)











#### Amazon ####
amazon_r_sentiment <- fread("gen/dataprep/amazon_r_sentiment.csv")
amazon_b_sentiment <- fread("gen/dataprep/amazon_b_sentiment.csv")



##### Summary Statistics - Amazon #####
# summary statistics - AFINN
am_AFINN_mean <- round(mean(amazon_r_sentiment$AFINN_score, na.rm = TRUE), 4)
am_AFINN_sd <- round(sd(amazon_r_sentiment$AFINN_score, na.rm = TRUE), 4)
am_AFINN_max <- round(max(amazon_r_sentiment$AFINN_score, na.rm = TRUE), 4)
am_AFINN_min <- round(min(amazon_r_sentiment$AFINN_score, na.rm = TRUE), 4)

# summary statistics - Likes
am_likes_mean <- round(mean(amazon_b_sentiment$Likes, na.rm = TRUE), 4)
am_likes_sd <- round(sd(amazon_b_sentiment$Likes, na.rm = TRUE), 4)
am_likes_max <- round(max(amazon_b_sentiment$Likes, na.rm = TRUE), 4)
am_likes_min <- round(min(amazon_b_sentiment$Likes, na.rm = TRUE), 4)

# summary statistics - Number of Answers
am_num_answ_mean <- round(mean(amazon_b_sentiment$Number_of_Answers, na.rm = TRUE), 4)
am_num_answ_sd <- round(sd(amazon_b_sentiment$Number_of_Answers, na.rm = TRUE), 4)
am_num_answ_max <- round(max(amazon_b_sentiment$Number_of_Answers, na.rm = TRUE), 4)
am_num_answ_min <- round(min(amazon_b_sentiment$Number_of_Answers, na.rm = TRUE), 4)


# overview before vs after (before / after top question)
table(amazon_r_sentiment$post)
round((425698 / (425698 + 830949) * 100), 2)

# before / post AFINN statistics (before / after top question)
AFINN_mean_pre <- round(mean(subset(amazon_r_sentiment, post == 0)$AFINN_score), 4)
AFINN_mean_post <- round(mean(subset(amazon_r_sentiment, post == 1)$AFINN_score), 4)

AFINN_sd_pre <- round(sd(subset(amazon_r_sentiment, post == 0)$AFINN_score), 4)
AFINN_sd_post <- round(sd(subset(amazon_r_sentiment, post == 1)$AFINN_score), 4)


# get_sentiments("afinn") %>%
#  filter(word == "terribly")


##### Graphs - Amazon #####
# line graph for cumulative number of reviews over time including Goodreads
am_summary_data <- amazon_r_sentiment %>%
  group_by(Year_Month) %>%
  summarize(Review_Count = sum(n()))
am_summary_data$Cumulative_Count <- cumsum(am_summary_data$Review_Count)
ggplot() +
  geom_line(data = gr_summary_data, aes(x = Year_Month, y = Cumulative_Count, color = "Goodreads")) +
  geom_line(data = am_summary_data, aes(x = Year_Month, y = Cumulative_Count, color = "Amazon")) +
  labs(x = "Year", y = "Cumulative Number of Reviews", color = "Source") +
  ggtitle("Figure 5: Cumulative Number of Reviews Over Time on Goodreads vs Amazon") +
  scale_y_continuous(labels = comma_format()) +
  theme(plot.title = element_text(hjust = 0.5))


# scatter plot of AFINN_score
ggplot(amazon_r_sentiment, aes(x = seq_along(AFINN_score), y = AFINN_score)) +
  geom_point(color = "red1") +
  labs(x = "Index", y = "AFINN Score") +
  ggtitle("Figure 6: Scatter Plot of AFINN Scores on Amazon") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Create a line plot to compare the average AFINN score
amazon_avg_score <- amazon_r_sentiment %>%
  group_by(Year_Month) %>%
  summarise(avg_score = mean(AFINN_score, na.rm = TRUE))
ggplot() +
  geom_line(data = goodreads_avg_score, aes(x = Year_Month, y = avg_score, color = "Goodreads")) +
  geom_line(data = amazon_avg_score, aes(x = Year_Month, y = avg_score, color = "Amazon")) +
  labs(x = "Year", y = "Average AFINN Score", color = "Source") +
  ggtitle("Figure 7: Comparison of Average AFINN Score on Goodreads vs Amazon") +
  theme(plot.title = element_text(hjust = 0.5))


