library(dplyr)
library(fixest)
library(data.table)
library(nortest)
library(AER)
library(stargazer)
library(tseries)

goodreads_r_sentiment <- fread("gen/dataprep/goodreads_r_sentiment.csv")
amazon_r_sentiment <- fread("gen/dataprep/amazon_r_sentiment.csv")

#### goodreads ####
# remove outlier
goodreads_r_sentiment <- goodreads_r_sentiment %>%
  filter(AFINN_score <= 1000)

# simple t-test effect Q&A on sentiment
sent_before <- subset(goodreads_r_sentiment, post == 0)$AFINN_score
sent_after <- subset(goodreads_r_sentiment, post == 1)$AFINN_score

t.test(sent_before, sent_after)

# checks on this t-test
set.seed(123) # to make sure everyone has the same outcome in the following tests

sent_before %>%
  sample(5000) %>% #largest sample possible
  shapiro.test() # rejects normality
sent_after %>%
  sample(5000) %>% 
  shapiro.test() # rejects normality

ad.test(sent_before) # rejects normality
ad.test(sent_after) # rejects normality

# Mann-Whitney U-test since normality was rejected
wilcox.test(sent_before, sent_after, conf.int = TRUE, conf.level = 0.95)





##### regressions #####
# simple regression
model1 <- feols(AFINN_score ~ post,
                data = goodreads_r_sentiment)
summary(model1)

# model + moderators and interactions
model2 <- feols(AFINN_score ~ post + Likes + Number_of_Answers +
                  post:Likes + post:Number_of_Answers,
                data = goodreads_r_sentiment)
summary(model2)

# model + three way interaction
model3 <- feols(AFINN_score ~ post + Likes + Number_of_Answers +
                  post:Likes + post:Number_of_Answers + post:Likes:Number_of_Answers,
                data = goodreads_r_sentiment)
summary(model3)




##### fixed effects #####
# Book_Id and date as fixed effects and clustered on Book_Id
# fixed effects - main effect
model4 <- feols(AFINN_score ~ post
                 | Book_Id + date,
                cluster = "Book_Id",
                data = goodreads_r_sentiment)
summary(model4)


# fixed effects - moderation
model5 <- feols(AFINN_score ~ post + Likes + 
                  post:Likes + post:Number_of_Answers
                | Book_Id + date,
                cluster = "Book_Id",
                data = goodreads_r_sentiment)
summary(model5)

model5b <- feols(AFINN_score ~ post + Likes + Number_of_Answers + 
                   post:Likes + post:Number_of_Answers
                 | Book_Id + date,
                cluster = "Book_Id",
                data = goodreads_r_sentiment)
summary(model5b)


# fixed effects - three-way moderation
model6 <- feols(AFINN_score ~ post + Likes + 
                  post:Likes + post:Number_of_Answers + 
                  post:Likes:Number_of_Answers
                | Book_Id + date,
                cluster = "Book_Id",
                data = goodreads_r_sentiment)
summary(model6)

model6b <- feols(AFINN_score ~ post + Likes + Number_of_Answers + 
                   post:Likes + post:Number_of_Answers + 
                   post:Likes:Number_of_Answers
                 | Book_Id + date,
                 cluster = "Book_Id",
                 data = goodreads_r_sentiment)
summary(model6b)



# assumption checks on model6
# Homoscedasticity
bptest(AFINN_score ~ post + Likes + Number_of_Answers, data = goodreads_r_sentiment) #violated

# multicollinearity
model_corr <- cor(goodreads_r_sentiment %>%
                    select(post,Likes,Number_of_Answers), use = "pairwise.complete.obs")
model_corr # met

# normality
jarque.bera.test(resid(model6)) # violated
qqnorm(resid(model6)) # violated

# independence
bgtest(AFINN_score ~ post + Likes + Number_of_Answers, order = 3, data = goodreads_r_sentiment) # violated


# trying to find out how it is possible that there is some variation with Number of Answers
#
book_counts <- goodreads_r_sentiment %>%
  group_by(Book_Id) %>%
  summarize(num_answers = n_distinct(Number_of_Answers))

# Filter the books that have more than one unique value for Number_of_Answers
books_with_multiple_answers <- book_counts %>%
  filter(num_answers > 1)

# View the books with multiple Number_of_Answers
table(books_with_multiple_answers)   # gives me an empty table, so i conclude that there is no variation per book









# Year_month as fixed instead of date
model4 <- feols(AFINN_score ~ post
                | Book_Id + Year_Month,
                cluster = "Book_Id",
                data = goodreads_r_sentiment)
summary(model4)

model5 <- feols(AFINN_score ~ post + Likes + 
                  post:Likes + post:Number_of_Answers
                | Book_Id + Year_Month,
                cluster = "Book_Id",
                data = goodreads_r_sentiment)
summary(model5)

model6 <- feols(AFINN_score ~ post + Likes + 
                  post:Likes + post:Number_of_Answers + 
                  post:Likes:Number_of_Answers
                | Book_Id + Year_Month,
                cluster = "Book_Id",
                data = goodreads_r_sentiment)
summary(model6)











#### Diff in Diff ####
amazon_booklist <- amazon_r_sentiment %>%
  select(Book_Id) %>%
  distinct() 

amazon_r_sentiment <- amazon_r_sentiment %>%
  mutate(exact_question_timestamp = as.Date(exact_question_timestamp)) %>%
  mutate(date = as.Date(date)) %>%
  mutate(Year_Month = as.Date(Year_Month))



# subset goodreads_r_sentiment to match the books in amazon_r_sentiment
goodreads_r_sent_subset <- filter(goodreads_r_sentiment, Book_Id %in% amazon_booklist$Book_Id) 
# I assume this is necessary as this will mean that only the books that are present in BOTH dataframes are tested 

# create merged dataframe for diff in diff

df_merged_reviews <- rbind(goodreads_r_sent_subset, amazon_r_sentiment, fill = TRUE)

# variable explanation
# post = whether review was posted before or after question was posted
# source = Amazon or Goodreads (either AM or GR)
# post_treatment = when review is posted on goodreads AND after question, than 1. Else 0. 


# change source variable to a dummy where treated = 1 for Goodreads, and 0 for Amazon. 
merged_reviews <- df_merged_reviews %>%
  mutate(source = if_else(source == "GR", 1,0)) %>%
  rename("treated" = "source")


# DiD analysis
sink("gen/analysis/output/model_did.txt")
did_model <- feols(AFINN_score ~ post + treated + post:treated + post:treated:Likes + post:treated:Number_of_Answers + post:treated:Likes:Number_of_Answers
                   | Book_Id + date, data = merged_reviews)
summary(did_model)
sink()



# without three-way interaction
sink("gen/analysis/output/model_did2.txt")
did_model2 <- feols(AFINN_score ~ post + treated + post:treated + post:treated:Likes + post:treated:Number_of_Answers
                   | Book_Id + date, data = merged_reviews)
summary(did_model2)
sink() # makes another variable significant