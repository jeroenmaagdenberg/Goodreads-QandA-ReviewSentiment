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

# regressions
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


# Book_Id and Year_Month as fixed effects and clustered on Book_Id
# fixed effects - main effect
model4 <- feols(AFINN_score ~ post
                 | Book_Id + date,
                cluster = "Book_Id",
                data = goodreads_r_sentiment)
summary(model4)

# fixed effects - moderation
model5 <- feols(AFINN_score ~ post + Likes + Number_of_Answers + 
                   post:Likes + post:Number_of_Answers
                 | Book_Id + date,
                cluster = "Book_Id",
                data = goodreads_r_sentiment)
summary(model5)


# fixed effects - moderation
model5b <- feols(AFINN_score ~ post + Likes + 
                  post:Likes + post:Number_of_Answers
                | Book_Id + date,
                cluster = "Book_Id",
                data = goodreads_r_sentiment)
summary(model5b)

# fixed effects - three-way moderation
model6 <- feols(AFINN_score ~ post + Likes + Number_of_Answers + 
                  post:Likes + post:Number_of_Answers + 
                  post:Likes:Number_of_Answers
                 | Book_Id + date,
                cluster = "Book_Id",
                data = goodreads_r_sentiment)
summary(model6)

model6b <- feols(AFINN_score ~ post + Likes + 
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
qqnorm(resid(model6)) # violated
jarque.bera.test(resid(model6)) # violated

# independence
bgtest(AFINN_score ~ post + Likes + Number_of_Answers, order = 3, data = goodreads_r_sentiment) # violated




#### Diff in Diff ####
# create merged dataframe for diff in diff
df_merged_reviews <- rbind(goodreads_r_sentiment, amazon_r_sentiment, fill = TRUE)

merged_reviews <- df_merged_reviews  # df_merged_reviews -> merged_reviews when it works as it should.  

# variable explanation
# post = whether review was posted before or after question was posted
# source = Amazon or Goodreads (either AM or GR)
# post_treatment = when review is posted on goodreads AND after question, than 1. Else 0. 


# combining video and both date & book_id
sink("gen/analysis/output/model_mod.txt")
did_model <- feols(AFINN_score ~ post + post:post_treatment + post:post_treatment:Likes + post:post_treatment:Number_of_Answers + post:post_treatment:Likes:Number_of_Answers
                    | Book_Id + Year_Month, data = merged_reviews)
summary(did_model)
sink()
