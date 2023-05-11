library(dplyr)
library(fixest)
library(data.table)
library(nortest)

# simple t-test effect Q&A on sentiment
sent_before <- subset(goodreads_r_sentiment, treated == 0)$AFINN_score
sent_after <- subset(goodreads_r_sentiment, treated == 1)$AFINN_score

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
model1 <- feols(AFINN_score ~ treated,
                data = goodreads_r_sentiment)
summary(model1)

# model + moderators and interactions
model2 <- feols(AFINN_score ~ treated + Likes + Number_of_Answers +
                  treated:Likes + treated:Number_of_Answers,
                data = goodreads_r_sentiment)
summary(model2)

# model + three way interaction
model3 <- feols(AFINN_score ~ treated + Likes + Number_of_Answers +
                  treated:Likes + treated:Number_of_Answers + treated:Likes:Number_of_Answers,
                data = goodreads_r_sentiment)
summary(model3)


# book and date as fixed effects
# fixed effects - main effect
model4 <- feols(AFINN_score ~ treated
                 | Book_Id + date,
                 data = goodreads_r_sentiment)
summary(model4)

# fixed effects - moderation
model5 <- feols(AFINN_score ~ treated + Likes + Number_of_Answers + 
                   treated:Likes + treated:Number_of_Answers
                 | Book_Id + date,
                 data = goodreads_r_sentiment)
summary(model5)


# fixed effects - moderation
model5b <- feols(AFINN_score ~ treated + Likes + 
                  treated:Likes + treated:Number_of_Answers
                | Book_Id + date,
                data = goodreads_r_sentiment)
summary(model5b)

# fixed effects - three-way moderation
model6 <- feols(AFINN_score ~ treated + Likes + Number_of_Answers + 
                  treated:Likes + treated:Number_of_Answers + 
                  treated:Likes:Number_of_Answers
                 | Book_Id + date,
                 data = goodreads_r_sentiment)
summary(model6)

