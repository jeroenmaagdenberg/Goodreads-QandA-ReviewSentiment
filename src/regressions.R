library(dplyr)
library(fixest)
library(data.table)
# library(ggplot2)
# library(stargazer)

sentimentlm <- lm(AFINN_mean ~ AFINN_pretreat:AFINN_posttreat + Likes, 
                  data = goodreads_sentiment
                  )

summary(sentimentlm)


model_simple <- feols(AFINN_mean ~ Likes + Number
                      | review_id, 
                      data = goodreads_r_sentiment)
summary(model_simple)
