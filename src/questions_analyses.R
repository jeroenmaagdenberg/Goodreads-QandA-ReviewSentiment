library(dplyr)
library(fixest)
library(data.table)
# library(ggplot2)
# library(stargazer)

sentimentlm <- lm(AFINN_mean ~ AFINN_pretreat + AFINN_posttreat, 
                  data = goodreads_sentiment
                  )

summary(sentimentlm)


model_simple <- feols(AFINN_mean ~ AFINN_pretreat + Likes:AFINN_pretreat
                      | Book_Id, 
                      data = goodreads_sentiment)
summary(model_simple)
