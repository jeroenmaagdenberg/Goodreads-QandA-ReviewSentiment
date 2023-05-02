library(dplyr)
library(fixest)
library(data.table)
# library(ggplot2)
# library(stargazer)

sentimentlm <- lm(AFINN_mean ~ Likes + Number_of_Answers, 
                  data = goodreads_sentiment
                  )

summary(sentimentlm)
install.packages("plm")
library(plm)

model1 <- plm(AFINN_mean ~ post_treatment, data =goodreads_r_sentiment)
summary(model1)

model2 <- plm(AFINN_mean ~ post_treatment + Likes + post_treatment:Likes, data =goodreads_r_sentiment)
summary(model2)




######## 
model_simple <- feols(AFINN_mean ~ Likes + Number
                      | review_id, 
                      data = goodreads_r_sentiment)
summary(model_simple)


