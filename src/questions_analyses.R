library(dplyr)
library(fixest)
library(data.table)
# library(ggplot2)
# library(stargazer)

sentimentlm <- lm(sentiment_afinn_mean ~ post_treatment + *post_treatment + Number_of_Answers, 
                  data = gr_questions_reviews_sent2
                  )

summary(sentimentlm)


model_simple <- feols(sentiment_afinn2 ~ post_treatment + Likes:post_treatment
                      | review_id, 
                      data = gr_questions_reviews_sent2)
summary(model_simple)
