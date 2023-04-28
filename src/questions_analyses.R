library(tidyverse)
library(ggplot2)
library(stargazer)
library(dplyr)

sentimentlm <- lm(sentiment_afinn_mean ~ QA_present + Likes 
                  + Likes*QA_present + Number_of_Answers 
                  + Number_of_Answers*QA_present, 
                  data = gr_questions_reviews
                  )

summary(sentimentlm)


