library(tidyverse)
library(psych)

setwd("~/Documents/UT_Austin/Courses/Intro to Biostats & Computational Analysis/Project/")
getwd()

#read data
smokingstatus <- read_csv("smoking.csv")
#remove index
smokingstatus2 <- smokingstatus[,-1]
smokingstatus2 <- smokingstatus2 %>% select(-oral)
#one-hot encoding (manual)
smokingstatus2$gender <- ifelse(smokingstatus2$gender=="M",1.0,0.0)
smokingstatus2$oral <- ifelse(smokingstatus2$oral=="Y",1.0,0.0)
smokingstatus2$tartar <- ifelse(smokingstatus2$tartar=="Y",1.0,0.0)

set <- smokingstatus2
hset <- head(set, 100)

hset %>% 
  select(gender, Gtp, triglyceride, hemoglobin, `height(cm)`, smoking) %>%
  pairs.panels()

cor(set)[25,]
pairs.panels(hset)

data_cor <- cor(set[ , colnames(set) != "smoking"],  # Calculate correlations
                set$smoking)
data_cor                                          # Print correlation values
