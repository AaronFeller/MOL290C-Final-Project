
#library(yaml)
library(mltools)
library(tidyverse)
library(randomForest)
library(tidypredict)
library(pROC)

setwd("~/Documents/UT_Austin/Courses/Intro to Biostats & Computational Analysis/Project/")
getwd()

#read data
smokingstatus <- read_csv("smoking.csv")
#remove index
smokingstatus2 <- smokingstatus[,-1]
#one-hot encoding (manual)
smokingstatus2$gender <- ifelse(smokingstatus2$gender=="M",1,0)
smokingstatus2$oral <- ifelse(smokingstatus2$oral=="Y",1,0)
smokingstatus2$tartar <- ifelse(smokingstatus2$tartar=="Y",1,0)

set <- smokingstatus2 %>% select(-oral)

# split training and test set
tr.idx <- c(sample(1:nrow(set), round(nrow(set) * 0.9)))
te.idx <- setdiff(1:nrow(set), tr.idx)



y <- as.data.frame(smokingstatus2$smoking)
x <- smokingstatus2[,1:25] %>% select(-oral)

x.tr <- x[tr.idx, ]
x.te <- x[te.idx, ]
y.tr <- y[tr.idx, ]
y.te <- y[te.idx, ]

#fit random forest model
rf.fit <- randomForest(x.tr, y.tr, cv.fold = 10)
rf.fit

#predict using x.te(st)
rf.pred <- predict(rf.fit, newdata = x.te, type = "response")#[, 1]

#plot ROC
plot.roc(y.te, rf.pred, grid = TRUE, print.auc = TRUE)

#fine tune the mtry
mtry <- tuneRF(set[],set$smoking, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)



#fit model again using best.m for mtry
rf.fit <- randomForest(x.tr, y.tr, cv.fold=10, mtry=best.m, importance=TRUE, ntree=500)
rf.fit

#predict using x.te(st)
rf.pred <- predict(rf.fit, newdata = x.te, type = "response")#[, 1]

#plot ROC with optimized mtry
plot.roc(y.te, rf.pred, grid = TRUE, print.auc = TRUE)

#show importance of each variable
importance(rf.fit)
varImpPlot(rf.fit)


########NEW REWORK
#fit model again using best.m for mtry
rf.fit <- randomForest(x.tr, y.tr, cv.fold=5, mtry=12, importance=TRUE, ntree=500)
rf.fit

#predict using x.te(st)
rf.pred <- predict(rf.fit, newdata = x.te, type = "response")#[, 1]

#plot ROC with optimized mtry
plot.roc(y.te, rf.pred, grid = TRUE, print.auc = TRUE)

#show importance of each variable
importance(rf.fit)
varImpPlot(rf.fit)


## MINIMIZED SET
set <- smokingstatus2 %>% select(-`hearing(left)`, -`hearing(right)`, -`Urine protein`, -`height(cm)`, -`dental caries`, 
                                 -`weight(kg)`, -tartar, -`serum creatinine`, -hemoglobin, -age, -`eyesight(left)`, -`eyesight(right)`)

# split training and test set
tr.idx <- c(sample(1:nrow(set), round(nrow(set) * 0.9)))
te.idx <- setdiff(1:nrow(set), tr.idx)



y <- as.data.frame(set$smoking)
x <- set[,1:25] %>% select(-oral)

x.tr <- x[tr.idx, ]
x.te <- x[te.idx, ]
y.tr <- y[tr.idx, ]
y.te <- y[te.idx, ]

#fit again
rf.fit <- randomForest(x.tr, y.tr, cv.fold=5, mtry=8, importance=TRUE, ntree=500)
rf.fit

#predict using x.te(st)
rf.pred <- predict(rf.fit, newdata = x.te, type = "response")#[, 1]

#plot ROC with optimized mtry
plot.roc(y.te, rf.pred, grid = TRUE, print.auc = TRUE)

#show importance of each variable
importance(rf.fit)
varImpPlot(rf.fit)








#set for PCA does not have oral or smoking (oral is useless)
set_for_pca <- set %>%
  select(-smoking)

#run prcomp on the set and output the pca (set_pca)
set_pca <- prcomp(set_for_pca, center = TRUE, scale. = TRUE)

#summarize pca
summary(set_pca)

#print pca
str(set_pca)

# load ggbiplot
library(ggbiplot)

# plot the utility of each variable with PCA
ggbiplot(set_pca, alpha = 0) + xlim(-1,1.5) + ylim(-1,1)

#parse the model for writing
#parsed <- parse_model(rf.fit)

#Write out yaml
#write_yaml(parsed, "smoking_model.yml")


#Applying saved model
#loaded_model <- read_yaml("smoking_model.yml")
#loaded_model <- as_parsed_model(loaded_model)

#tidypredict_fit_loaded_model <- tidypredict_fit(loaded_model)

#output <- tidypredict_fit(x1.df, loaded_model)

#x1 to dataframe
#x1.df <- as.data.frame(x1)

# Example : Output file <- predict(model, new data)
#test_output$prediction <- predict(rf.fit, x1.df)


