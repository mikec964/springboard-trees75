# load data
stevens <- read.csv("stevens.csv")
str(stevens)


# create train and test sets
library(caTools)
set.seed(3000)
split <- sample.split(stevens$Reverse, SplitRatio=0.7)  # create vector
stevens_train <- subset(stevens, split == TRUE)         # apply vector
stevens_test <- subset(stevens, split == FALSE)


# build CART model
library(rpart)  
library(rpart.plot)
stevens_cart <- rpart(Reverse ~ Circuit + Issue + Petitioner + 
                        Respondent + LowerCourt + Unconst,
                      data=stevens_train, method="class",
                      control=rpart.control(minbucket=25))
prp(stevens_cart)
# tree splits. Predict 0 means affirm, 1 means reverse
# use table(Responde) to see list of values, unabbreviated


# compare CART model predictions to actual
# type="class" limits to two options 0,1
pred_cart_stevens <- predict(stevens_cart, newdata=stevens_test, type="class")
table(stevens_test$Reverse, pred_cart_stevens) # confusion matrix
(41+71)/(41+36+22+71) # 65.8% accuracy
# not shown: logistic regression would be 66.5% accuracy,
# baseline (always predict reverse) would be 54.7%
# so... competitive with logistic, easier to interpret


# Examine ROCR curve
library(ROCR)
stevens_rocr <- predict(stevens_cart, newdata=stevens_test)
stevens_rocr
# two columns for each test set observation
# col0 is % of training set data in same subset as that same test set obser w/ outcome 0
# col1 is % of training set data in the same subset as that test set obs w/ outcome 1
# consider col1 as probability that that test set obs has outcome 1,
# use for threshholding

pred <- prediction(stevens_rocr[,2], stevens_test$Reverse)
perf <- performance(pred, "tpr", "fpr")
plot(perf)


# Random Forest
library(randomForest)
# To use for classification (instead of linear regression),
# dependent variable must be a factor
stevens_train$Reverse <- as.factor(stevens_train$Reverse)
stevens_test$Reverse <- as.factor(stevens_test$Reverse)
stevens_forest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + 
                                 Respondent + LowerCourt + Unconst,
                               data=stevens_train, nodesize=25, ntree=200)
pred_forest_stevens <- predict(stevens_forest, newdata=stevens_test)
table(stevens_test$Reverse, pred_forest_stevens)
(43+76)/(40+37+19+74)
# 0.7, slight improvement


# Use cross-validation of CART model
library(caret)
library(e1071)
# Define number of folds
fit_control <- trainControl(method="cv", number=10)
cart_grid <- expand.grid(.cp=(1:50)*0.01)
train(Reverse ~ Circuit + Issue + Petitioner + 
        Respondent + LowerCourt + Unconst,
      data=stevens_train, method="rpart", trControl=fit_control, 
      tuneGrid=cart_grid)
# Output is suggested cp, accuracy, and accuracy SD... suggests cp of 0.18

# Create new CART value using cp instead of minbucket
stevens_cart2 <- rpart(Reverse ~ Circuit + Issue + Petitioner + 
                        Respondent + LowerCourt + Unconst,
                      data=stevens_train, method="class",
                      control=rpart.control(cp=0.18))
pred_cart2_stevens <- predict(stevens_cart2, newdata=stevens_test, type="class")
confusionMatrix(table(pred_cart2_stevens, stevens_test$Reverse))
# 72.4% accuracy, improved over 65.8% CART and 70% random forest

