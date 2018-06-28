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
stevens_tree <- rpart(Reverse ~ Circuit + Issue + Petitioner + 
                        Respondent + LowerCourt + Unconst,
                      data=stevens_train, method="class",
                      control=rpart.control(minbucket=25))
prp(stevens_tree)
# tree splits. Predict 0 means affirm, 1 means reverse
# use table(Responde) to see list of values, unabbreviated

# compare CART model predictions to actual
stevens_cart <- predict(stevens_tree, newdata=stevens_test, type="class")
table(stevens_test$Reverse, stevens_cart) # confusion matrix
(41+71)/(41+36+22+71) # accuracy
# not shown: logistic regression would be 66.5% accuracy,
# baseline (always predict reverse) would be 54.7%
# so... competitive with logistic, easier to interpret

# Examine ROCR curve
library(ROCR)
stevens_rocr <- predict(stevens_tree, newdata=stevens_test)
stevens_rocr
# two columns for each test set observation
# col0 is % of training set data in same subset as that same test set obser w/ outcome 0
# col1 is % of training set data in the same subset as that test set obs w/ outcome 1
# consider col1 as probability that that test set obs has outcome 1,
# use for threshholding

pred <- prediction(stevens_rocr[,2], stevens_test$Reverse)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
