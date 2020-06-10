#Importing the Dataset
dataset = read.csv('bank-loan.csv')
dataset
#finding if there are any missing values
sum(is.na(dataset$default))
sum(is.na(dataset$age))
# filling the missing values with median
dataset$default[is.na(dataset$default)] <- median(dataset$default, na.rm = TRUE)
# checking if there are any missing values after filling them with median
sum(is.na(dataset$default))
dataset
loan = subset(dataset,select = -c(address) )
loan = loan[0:8]
# Logistic Regression 
logit<-glm(default~.,family=binomial,data =loan)
summary(logit)
# Odds Ratio
exp(coef(logit))
# Confusion matrix table 
prob <- predict(logit,type=c("response"),loan)
prob
confusion<-table(prob>0.5,loan$default)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy

#splitting the dataset to implement the Decision Tree 
require(caTools)
set.seed(123)
split = sample.split(loan$default,SplitRatio = 0.70)
training_set = subset(loan,split == TRUE)
test_set = subset(loan, split == FALSE)
#feature scaling
training_set[,1:7]=scale(training_set[,1:7])
test_set[,1:7]=scale(test_set[,1:7])
#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')
#develop decision tree model on training data
training_set[,8]<-as.factor(training_set[,8])
C50_model =C50::C5.0(training_set[, -8],training_set[, 8])
summary(C50_model)
#Lets predict for test cases
C50_Predictions = predict(C50_model, test_set[,-8], type = "class")
confusion<-table(C50_Predictions,test_set[,8])
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
library(ROCR)
pred = prediction(as.numeric(C50_Predictions),as.numeric(test_set[,8]))
perf1 <- performance(pred,"tpr","fpr")
plot(perf1, col="orange", lwd=2) 
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=2)

auc = performance(pred, 'auc')
slot(auc, 'y.values')
# Developing the Random forest model on training data
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-8],
                          y = training_set$default,
                          ntree = 500)
# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-8])
confusion<-table(test_set[,8],y_pred)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
library(ROCR)
pred = prediction(as.numeric(y_pred),as.numeric(test_set[,8]))
perf1 <- performance(pred,"tpr","fpr")
plot(perf1, col="orange", lwd=2) 
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=2)

auc = performance(pred, 'auc')
slot(auc, 'y.values')
# Implementing the XGBoosting Algorithm
library(xgboost)
classifier = xgboost(data = as.matrix(training_set[-8]), label = training_set$default, nrounds = 10)
# Predicting the Test set results
y_pred = predict(classifier, newdata = as.matrix(test_set[-8]))
y_pred = (y_pred >= 0.5)
#confusion matrix
confusion<-table(test_set[,8],y_pred)
confusion
#Accuracy
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
library(ROCR)
pred = prediction(as.numeric(y_pred),as.numeric(test_set[,8]))
perf1 <- performance(pred,"tpr","fpr")
plot(perf1, col="orange", lwd=2) 
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=2)

auc = performance(pred, 'auc')
slot(auc, 'y.values')


        