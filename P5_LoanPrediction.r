rm(list=ls())
setwd('C:/Users/cchousal/Documents/Data Science/FINAL PROJECT WORK/PROJECT-5-8211-LOAN-PREDICTION-PROJECT-03AUG2020032128')
loan <- read.csv('P5_cutomer_loan.csv')

##------------------------------------------------TASK A-----------------------------------------------------------------
##----a----------
str(loan)
sum(is.na(loan))
##----b----------
library(dplyr)
dti <- loan$debts/loan$income
loan_new <- cbind(loan, dti)
##----c----------
loan_new <- loan_new %>% mutate(loan_decision_status = ifelse(loan_decision_type == 'Denied',0,1))
class(loan_new$loan_decision_status)
loan_new$loan_decision_status <- factor(loan_new$loan_decision_status, levels=c(0,1))
class(loan_new$loan_decision_status)
##----d----------
customer_loan_refined <- loan_new[,c(3,4,6,7,8,11,13,14)]
##----e----------
str(customer_loan_refined)

customer_loan_refined$gender <- as.numeric(factor(customer_loan_refined$gender))
customer_loan_refined$marital_status <- as.numeric(factor(customer_loan_refined$marital_status))
customer_loan_refined$occupation <- as.numeric(factor(customer_loan_refined$occupation))
customer_loan_refined$loan_type <- as.numeric(factor(customer_loan_refined$loan_type))
str(customer_loan_refined)

##------------------------------------------------TASK B-----------------------------------------------------------------
##----a----------
library(caTools)
split <- sample.split(customer_loan_refined$loan_decision_status, SplitRatio = 0.7)
train <- subset(customer_loan_refined, split == T)
test <- subset(customer_loan_refined, split == F)
##----b----------
# train_scaled <- scale(train[,1:7])
# test_scaled <- scale(test[,1:7])
# train_new <- cbind(train_scaled,train[,8])
# test_new <- cbind(test_scaled,test[,8])
##----b & c---------- PCA
library('FactoMineR')
train_pca <- PCA(train[,1:7], scale.unit = TRUE, ncp =2)
summary(train_pca)
test_pca <- PCA(test[,1:7], scale. = TRUE)
summary(test_pca)
##----d---------- Naive Bayes Model
library('e1071')
NB_Model <- naiveBayes(loan_decision_status~.,train)
#plotting model

##----e----------
predicted_status <- predict(NB_Model, test, type = 'class')
##----f----------
library(caret)
confusionMatrix(predicted_status, test$loan_decision_status)

