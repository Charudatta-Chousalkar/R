rm(list=ls())

library(dplyr)
library(ggplot2)

setwd("C:/Users/cchousal/Documents/Data Science/FINAL PROJECT WORK/PROJECT-7-8211-CENSUS-PROJECT-PART-1-09AUG2020054011")
census <- read.csv('P6_census-income_.csv')
str(census)
summary(census)
str(census)
census %>% ggplot(aes(x=X), fill =factor(X))+geom_bar(color = 'red')

##------------------------------------------------1. Data Preprocessing-----------------------------------------------------------------

sum(is.na(census))
sum(is.null(census))

#Remove rows with missing values
#After checking data, its observed that missing values are reprented by '?'. So replace ? with NA
census[census == ' ?'] <- NA
sum(is.na(census))
census <- na.omit(census)

#Remove whitespaces (which is present before many words in data) with str_trim if column is character type
library(stringr)
census %>% mutate_if(is.character,str_trim) -> census

##------------------------------------------------2. Data Manipulation-----------------------------------------------------------------

census_ed <- census %>% select(education)
head(census_ed)

census_seq <- census %>% select(age:relationship)
head(census_seq)

census_col <- census %>% select(c(5,8,11))
head(census_col)

male_gov <- census %>% filter(sex == 'Male' & workclass == "State-gov")
head(male_gov)

census_us <- census %>% select(age,education,native.country) %>% filter(age == 39 & (education == 'Bachelors' | native.country == 'United-States'))
head(census_us)

#set.seed(1) will give same rows in sample otherwise random rows
census_200 <- census %>% sample_n(200) 

class(census$workclass)
census$workclass <- as.factor(census$workclass)
census %>% count(workclass)

census %>% group_by(workclass) %>% summarise(mean(capital.gain))

##------------------------------------------------3. Data Visualization-----------------------------------------------------------------

#Bar Plot
#By race
census %>% ggplot(aes(x=relationship, fill = race)) + 
  geom_bar() +
  labs(x= 'Categories Of Relatinship' , y= 'Count Of Categories')

#By Sex
census %>% ggplot(aes(x=relationship, fill = sex)) + 
  geom_bar(position = "dodge") +
  labs(x= 'Categories Of Relatinship' , y= 'Count Of Categories', title = 'Distribution Of Relationships by Sex')
  

#Histogram
census %>% ggplot(aes(x=age, fill = X)) + 
  geom_histogram(bins = 50) + 
  labs(title = 'Distribution Of Age', fill = 'Yearly Income') +
  theme_bw()

# Scatter Plot
census %>% ggplot(aes(x=capital.gain, y=hours.per.week, col = X)) + 
  geom_point(alpha = 0.6, size = 2) +
  labs(x= 'Capital Gain' , y= 'Hours per Week', title = 'Capital Gain Vs Hours per Week By Income', col = 'Yearly Income')

# Box Plot
census %>% ggplot(aes(x=education, y=age,fill = sex)) + 
  geom_boxplot() +
  labs(title = 'Box Plot of Age by Education and Sex')

##------------------------------------------------4. Linear Regression-----------------------------------------------------------------
## Train-Test Split
library(caTools)
split <- sample.split(census$hours.per.week,SplitRatio = 0.7)
train <- subset(census, split == T)
test <- subset(census, split == F)

Lin_Model <- lm(hours.per.week~education.num, data = train)
summary(Lin_Model)
Result <- predict(Lin_Model, newdata = test)
Final_Result <- cbind(Actual = test$hours.per.week, Predicted = Result)
Final_Result <- as.data.frame(Final_Result)
Error <- Final_Result$Actual - Final_Result$Predicted
Final_Result <- cbind(Final_Result,Error)
head(Final_Result)
rmse <- sqrt(mean(Final_Result$Error^2))

##------------------------------------------------5. Logistic Regression-----------------------------------------------------------------
## a) Simple Log Reg
## Train-Test Split
str(census)
census$X <- as.factor(census$X) 

split <- sample.split(census$X,SplitRatio = 0.65)
train <- subset(census, split == T)
test <- subset(census, split == F)

Log_Model <- glm(X~occupation,data = train, family='binomial')
summary(Log_Model)

Predict <- predict(Log_Model,newdata = test, type = 'response')
range(Predict)

library(ROCR)
predict_log_roc <- prediction(Predict, test$X)
acc <- performance(predict_log_roc,'acc')
plot(acc)

table(census$X)
range(Predict)

lm.pred <- ifelse(Predict > 0.48, '>50K', '<=50K')

tab <- table(lm.pred,test$X)
Accuracy <- sum(diag(tab))/sum(tab)
#OR (instead of above two lines)
library(caret)
confusionMatrix(factor(lm.pred),test$X)

roc <- performance(predict_log_roc,'tpr','fpr')
plot(roc)
auc <- performance(predict_log_roc,'auc')
auc <- auc@y.values[1]

## b) Multiple Log Reg

split <- sample.split(census$X,SplitRatio = 0.8)
train <- subset(census, split == T)
test <- subset(census, split == F)

multiLog_Model <- glm(X~age+workclass+education,data = train, family='binomial')
summary(multiLog_Model)

mPredict <- predict(multiLog_Model,newdata = test, type ='response')
mpredict_log_roc <- prediction(mPredict, test$X)
m_acc <- performance(mpredict_log_roc,'acc')
plot(m_acc)

m_lm.pred <- ifelse(mPredict > 0.45, '>50K', '<=50K')

confusionMatrix(factor(m_lm.pred),test$X)

m_roc <- performance(mpredict_log_roc,'tpr','fpr')
plot(m_roc)
m_auc <- performance(mpredict_log_roc,'auc')
m_auc <- m_auc@y.values[1]

##------------------------------------------------6. Decision Tree-----------------------------------------------------------------
library(rpart)
library(rpart.plot)
str(census)
census$native.country <- as.factor(census$native.country) 
split <- sample.split(census$X,SplitRatio = 0.7)
train <- subset(census, split == T)
test <- subset(census, split == F)
test <- test[test$native.country %in% train$native.country,] # So that no new level appear in prediction with test set.

dec_tree <- rpart(X~.,data = train, method = 'class')
rpart.plot(dec_tree, extra = 0, type = 5, tweak = 1.2)

pred_val <- predict(dec_tree, newdata = test, type='class')

confusionMatrix(pred_val, test$X)

##------------------------------------------------7. Random Forest-----------------------------------------------------------------
library(randomForest)
split <- sample.split(census$X,SplitRatio = 0.8)
train <- subset(census, split == T)
test <- subset(census, split == F)
#test <- test[test$native.country %in% train$native.country,] # So that no new level appear in prediction with test set.

rand_forest <- randomForest(X~.,data = train, ntree=300)
plot(rand_forest)

pred_val2 <- predict(rand_forest, newdata = test)

confusionMatrix(pred_val2, test$X)
