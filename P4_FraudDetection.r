library(dplyr)
setwd("C:/Users/cchousal/Documents/Data Science/FINAL PROJECT WORK/PROJECT-4-8211-FRAUD-DETECTION-01AUG2020154355")
CreditCard <- read.csv('P4_CreditCard.csv')
summary(CreditCard)
str(CreditCard)
# Convert class column into to factor
CreditCard$Class  <- factor(CreditCard$Class, levels = c(0,1))
sum(is.na(CreditCard)) # To check blanks in data
prop.table(table(CreditCard$Class)) #Its showing that data is unbalanced. as number of 1 is very less

# Train-Test
library(caTools)
sample.split(CreditCard$Class, SplitRatio = 0.8) -> Split_Tag
train <- subset(CreditCard, Split_Tag == T)
test <- subset(CreditCard, Split_Tag == F)
table(train$Class)

train$Class  <- factor(train$Class, levels = c(0,1))
test$Class  <- factor(test$Class, levels = c(0,1))
table(train$Class)


# Balance the training data.
#(Method1: ROS: Random oversampling. Increasing count of fraud data)
#(Method2: RUS: Random undersampling. Reducing count of Legitimate data)
#(Method3: ROS & RUS Both: Random undersampling. Reducing count of Legitimate data)
# Method 1,2 and 3is balancing data but by creating duplicate cases. So ethod 4 is required.
#(Method4: SMOTE: It will create synthetic fraud cases by using k nearest neighbors {Non overlapping})


library(smotefamily)
# Set number of Legitimate, Fraud cases and desired % of legitimate cases.
table(train$Class)
n0 <- 13955
n1 <- 45
r0 <- 0.6
# How many synthetic cases we want?
ntimes = ((1-r0)/r0)* (n0/n1)-1
smote_op <- SMOTE(X= train[,-c(1,31)], target = train$Class, K = 5, dup_size = ntimes)

train_smote <- smote_op$data
colnames(train_smote)[30] <- "Class"

# Model Building
library(rpart)
library(rpart.plot)
Model <- rpart(Class~., train)
Model_smote <- rpart(Class~., train_smote)
rpart.plot(Model_smote, extra = 0, type = 5, tweak = 1.2)

# Prediction on test dataset

#predicted_val <- predict(Model, test, type = 'class')
predicted_val <- predict(Model_smote, test, type = 'class')

# Compare Predicted vs Actual
library(ggplot2)
library(caret)
confusionMatrix(predicted_val, test$Class)