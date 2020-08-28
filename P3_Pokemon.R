library('dplyr')
rm(list=ls())

setwd("C:/Users/cchousal/Documents/Data Science/FINAL PROJECT WORK/PROJECT-3-8211-POKEMON-01AUG2020063051")

Pokemon <- read.csv('P3_Pokemon.csv')

##------------------------------------------------Task 1-----------------------------------------------------------------
#Pokemon Selection for Fight

Pokemon %>% filter(Type.1 == 'Grass' & Type.2 == 'Poison') %>% filter(Speed == max(Speed)) -> P1
Pokemon %>% filter(Type.1 == 'Water' & Type.2 == 'Flying') %>% filter(Defense == max(Defense)) -> P2
Pokemon %>% filter(Type.1 == 'Fire' & Type.2 == 'Psychic') %>% filter(Attack == max(Attack)) -> P3
rbind(P1,P2,P3) -> My_Pokemon
cat('I Choose:',P1$Name,',',P2$Name,'&',P3$Name,'  for first battle')

##------------------------------------------------Task 2-----------------------------------------------------------------
#Attack Vs Defense
# Train Test Split
library(caTools)
sample.split(Pokemon$Attack,SplitRatio = 0.65) -> split_tag
train <- subset(Pokemon,split_tag == T) 
test <- subset(Pokemon,split_tag == F) 

# Building Liner Regression Model (x=Defense,y=Attack)
lm(Attack~Defense, data=train) -> Model1

#Predicting Model on Test Dataset
predict(Model1, newdata = test) -> Result1

#Comparing Predict vs Actual Values 
#Bind Them together
cbind(Actual=test$Attack,Predicted=Result1) -> Final_Data
as.data.frame(Final_Data) -> Final_Data
Final_Data$Actual - Final_Data$Predicted -> error
cbind(Final_Data,Error = error) -> Final_Data
sqrt((mean(Final_Data$Error^2))) -> RMSE

##------------------------------------------------Task 3-----------------------------------------------------------------
# Identify If Pokemon is Legendary or not
# Train Test Split

sample.split(Pokemon$Legendary,SplitRatio = 0.65) -> split_tag1
train1 <- subset(Pokemon,split_tag1 == T) 
test1 <- subset(Pokemon,split_tag1 == F)

#Recursive Partitioning Package
library(rpart)
# Building Decision Tree Model (x=Defense,y=Attack)
rpart(Legendary~., data=train1) -> Model2
#test1 <- test1[test1$Name %in% Pokemon$Name,]
#Predicting Model on Test Dataset
predict(Model2, newdata = test1,type="class") -> Result2
table(test1$Legandary,Result2)
#Accuracy
(257+9)/(257+9+14)
