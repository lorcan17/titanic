#EDA of train file
library(readr)
library(dplyr)
library(ggplot2)
library(skimr)
library(broom)
library(Metrics)
library(corrplot)
library(caret)
train <- read_csv("train.csv")

train <-
  train %>% mutate(
    Survived = as.factor(Survived),
    Embarked = as.factor(Embarked),
    Sex = as.factor(Sex),
    Pclass = as.factor(Pclass)
  )
glimpse(train)

#can split out name to see if Dr etc.
#age contains NAs
#cabin contains missing values
#AMELIA II is a package that deals with missing values
#Or I could use na.pass in GLM to keep the NAs and build model off
#data including NAs

skimr::skim(train)
train %>% distinct(Embarked)
#C = Cherbourg, Q = Queenstown, S = Southampton

train %>% ggplot(aes(Survived, fill = Sex)) + geom_histogram(stat = "count")


train %>% ggplot(aes(Survived, fill = Embarked)) + geom_histogram(stat = "count")

train %>% ggplot(aes(Survived, fill = Pclass)) + geom_histogram(stat = "count")
train %>% ggplot(aes(Survived, fill = Sex)) + geom_histogram(stat = "count")

train %>% ggplot(aes(x = Pclass, y = Age, color = Survived)) + geom_boxplot()

train %>% ggplot(aes(x = Survived, y = Parch)) + geom_boxplot()

train %>% ggplot(aes(x = Pclass, y = SibSp, color = Survived)) + geom_boxplot()
train %>% filter(Pclass == 1) %>%  ggplot(aes(x = Pclass, y = Fare, color = Survived)) + geom_boxplot()
train %>% filter(Pclass != 1) %>%  ggplot(aes(x = Pclass, y = Fare, color = Survived)) + geom_boxplot()

correlations <-  train %>% select(everything(), - PassengerId, - Ticket, - Name, - Cabin) %>%  cor()

corrplot(correlations, method = "circle")

pairs(train, col=train$Survived)

x <- train %>% select(everything(), - PassengerId, - Ticket, - Name, - Cabin, - Survived)
y <- train %>% select(Survived)

scales <- list(x=list(relation="free"),y = list(relation= "free"))
featureplot(x=x, y=y, plot = "density", scales=scales)

scales
model <-
  glm(
    Survived ~ . - PassengerId - Ticket - Name - Cabin,
    train,
    family = "binomial"
  )
summary(model)

#To reduce the number of predictors I need to look at the following
# AIC, Bayesian IC, 
glm.fit <- predict(model, train, type = "response")

