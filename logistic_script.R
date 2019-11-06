## 1. Import the dataset

library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(MASS)
library(corrplot)

ad.data = read.csv('dataset/advertising.csv')
head(ad.data)
ad.data = ad.data[c(1,2,3,4,7,11,12)]
head(ad.data)

## 2. Rename the columns
colnames(ad.data) <- tolower(make.names(colnames(ad.data)))
head(ad.data)

## 3. Missing Values
### Check for missing values in dataframe
sapply(ad.data, function(x) sum(is.na(x)))

library('Amelia')
missmap(ad.data, y.at = c(1), col = c('yellow','black'), y.labels = c(' '))

## 4. Explanatory Data Analysis

correlations <- cor(ad.data)
corrplot(correlations, method="circle")

### Age Histogram Plot
theme_update(plot.title = element_text(hjust = 0.5))
ggplot(ad.data, aes( x = age)) + 
    geom_histogram(color = "darkblue", fill = "lightblue") +
    labs(title = "Age Histogram Plot", x = "Age", y = "Count")


ggplot(ad.data, aes( x = time.of.day)) + 
    geom_bar(color = "darkblue", fill = "lightblue") +
    labs(title = "Time of Day", x = "Time", y = "Count")

## 5. Functions

groupAge <- function(age){
    if(age <= 15){
        return(as.factor("0-15"))  
    }else if( (age >= 16) & (age <= 30) ){
        return(as.factor("16-30"))
    }else if( (age >= 31) & (age <= 45) ){
        return(as.factor("31-45"))
    }else if( (age >= 46) & (age <= 60) ){
        return(as.factor("46-60"))
    }else if( (age >= 61) & (age <= 75) ){
        return(as.factor("61-75"))
    }
}

ad.data$age <- sapply(ad.data$age, groupAge)


## 6. Encoding categorical data
str(ad.data)

## Time of day
levels(ad.data$time.of.day)
ad.data$time.of.day = factor(ad.data$time.of.day,
                  levels = c('Morning', 'Afternoon', 'Evening', 'Night'),
                  labels = c('Morning', 'Afternoon', 'Evening', 'Night'))

## Age
levels(ad.data$age)

ad.data$age = factor(ad.data$age,
                             levels = c('16-30', '31-45', '46-60','61-75'),
                             labels = c('16-30', '31-45', '46-60','61-75'))


## Gender
levels(ad.data$gender)
ad.data$gender = factor(ad.data$gender,
                             levels = c(1, 0),
                             labels = c('Male', 'Female'))


str(ad.data)
levels(ad.data$time.of.day)
levels(ad.data$age)
levels(ad.data$gender)

## 7. Split the dataset into Training set and test set
library(caTools)
#set.seed(121)
split = sample.split(ad.data$clicked.on.ad, SplitRatio = 0.8)
ad.training.set = subset(ad.data, split == TRUE)
ad.test.set = subset(ad.data, split == FALSE)

write.csv(ad.training.set, file = "training.csv")
write.csv(ad.test.set, file = "test.csv")

## 8. Fit Logistic Regression to the Training Set 
##colnames(ad.data) 
### daily.time.spent.on.site, age, area.income, daily.internet.usage, gender, time.of.day 
### clicked.on.ad

ad.regressor = glm(clicked.on.ad ~ .,
                family = binomial(link="logit"),
               data = ad.training.set)
summary(ad.regressor)


probabilities <- ad.regressor %>% predict(ad.test.set, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "Clicked", "Not Clicked")
table(predicted.classes)

## 9. Backward Elimination

# Consider the predictor with the highest p-value . If P > SL, remove the predictor

backwardElimination <- function(x, sl) {
    numVars = length(x)
    for (i in c(1:numVars)){
        regressor = glm(formula = clicked.on.ad ~ ., 
                        family = binomial(link="logit"),
                        data = x)
        maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|z|)"])
        if (maxVar > sl){
            j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|z|)"] == maxVar)
            x = x[, -j]
        }
        numVars = numVars - 1
    }
    return(regressor)
}

SL = 0.05
elimination.regressor = backwardElimination(ad.training.set, SL)
summary(elimination.regressor)
ad.test.set$predicted.clicked <- elimination.regressor %>% predict(ad.test.set, type = "response")
table(ad.test.set$clicked.on.ad, ifelse(ad.test.set$predicted.clicked > 0.5, "Clicked", "Not Clicked"))