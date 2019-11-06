# DANA Advertising Report

## Import the libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(MASS)

## Import datasets
ad.data = read.csv("dataset/advertising.csv")
head(ad.data)

## Check the class
class(ad.data)

### Rename the column names
colnames(ad.data)
colnames(ad.data) <- tolower(make.names(colnames(ad.data)))
colnames(ad.data)

## Check the data structure
str(ad.data)

#### Time of Day

levels(ad.data$time.of.day)
ad.data$time.of.day = factor(ad.data$time.of.day, levels = c("Morning","Afternoon","Evening","Night"))
levels(ad.data$time.of.day)

#### Age
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
ad.data$age = factor(ad.data$age,
                     levels = c('16-30', '31-45', '46-60','61-75'),
                     labels = c('16-30', '31-45', '46-60','61-75'))
levels(ad.data$time.of.day)

## Check the data structure
head(ad.data)

## Check the summary
summary(ad.data)

## Age Histogram Plot
theme_update(plot.title = element_text(hjust = 0.5))
ggplot(ad.data, aes( x = age)) + 
    geom_histogram(color="darkblue",fill="lightblue") +
    labs(title="Age Histogram Plot",x="Age", y = "Count")

###################################
## 2-sample Test for Quantitative #
###################################

### 1. daily.time.spent.on.site

var.test(ad.data$clicked.on.ad, ad.data$daily.time.spent.on.site)
t.test(ad.data$clicked.on.ad, ad.data$daily.time.spent.on.site, var.equal = T)

### 2. age
var.test(ad.data$clicked.on.ad, ad.data$age)
t.test(ad.data$clicked.on.ad, ad.data$age,var.equal = T)

### 3. area.income
var.test(ad.data$clicked.on.ad, ad.data$area.income)
t.test(ad.data$clicked.on.ad, ad.data$area.income,var.equal = T)

### 4. daily.internet.usage
var.test(ad.data$clicked.on.ad, ad.data$daily.internet.usage)
t.test(ad.data$clicked.on.ad, ad.data$daily.internet.usage, var.equal = T)

###########################################
## 2-sample Test for Qualitative Variable #
###########################################

### 7. time.of.day
tbl.clicked.on.ad <- table(ad.data$clicked.on.ad, ad.data$time.of.day)
tbl.clicked.on.ad
chisq.test(tbl.clicked.on.ad)

### 7. age
tbl.age <- table(ad.data$clicked.on.ad,ad.data$age)
tbl.age
chisq.test(tbl.age)
chisq.test(tbl.age[rowSums(tbl.age)>0,])
chisq.test(tbl.age[rowSums(tbl.age)>0,], simulate.p.value = TRUE)