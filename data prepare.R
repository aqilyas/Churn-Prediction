###***************preprocess for SVM and KNN*******************

setwd("C:/Users/vPro/Desktop/Groningen/Rug/paper/data")
data = read.csv("telco_churn.csv")
data$SeniorCitizen = as.factor(data$SeniorCitizen)

library(caret)
library(e1071)

#selecting factor variables to create dummies
dmdata = data[, sapply(data, is.factor) & colnames(data) != "customerID"]

#creating dummy variables
dm = dummyVars(~ ., data = dmdata[,-17], levelsOnly = FALSE)
newdmdata = data.frame(predict(dm, newdata = dmdata[-17]))

numdata = data[, sapply(data, is.numeric)]

#combining columns together
readydata = data.frame(newdmdata, numdata)
#transforming and adding churn column
data$Churn[data$Churn == "Yes"] = 1
data$Churn[data$Churn == "No"] = 0
data$Churn = as.numeric(data$Churn)
readydata = data.frame(readydata, data$Churn)

#Scaling numerical vars
scalenumdata = scalenumdata[,1:3]
rsdata = data.frame(readydata[,1:43], scalenumdata, readydata[,47])
rm(readydata)
names(rsdata)[47] = "Churn"
rsdata$Churn = as.factor(rsdata$Churn)
#scalenumdata = scale(readydata[,-(1:43)])
#scalenumdata = as.data.frame(scalenumdata)

## Keeping 20% of data for final testing and comparison

trainIndex <- createDataPartition(rsdata$Churn, p = .8, list = FALSE)
Churntest = rsdata[-trainIndex,]
Churncv = rsdata[trainIndex,]

## removing NAs
rsdata = rsdata[complete.cases(rsdata),]

## loading ROC library for evaluation
library(pROC)








