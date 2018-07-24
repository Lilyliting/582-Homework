
setwd("/Users/apple/Desktop/582/582_3")
library(quantmod)
library(rpart)
library(rpart.plot)
library (ISLR)
library (MASS)
library (class)
startDate = as.Date("2012-01-01")
endDate = as.Date("2014-01-01")
getSymbols("PEP", from = startDate, to = endDate) # Get the stock symbol

# 1 
MACD <- MACD(Op(PEP),fast = 12, slow = 26, signal = 9)
#Calculate a MACD with standard parameters
MACDsignal <- MACD[, 2]
#Grab just the signal line to use as our indicator.

# 2
SMI <- SMI(Op(PEP),n=13,slow=25,fast=2,signal=9) 
#Stochastic Oscillator with standard parameters
SMI <- SMI[, 1]
#Grab just the oscillator to use as our indicator

# 3
EMA5 <- EMA(Op(PEP), n = 5)
#Calculate a 5-period exponential moving average (EMA)

# 4
EMA15 <- EMA(Op(PEP), n = 15)
#Calculate a 15-period exponential moving average (EMA)

# 5
RSI3 <- RSI(Op(PEP), n = 3)
#Calculate a 3-period relative strength index (RSI) off the open price

# 6
RSI10 <- RSI(Op(PEP), n = 10)
#Calculate a 10-period relative strength index (RSI) off the open price

# 7
ROC <- ROC(Op(PEP), n=1, type = c( "discrete"))
#Calculate Rate Of Change (ROC)

# 8
ADX7 <- ADX(HLC(PEP), n = 7)
ADX7 <- ADX7[, 4]
#Calculate a 7-period Average Direction Index (ADX)

# 9
ADX14 <- ADX(HLC(PEP), n = 14)
ADX14 <- ADX14[, 4]
#Calculate a 14-period Average Direction Index (ADX)

# 10
Volume <- Vo(PEP)
#Get volume

#Then calculate the variable we are looking to predict and build our data sets.
PriceChange <- Cl(PEP) - Op(PEP)
#Calculate the difference between the close price and open price
Class <- ifelse(PriceChange > 0,"UP","DOWN")
#Create a binary classification variable, the variable we are trying to predict.
DataSet <- data.frame(MACDsignal, SMI, EMA5, EMA15, RSI3, RSI10, 
                      ROC, ADX7, ADX14, Volume, Class)
#Create our data set
colnames(DataSet)<-c("MACDsignal", "SMI", "EMA5", "EMA15", "RSI3", "RSI10", 
                     "ROC", "ADX7", "ADX14", "Volume", "Class") 
#Name the columns
DataSet <- DataSet[-c(1:33),]
#Get rid of the data where the indicators are being calculated
TrainingSet <- DataSet[1:312,]
#Use 2/3 of the data to build the tree
TestSet <- DataSet[313:469,]
#And leave out 1/3 data to test our strategy

DecisionTree<-rpart(Class~MACDsignal + SMI + EMA5 + EMA15 + RSI3 +  RSI10 + 
                    ROC + ADX7 + ADX14 + Volume, data=TrainingSet, cp=.001)

prp(DecisionTree,type=2,extra=8)

printcp(DecisionTree)
#shows the minimal cp for each trees of each size.
plotcp(DecisionTree,upper="splits")
#plots the average geometric mean for trees of each size.

PrunedDecisionTree <- prune(DecisionTree, cp = 0.039683)
# Selecting the complexity parameter (cp) that has the lowest cross-validated error (xerror)
prp(PrunedDecisionTree, type=2, extra=8)

table(predict(PrunedDecisionTree,TestSet,type="class"),TestSet[, 11],dnn=list('predicted','actual'))

# Logistic Regression
glm.fit <- glm(Class~MACDsignal + SMI + EMA5 + EMA15 + RSI3 +  RSI10 + 
                   ROC + ADX7 + ADX14 + Volume, data=TrainingSet, family=binomial)

glm.probs <- predict(glm.fit, TestSet, type ="response")

glm.pred=rep("DOWN", 157)
glm.pred[glm.probs >.5]="UP"

table(glm.pred, Class[313:469,])
mean(glm.pred == Class[313:469,])
# 0.4585987

# Linear Discriminant Analysis
lda.fit <- lda(Class~MACDsignal + SMI + EMA5 + EMA15 + RSI3 +  RSI10 + 
                   ROC + ADX7 + ADX14 + Volume, data=TrainingSet)
lda.fit
plot(lda.fit)

lda.pred <- predict(lda.fit, TestSet)
lda.class <- lda.pred$class
table(lda.class, Class[313:469,])
# 0.4585987

# Quadratic Discriminant Analysis
qda.fit <- qda(Class~MACDsignal + SMI + EMA5 + EMA15 + RSI3 +  RSI10 + 
                   ROC + ADX7 + ADX14 + Volume, data=TrainingSet)
qda.fit

qda.probs <- predict(qda.fit, TestSet)
qda.class <- qda.probs$class
table(qda.class, Class[313:469,])
mean(qda.class == Class[313:469,])
# 0.4713376

