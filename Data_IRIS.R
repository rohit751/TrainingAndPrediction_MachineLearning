#clear environment
rm(list = ls())

#Create data object - In this case: iris
data(iris)

#Create training & validation objects
trnIdx = sample(as.integer(rownames(iris)), floor(length(rownames(iris))*0.75))
trainData = iris[trnIdx,]
validData = iris[-trnIdx,]

#Training using LDA algorithm
library(MASS)
ldaMod = lda(formula = trainData$Species~., data = trainData)

#Prediction
ldaPred = predict(object = ldaMod, newdata = validData)

View(ldaPred)
