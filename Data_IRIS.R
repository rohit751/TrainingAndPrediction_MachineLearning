# clear environment
rm(list = ls())

# Create data object - In this case: iris ####
data(iris)

# Create training & validation objects ####
trnIdx = sample(as.integer(rownames(iris)), floor(length(rownames(iris))*0.75))
trainData = iris[trnIdx,]
validData = iris[-trnIdx,]
#View(trainData)
#View(validData)

# Verify the objects
trainNvalid = union(trnIdx, as.integer(rownames(validData)))
setdiff(trainNvalid, as.integer(rownames(iris)))
# clean up environment
rm(trainNvalid)

# Training - LDA model ####
library(MASS)
ldaMod = lda(formula = trainData$Species~., data = trainData)

# Prediction - LDA model ####
ldaPred = predict(object = ldaMod, newdata = validData)
#View(ldaPred)

# Validation - LDA model ####
validData$LdaPredClass = ldaPred$class
validData$LdaPredProb = apply(ldaPred$posterior, MARGIN = 1, FUN = max)
View(validData)

# Performance - LDA model ####
ldaCorrect = sum(validData$Species == validData$LdaPredClass)
ldaIncorrect = length(validData$Species) - ldaCorrect
ldaErrorRate = ldaIncorrect/length(validData$Species)

# Display results - LDA model ####
sprintf("LDA Model Error Rate: %s%%", format(ldaErrorRate*100, digits = 4))



# Training - QDA model ####
qdaMod = qda(formula = trainData$Species~., data = trainData)

# Prediction - QDA model ####
qdaPred = predict(object = qdaMod, newdata = validData)
#View(qdaPred)

# Validation - QDA model ####
validData$QdaPredClass = qdaPred$class
validData$QdaPredProb = apply(qdaPred$posterior, MARGIN = 1, FUN = max)
View(validData)

# Performance - QDA model ####
qdaCorrect = sum(validData$Species == validData$QdaPredClass)
qdaIncorrect = length(validData$Species) - qdaCorrect
qdaErrorRate = qdaIncorrect/length(validData$Species)

# Display results - QDA model ####
sprintf("QDA Model Error Rate: %s%%", format(qdaErrorRate*100, digits = 4))



# Training - Random Forest model ####
library(randomForest)
rfMod = randomForest(formula = trainData$Species~., data = trainData)

# Prediction - Random Forest model ####
rfPred = predict(object = rfMod, newdata = validData, type = 'prob')
#View(rfPred)

# Validation - Random Forest model ####
validData$RfPredClass = colnames(rfPred)[max.col(rfPred, ties.method="first")]
validData$RfPredProb = apply(rfPred, MARGIN = 1, FUN = max)
View(validData)

# Performance - Random Forest model ####
rfCorrect = sum(validData$Species == validData$RfPredClass)
rfIncorrect = length(validData$Species) - rfCorrect
rfErrorRate = rfIncorrect/length(validData$Species)

# Display results - Random Forest model ####
sprintf("Random Forest Model Error Rate: %s%%",
        format(rfErrorRate*100, digits = 4))
