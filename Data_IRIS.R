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

# Training using LDA algorithm ####
library(MASS)
ldaMod = lda(formula = trainData$Species~., data = trainData)

# Prediction using the LDA model####
ldaPred = predict(object = ldaMod, newdata = validData)
#View(ldaPred)

# Validation ####
validData$LdaPredClass = ldaPred$class
validData$LdaPredProb = apply(ldaPred$posterior, MARGIN = 1, FUN = max)
View(validData)

# Performance ####
ldaCorrect = sum(validData$Species == validData$LdaPredClass)
ldaIncorrect = length(validData$Species) - ldaCorrect
ldaErrorRate = ldaIncorrect/length(validData$Species)

# Display results ####
sprintf("LDA Model Error Rate: %s%%", format(ldaErrorRate*100, digits = 3))
