# Clean environment
rm(list = ls())
dev.off()

# Retrieve training & evaluating data from the datasets
library(MASS)
trainData = Pima.tr
evalData = Pima.te

# Set required variables ####
thresold = 0.9
targetVar = "type"

# Training - Random Forest ####
library(randomForest)
trainFormula = formula(paste0(targetVar, " ~ ."))
rfMod = randomForest(formula = trainFormula, data = trainData)

# Prediction - Random Forest ####
rfPred = predict(object = rfMod, newdata = evalData, type = 'prob')

# Validation - Random Forest ####
evalData$rfPred = colnames(rfPred)[max.col(rfPred, ties.method = 'first')]
evalData$rfProb = apply(rfPred, MARGIN = 1, FUN = max)

# Performance (threshold = 90%) - Random Forest ####
evalData$rfPred[which(evalData$rfProb<thresold)] = 'N/A'
rfTotalPred_90 = sum(evalData$rfPred != 'N/A')
rfCorrect_90 = sum(evalData$rfPred == evalData$type)
rfIncorrect_90 = rfTotalPred_90 - rfCorrect_90
rfErrRate_90 = rfIncorrect_90/rfTotalPred_90

# Display Results - Random Forest ####
sprintf("Random Forest Model Error Rate(90%% threshold): %s%%",
        format(rfErrRate_90*100, digits = 4))

# Prediction Matrix - Random Forest ####
predMat = matrix(data = 'NAN', nrow = 101, ncol = 3)
colnames(predMat) = c('Thresold', 'ErrorRate', 'PredictionRate')
for(i in 1:dim(predMat)[1])
{
    predMat[i, 1] = (i-1)/(dim(predMat)[1]-1)
    Incorrect = sum(evalData$rfPred != evalData$type &
                        evalData$rfProb >= predMat[i, 1])
    totalPred = sum(evalData$rfProb >= predMat[i, 1])
    predMat[i, 2] = Incorrect/totalPred
    predMat[i, 3] = totalPred/length(evalData$rfPred)
}

#Graphical representation - Random Forest ####
par(mar=c(5, 4, 4, 4))
myGraph = recordPlot()
plot(predMat[, 1], predMat[, 2], main = 'Prediction Threshold',
     xlab = "Threshold", ylab = "", axes = FALSE,
     col = 'red', type = 'l')
axis(side = 1, at = predMat[, 1][seq(1, length(predMat[, 1]), 10)])
axis(side = 2, ylim = c(0, max(predMat[, 2])), col = c('red', 'red'),
     col.axis = 'red')
mtext(side = 2, "Error Rate", line = 2.5, col = 'red')
par(new = TRUE)
plot(predMat[, 1], predMat[, 3], xlab = "", ylab = "",
     axes = FALSE, col = 'green', type = 'l')
axis(side = 4, ylim = c(0, max(predMat[, 3])), col = c('green', 'green'),
     col.axis = 'green')
mtext(side = 4, "Prediction Rate", line = 2.5, col = 'green')
legend("bottomleft", legend = c('Error Rate', 'Prediction Rate'),
       text.col = c('red', 'green'), col = c('red', 'green'), lty = 1)
box()
replayPlot(myGraph)
