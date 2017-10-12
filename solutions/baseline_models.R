require(gridExtra)
require(ggplot2)

wd = getwd()
csv <- read.csv(paste(wd, "College.csv", sep="/"))
rownames(csv)=csv[,1]
csv=csv[,-1]
df <- data.frame(csv)
nrows <- nrow(df)
trainSetMagnitude <- floor(nrows * .8)
allIndices <- sample(nrows, nrows)
trainSetRowIndices <- sample(1:nrows, trainSetMagnitude, replace=FALSE)
testSetRowIndices <- setdiff(allIndices, trainSetRowIndices)
train.set <- df[trainSetRowIndices, ]
test.set <- df[testSetRowIndices, ]
train.set$AcceptRate <- train.set$Accept / train.set$Apps
test.set$AcceptRate <- test.set$Accept / test.set$Apps
test.set$MeanPrediction <- mean(train.set$AcceptRate)
test.set$MeanResiduals <- test.set$AcceptRate - test.set$MeanPrediction
meanRMSE <- sqrt(mean(test.set$MeanResiduals**2))

test.set$MedianPrediction <- median(train.set$AcceptRate)
test.set$MedianResiduals <- test.set$AcceptRate - test.set$MedianPrediction
medianRMSE <- sqrt(mean(test.set$MedianResiduals**2))

ggplot(test.set, aes(x = AcceptRate)) + geom_point(aes(y=MeanResiduals), color="red") + geom_point(aes(y=MedianResiduals), color="blue")
