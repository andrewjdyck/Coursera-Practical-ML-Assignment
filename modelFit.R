
library(caret)
train_in <- read.csv('./pml-training.csv', header=T)
test_in <- read.csv('./pml-testing.csv', header=T, colClasses = sapply(training, class))
#training$classe <- as.factor(training$classe)
#testing$classe <- as.factor(testing$classe)

training
set.seed(127)
#folds <- createFolds(y=training$classe, k=3, list=TRUE, returnTrain=TRUE)

preObj <- preProcess(training[, 1:(nrow(training)-1)], method =c('center', 'scale'))
featurePlot(x=training[, 15:17], y=training$classe, plot = 'pairs')

# PCA
#prComp <- prcomp(var1, var2)
#preProc <- preProcess(training[, 1:2], method = 'pca', pcaComp = 2)
#testPC <- predict(preProc, testing[, 1:2]) # predicts preprocessed training data

#feature selection
set.seed(10)
ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)
lmProfile <- rfe(x, y,
                 sizes = subsets,
                 rfeControl = ctrl)
lmProfile

fitControl <- trainControl(method='cv', number = 3)
model <- train(
  classe ~ ., data=training[, 8:ncol(training)],
  trControl=fitControl,
  #preProcess=c('center', 'scale'),
  method='rpart'
)
plot(model$finalModel)
text(model$finalModel)

pred <- predict(model, newdata=testing[, 8:ncol(testing)])
confusionMatrix(pred, testing$classe)