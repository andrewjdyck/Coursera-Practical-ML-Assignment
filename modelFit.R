
library(caret)
training <- read.csv('./pml-training.csv', header=T)
testing <- read.csv('./pml-testing.csv', header=T)

set.seed(127)
#folds <- createFolds(y=training$classe, k=3, list=TRUE, returnTrain=TRUE)

preObj <- preProcess(training[, 1:(nrow(training)-1)], method =c('center', 'scale'))
featurePlot(x=training[, 1:10], y=training$classe, plot = 'pairs')

fitControl <- trainControl(method='cv', number = 3)

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


model <- train(
  classe ~ var_accel_forearm, data=training,
  trControl=fitControl,
  preProcess=c('center', 'scale'),
  method='rpart'
)


