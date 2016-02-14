
library(caret)
train_in <- read.csv('./pml-training.csv', header=T)
validation <- read.csv('./pml-testing.csv', header=T)
#training$classe <- as.factor(training$classe)
#testing$classe <- as.factor(testing$classe)

# find only nonzero columns in validation set
all_zero_colnames <- sapply(names(validation), function(x) all(is.na(validation[,x])==TRUE))
nznames <- names(all_zero_colnames)[all_zero_colnames==FALSE]
nznames <- nznames[-(1:7)]
nznames <- nznames[1:(length(nznames)-1)]

set.seed(127)
training_sample <- createDataPartition(y=train_in$classe, p=0.7, list=FALSE)
training <- train_in[training_sample, ]
testing <- train_in[-training_sample, ]
#folds <- createFolds(y=training$classe, k=3, list=TRUE, returnTrain=TRUE)

#preObj <- preProcess(training[, 1:(nrow(training)-1)], method =c('center', 'scale'))
#featurePlot(x=training[, 15:17], y=training$classe, plot = 'pairs')

# PCA
#prComp <- prcomp(var1, var2)
#preProc <- preProcess(training[, 1:2], method = 'pca', pcaComp = 2)
#testPC <- predict(preProc, testing[, 1:2]) # predicts preprocessed training data

#feature selection
#set.seed(127)
# ctrl <- rfeControl(functions = lmFuncs,
#                    method = "repeatedcv",
#                    repeats = 5,
#                    verbose = FALSE)
# lmProfile <- rfe(x, y,
#                  sizes = subsets,
#                  rfeControl = ctrl)
# lmProfile

fitControl <- trainControl(method='cv', number = 3)
model_cart <- train(
  classe ~ ., 
  #data=training[, 8:ncol(training)],
  data=training[, c('classe', nznames)],
  trControl=fitControl,
  #preProcess=c('center', 'scale'),
  method='rpart'
)
save(model_cart, file='./ModelFitCART.RData')
model_gbm <- train(
  classe ~ ., 
  #data=training[, 8:ncol(training)],
  data=training[, c('classe', nznames)],
  trControl=fitControl,
  #preProcess=c('center', 'scale'),
  method='gbm'
)
save(model_gbm, file='./ModelFitGBM.RData')
model_rf <- train(
  classe ~ ., 
  #data=training[, 8:ncol(training)],
  data=training[, c('classe', nznames)],
  trControl=fitControl,
  #preProcess=c('center', 'scale'),
  method='rf',
  ntree=100
)
save(model_rf, file='./ModelFitRF.RData')
# plot(model$finalModel)
# text(model$finalModel)

#load('./ModelFitRF.RData')

predCART <- predict(model_cart, newdata=testing)
cmCART <- confusionMatrix(predCART, testing$classe)

predGBM <- predict(model_gbm, newdata=testing)
cmGBM <- confusionMatrix(predGBM, testing$classe)

predRF <- predict(model_rf, newdata=testing)
cmRF <- confusionMatrix(predRF, testing$classe)

AccuracyResults <- data.frame(
  Model = c('CART', 'GBM', 'RF'),
  Accuracy = rbind(cmCART$overall[1], cmGBM$overall[1], cmRF$overall[1])
)


