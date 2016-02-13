
library(caret)
train_in <- read.csv('./pml-training.csv', header=T)
validation <- read.csv('./pml-testing.csv', header=T)
#training$classe <- as.factor(training$classe)
#testing$classe <- as.factor(testing$classe)

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
model <- train(
  classe ~ ., data=training,
  trControl=fitControl,
  #preProcess=c('center', 'scale'),
  method='rf'
)
save(model, './ModelFitRandomForest.RData')
# plot(model$finalModel)
# text(model$finalModel)

pred <- predict(model, newdata=testing)
confusionMatrix(pred, na.omit(testing)$classe)
