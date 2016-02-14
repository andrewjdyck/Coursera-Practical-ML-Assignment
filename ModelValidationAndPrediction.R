
rm(list=ls(all=TRUE))
library(caret)
load('./ModelFitRandomForest.RData')
train_in <- read.csv('./pml-training.csv', header=T, na.strings = c("", "#DIV/0!", "NA"))
validation <- read.csv('./pml-testing.csv', header=T, na.strings = c("", "#DIV/0!", "NA"))

train_class <- sapply(train_in, class)
validation_class <- sapply(validation, class)
coltest <- data.frame(
  TrainingClasses=train_class,
  ValidationClasses=validation_class,
  stringsAsFactors = FALSE
)
v2 <- read.csv('./pml-testing.csv', header=T, colClasses = train_class)
t2 <- read.csv('./pml-training.csv', header=T, colClasses = train_class)

logical_to_numeric <- row.names(coltest)[coltest$ValidationClasses == 'logical' & coltest$TrainingClasses == 'numeric']
logical_df <- validation[, logical_to_numeric]
v2 <- validation
v2[, logical_to_numeric] <- sapply(v2[, logical_to_numeric], as.numeric)

coltest2 <- data.frame(
  TrainingClasses=train_class,
  ValidationClasses=sapply(v2, class),
  stringsAsFactors = FALSE
)


predValidation <- predict(model, newdata=validation)
answers <- data.frame(
  problem_id=validation$problem_id,
  predicted=predValidation
)
