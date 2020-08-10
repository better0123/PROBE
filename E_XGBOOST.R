library(readr)

# train <- read_csv("NEW/train_all.csv")
# test <- read_csv("NEW/test_all.csv")

train <- read_csv("NEW/train_voluson.csv")
test <- read_csv("NEW/test_voluson.csv")

# train <- read_csv("NEW/train_toshiba.csv")
# test <- read_csv("NEW/test_toshiba.csv")

# train <- read_csv("NEW/train_samsung.csv")
# test <- read_csv("NEW/test_samsung.csv")

# train <- read_csv("NEW/train_esaote.csv")
# test <- read_csv("NEW/test_esaote.csv")

if(dim(train)[2] > dim(test)[2]){
  train <- train[,which(names(train) %in% names(test))]
} else if(dim(train)[2] < dim(test)[2]){
  test <- test[,which(names(test) %in% names(train))]
}

##################################################
#################################################

library(xgboost)
library(Matrix)

options(na.action='na.pass')

train.data = as.matrix(train[,-which(names(train) %in% "gBRCA_bin")])
train.label = train$gBRCA_bin
test.label = test$gBRCA_bin
test.data = as.matrix(test[,-which(names(test) %in% "gBRCA_bin")])

dtrain = xgb.DMatrix(data=train.data,label=train.label)
dtest = xgb.DMatrix(data=test.data,label=test.label)


searchGridSubCol <- expand.grid(subsample = c(0.2,0.5,0.8), 
                                colsample_bytree = c(0.2,0.5,0.8),
                                max_depth = c(1, 10, 50),
                                min_child = c(10,50,100), 
                                eta = c(0.5,0.05,0.01),
                                ntrees = c(5,10,100)
)


system.time(
  rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
    
    #Extract Parameters to test
    currentSubsampleRate <- parameterList[["subsample"]]
    currentColsampleRate <- parameterList[["colsample_bytree"]]
    currentDepth <- parameterList[["max_depth"]]
    currentEta <- parameterList[["eta"]]
    currentMinChild <- parameterList[["ntrees"]]
    currentNTrees <- parameterList[["min_child"]]
    xgboostModelCV <- xgb.cv(data =  dtrain, "nrounds" = currentNTrees, nfold = 5, showsd = TRUE, 
                             metrics = "error", verbose = TRUE, "eval_metric" = "error",
                             "objective" = "reg:logistic", "max.depth" = currentDepth, "eta" = currentEta,                               
                             "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate,
                              print_every_n = 10, "min_child_weight" = currentMinChild, booster = "gbtree",
                             early_stopping_rounds = 10)
    
    xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
    error <- tail(xvalidationScores$test_error_mean, 1)
    terror <- tail(xvalidationScores$train_error_mean,1)
    output <- return(c(error, terror, currentSubsampleRate, currentColsampleRate, currentDepth, currentEta, currentMinChild,currentNTrees))}))

output <- as.data.frame(t(rmseErrorsHyperparameters))
varnames <- c("TrainError", "TestError", "SubSampRate", "ColSampRate", "Depth", "eta", "currentMinChild", "currentNTrees")
names(output) <- varnames
output <-output[order(output$TestError),]
head(output)

riga_output <- 1


library(caret)


xgb.model <- xgboost(data=train.data, label=train.label,nrounds= output$currentNTrees[riga_output],
                       currentSubsampleRate = output$SubSampRate[riga_output],  metrics = "error",
                       max.depth = output$Depth[riga_output],
                       eval_metric = "error", colsample_bytree = output$ColSampRate[riga_output],
                       min_child_weight = output$currentMinChild[riga_output], objective='reg:logistic',
                       eta =output$eta[riga_output], booster = "gbtree")
  
prediction_train <- predict(xgb.model, train.data)
confusionMatrix(as.factor(round(prediction_train)),reference = as.factor(train.label),positive = "1")
  
prediction_test <- predict(xgb.model, test.data)
confusionMatrix(as.factor(round(prediction_test)),reference = as.factor(test.label),positive = "1")


cat("accuracy")
sum(diag(cm))/sum(cm)

cat("specificity")
cm[1,1]/(cm[1,1]+cm[2,1])

cat("sensitivity")
cm[2,2]/(cm[2,2]+cm[1,2])

cat("NPV")
cm[1,1]/(cm[1,1]+cm[1,2])

cat("PPV")
cm[2,2]/(cm[2,2]+cm[2,1])

a <- roc(train.label,prediction_train)
plot(a,main=paste("AUC:",a$auc))
a <- roc(test.label,prediction_test)
plot(a,main=paste("AUC:",a$auc))


importance <- xgb.importance(colnames(train.data), model = xgb.model)
print(xgb.plot.importance(importance_matrix = importance, top_n = 15))