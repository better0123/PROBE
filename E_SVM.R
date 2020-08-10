library(pROC)
library(Epi)
library(caret)
library(nnet)
library(NLP)
library(formula.tools)
library(e1071)

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

train <- train[,-1]
test <- test[,-1]
train$gBRCA_bin <- as.factor(train$gBRCA_bin)
test$gBRCA_bin <- as.factor(test$gBRCA_bin)

covs <- names(train)[-which(names(train) %in% c("gBRCA_bin"))]
formula_base <- "gBRCA_bin ~ "
tmp_res <- list()
i <- 1
riga <- 1
aic_best_kappa <- 2000
best_kappa <- 0
already_selected <- c()

for(i in 1:30){
  riga <- 1
  tmp_res[[i]] <- as.data.frame(matrix(data = NA, nrow = length(covs), ncol = 4))
  names(tmp_res[[i]]) <- c("formula","kappa","aic","AUC")
  for(cov in covs){
    cat(paste(paste("doing",i) ,"var cov..."))
    cat("\n")
    cat(cov)
    cat("\n")
    
    
    if(cov %in% c("min_50.100","min_0.50","min_0.100","F_stat.max","F_stat.range","F_szm.lze",already_selected)){
      tmp_res[[i]][riga,"formula"] <- ""
      tmp_res[[i]][riga,"kappa"] <- 0
      tmp_res[[i]][riga,"aic"] <- 0
      tmp_res[[i]][riga,"AUC"] <- 0
    } else{
      formula <- as.formula(paste(formula_base,paste("+",cov)))
      logitmod <- svm(formula, data=train,probability = T)
      pred_tmp <- predict(logitmod, newdata = train, type = "response",probability = T)
      pred <- attr(pred_tmp,"probabilities")[,"1"]
      rock = ROC(test = pred, stat = train$gBRCA_bin)
      #thresh <- which.max(rowSums(rock$res[, c("sens", "spec")])) 
      thresh <- which.max(rowSums(rock$res[, c("sens", "spec")])) 
      thresh <- round(as.numeric(names(thresh)),3)
      if(length(thresh) == 0) next()
      else if(is.na(thresh)) next()
      pred_class <- pred
      pred_class[which(pred_class < thresh)] <- 0
      pred_class[which(pred_class > thresh)] <- 1
      cm <- confusionMatrix(as.factor(pred_class), as.factor(train$gBRCA_bin),positive = "1")
      
      tmp_res[[i]][riga,"formula"] <- as.character(formula)
      tmp_res[[i]][riga,"kappa"] <- cm$overall[2]
      tmp_res[[i]][riga,"aic"] <- 0
      tmp_res[[i]][riga,"AUC"] <- rock$AUC
      
      riga <- riga + 1
    }
  }
  
  tmp_res[[i]] <- tmp_res[[i]][complete.cases(tmp_res[[i]]),]
  kappa_nuova <- tmp_res[[i]][which.is.max(tmp_res[[i]]$kappa),"kappa"]
  
  if(kappa_nuova >= best_kappa){
    formula_base <- tmp_res[[i]][which.is.max(tmp_res[[i]]$kappa),"formula"]
    best_kappa <- tmp_res[[i]][which.is.max(tmp_res[[i]]$kappa),"kappa"]
    aic_best_kappa <- 0
    already_selected <- c(already_selected,gsub(" ", "", sub('.*\\+', '', formula_base), fixed = TRUE))
  } else{
    result <- list("formula_base"=formula_base,"best_kappa" = best_kappa,"aic_best_kappa" = aic_best_kappa)
    return(result)
  }
  
  i <- i + 1
}

# scelgo il best

index <- length(tmp_res) - 1


logitmod <- svm(as.formula(tmp_res[[index]][which.is.max(tmp_res[[index]]$kappa),"formula"]), data=train, probability = T)
pred_tmp <- predict(logitmod, newdata = train, type = "response",probability = T)
pred <- attr(pred_tmp,"probabilities")[,"1"]
rock = ROC(test = pred, stat = train$gBRCA_bin)
thresh_train <- names(which.max(rowSums(rock$res[-1, c("sens", "spec")])))
thresh_train <- round(as.numeric(thresh_train),3)
pred_class <- pred
pred_class[which(pred_class < thresh_train)] <- 0
pred_class[which(pred_class > thresh_train)] <- 1
cm_train <- confusionMatrix(as.factor(pred_class), as.factor(train$gBRCA_bin),positive = "1")
cm_train


y <- numeric()
for(i in 1:(index+1)){
  y[i] <- tmp_res[[i]][which.is.max(tmp_res[[i]]$kappa),"kappa"]
}

plot(seq(1:(index+1)),y)

y <- numeric()
for(i in 1:(index+1)){
  y[i] <- tmp_res[[i]][which.is.max(tmp_res[[i]]$aic),"aic"]
}

plot(seq(1:(index+1)),y)

## vado sul testing set

pred_tmp <- predict(logitmod, newdata = test, type = "response", probability = T)
pred <- attr(pred_tmp,"probabilities")[,"1"]
rock = ROC(test = pred, stat = test$gBRCA_bin)
thresh<- names(which.max(rowSums(rock$res[, c("sens", "spec")])))
thresh <- round(as.numeric(thresh),3)
pred_class <- pred
pred_class[which(pred_class < thresh_train)] <- 0
pred_class[which(pred_class > thresh_train)] <- 1
cm_test <- confusionMatrix(as.factor(pred_class), as.factor(test$gBRCA_bin),positive = "1")
cm_test