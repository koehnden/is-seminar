source("applyFeatureSelection.R")
####### Wrapper functions for algor(ithm from the FSelect package, different from the Custom packages due to the formula interface ##########
fsSelect <- function(datasets=NULL){
  set.seed(800)
  results <- list()
  
  # Loop selects the target variable, selects the features
  for (i in c(1:length(datasets))) {
    d <- datasets[i]
    selectedFeatures <- applyFS(d)
    
    results[i] <- selectedFeatures
  }
  
  names(results) <- colnames(datasets)
  return(results)
}

####### Test the performance of a dataset given a set of selected features #########################
fsPerformance <- function(datasets=NULL,selectedFeatures=NULL){
  set.seed(800)
  
  results <- list()
  
  # Loop selects the target variable, trains the models
  for (i in c(1:length(datasets))) {
    d <- datasets[i]
    targetVariable <- d[,ncol(c)]
    d <- d[,1:ncol(d)-1]

    performance <- modelPerformance(d[,names(d) %in% selectedFeatures[1]],targetVariable)
    results[i] <- performance
    }
  
  names(results) <- colnames(datasets)
  return(results)
}

#### Wrapper for all the models ###############################
modelPerformance <- function(dataset=NULL,targetVariable=NULL,linear=T){
  fitControl <- trainControl(method = "cv",number = 10, allowParallel=T, 
                             summaryFunction = twoClassSummary,classProbs = TRUE)  
  # Gradient Boosting
  xgbFit <- train(targetVariable~., data = dataset,method="gbm",
                  trControl=fitControl,metric="ROC")
  # Random Forest
  rfFit <- train(targetVariable~., data = dataset ,method = "rf",
                 trControl = fitControl, metric="ROC")
  # Neural Net
  nnFit <- train(targetVariable ~ ., data = dataset ,method = "nnet",
                 trControl = fitControl, metric="ROC")
  
  if(linear==F){
    # Non-linear with radial kernel
    svmFit <- train(targetVariable ~ ., data = dataset ,
                    method="svmRadial",trControl = fitControl, metric="ROC")
    
  } else {
    # SVM with linear kernel
    svmFit <- train(targetVariable ~ ., data = dataset ,method = "svmLinear",
                    trControl = fitControl, metric="ROC")  
  }#if else
  return(list(xgbFit,rfFit,nnFit,svmFit))
  #return(list(svmFit))
}
