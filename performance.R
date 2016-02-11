###### Wrapper function for custom written algorithms #######################
fsSelectCustom <- function(datasets=NULL,fsAlgo=NULL){
  set.seed(800)
  results <- list()
  
  # Loop selects the target variable, selects the features
  for (i in c(1:length(datasets))) {
    d <- datasets[i]
    targetVariable <- d[,ncol(c)]
    d <- d[,1:ncol(d)-1]
    selectedFeatures <- fsAlgo(targetVariable,d)
    
    results[i] <- selectedFeatures
  }
  
  names(results) <- colnames(datasets)
  return(results)
}

####### Wrapper functions for algorithm from the FSelect package, different from the Custom packages due to the formula interface ##########
fsSelectFSelector <- function(datasets=NULL,fsAlgo=NULL){
  set.seed(800)
  results <- list()
  
  # Loop selects the target variable, selects the features
  for (i in c(1:length(datasets))) {
    d <- datasets[i]
    targetVariable <- d[,ncol(c)]
    d <- d[,1:ncol(d)-1]
    selectedFeatures <- fsAlgo(targetVariable~.,d)
    
    results[i] <- selectedFeatures
  }
  
  names(results) <- colnames(datasets)
  return(results)
}

####### Test the performance of a dataset given a set of selected features #########################
fsPerformance <- function(datasets=NULL,selectedFeatures=NULL){
  set.seed(800)
  fitControl <- trainControl(method = "cv",number = 5, allowParallel=T, summaryFunction = twoClassSummary,classProbs = TRUE)
  
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
modelPerformance <- function(dataset=NULL,targetVariable=NULL){
  # Gradient Boosting
  xgbFit <- train(targetVariable ~ ., data = dataset ,method = "xgbTree",trControl = fitControl,verbose=TRUE, metric="ROC")
  # Random Forest
  rfFit <- train(targetVariable ~ ., data = dataset ,method = "rfTree",trControl = fitControl,verbose=TRUE, metric="ROC")
  # Neural Net
  nnFit <- train(targetVariable ~ ., data = dataset ,method = "nnet",trControl = fitControl,verbose=TRUE, metric="ROC")
  # SVM
  svmFit <- train(targetVariable ~ ., data = dataset ,method = "svmLinear",trControl = fitControl,verbose=TRUE, metric="ROC")
  
  return(list(xgbFit,rfFit,nnFit,svmFit))
}
