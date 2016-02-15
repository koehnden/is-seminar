# load the necessary packages
source("LoadPackages.R")

# load up the datasets and filter, performance functions
source("applyFeatureSelection.R")
source("LoadArtificialData.R")
source("LoadRealData.R")
source("performance.R")

# create a list of all datasets we want to test
datasets <- list(corral,corral_100,xor,xor_100,parity,toy,data1,toy.data2,toy.data3,monk,madelon)

# Assumptions: Last variable of the dataset is the target variable

#################### Analysis ##############################################
resultsSelectedFeatures <- list()
resultsPerformance <- list()

# Loop selects the target variable, selects the features
for (i in c(1:2)) {

  d <- datasets[i]
  selectedFeatures <- applyFS(d)
  resultsSelectedFeatures[i] <- selectedFeatures
  
  # Performance
  for (j in c(1:length(selectedFeatures))) {
    targetVariable <- d[,ncol(c)]
    d <- d[,1:ncol(d)-1]
    
    performance <- modelPerformance(d[,names(d) %in% selectedFeatures[j]],targetVariable)
    resultsSelectedFeatures[j] <- performance
  }
  
}
