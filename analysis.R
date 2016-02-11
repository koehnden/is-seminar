# load the necessary packages
source("LoadPackages.R")

# load up the datasets and filter, performance functions
source("IWFSfunction.R")
source("LoadArtificialData.R")
source("LoadRealData.R")
source("performance.R")

# create a list of all datasets we want to test
datasets <- list(corral,corral_100,xor,xor_100,parity,toy,data1,toy.data2,toy.data3,monk,madelon)

# Assumptions: Last variable of the dataset is the target variable

#################### IWFS Algorithm ##############################################
# Select the features
selectedFeaturesIWFS <- fsSelectCustom(datasets,iwfs)
# Train the models and get the performance
performanceIWFS <- fsPerformance(datasets,selectedFeaturesIWFS)


################### CFS Algorithm ################################################
# Select the features
selectedFeaturesCFS <- fsSelectCustom(datasets,cfs)
# Train the models and get the performance
performanceCFS <- fsPerformance(datasets,selectedFeaturesCFS)

################## Relief-F Algorithm ############################################
# Select the features
selectedFeaturesRELIEF <- fsSelectCustom(datasets,relief)
# Train the models and get the performance
performanceRELIEF <- fsPerformance(datasets,selectedFeaturesRELIEF)

################# Basic Caret functions #########################################