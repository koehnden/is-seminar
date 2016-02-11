################### Split the Data Sets #####################
#
# We use a ratio of 60% training and 40% test
splitData <- function(data, ratio=0.6){
  
  # to get the same split every time
  set.seed(12345) 
  # get the index of the training set
  idx.tr <- createDataPartition(data$target, p=ratio, list=FALSE) 
  
  # partition in training and test set
  trainData <- data[ idx.tr,] 
  testData  <- data[-idx.tr,]
  return(list(trainData=trainData, testData=testData))
}

# Corral 
corral.split <- splitData(data=corral) 
corral.tr <- corral.split$trainData
corral.test <- corral.split$testData

# Corral-100
corral_100.split <- splitData(data=corral_100) 
corral_100.tr <- corral_100.split$trainData
corral_100.test <- corral_100.split$testData

# XOR-100
xor_100.split <- splitData(data=xor_100) 
xor_100.tr <- xor_100.split$trainData
xor_100.test <- xor_100.split$testData

# parity 5+5
parity.split <- splitData(data=parity) 
parity.tr <- parity.split$trainData
parity.test <- parity.split$testData

# Toy Data Sets  
toy1.split <- splitData(data=toy.data1)
toy1.tr <- toy1.split$trainData
toy1.test <- toy1.split$testData

# Toy Data Sets  
toy2.split <- splitData(data=toy.data2)
toy2.tr <- toy2.split$trainData
toy2.test <- toy2.split$testData

# Toy Data Sets  
toy3.split <- splitData(data=toy.data3)
toy3.tr <- toy3.split$trainData
toy3.test <- toy3.split$testData

## Monk already splitted, we do not change the split because there is only noise
#   in the training set






