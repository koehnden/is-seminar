######################### Interaction Weight Feature Selection ########################
#
## requires InfoTheo and loadPackages 
source("NewFilter/InfoTheo.R") # to estimate Mututal Information and Symmetrical Uncertainty
#
#### Filter proposed by Zheng et al. (2015) 
#    and almost identical from Sun et al.(2013)
# Definition Interactive Features: 
#  A Feature is interactive if it provides more information for the classification 
#  problem in combination with other features than by itself 
#                                   
# Idea of the Filter is to find such features.
# use Symmetical uncertainty plus an interaction weight factor (IWFS)
# 
#
###### Interaction Weight function
# Calculates IW = 1 + I(F_i;F_j;C)/H(F_i) + H(F_j)
# where a H(X) is the entropy function of X and I(X;Y;Z) is the interaction gain 
# of three random variables X, Y and Z
## The function needs the following input
# x - feature i
# y - feature j 
# target - target variable of the classification problem
# Note that all inputs must be discrete. So contiuos variables must be discretized first. 
IW <- function(x,y,target){
  ## Computation of the Interaction Gain: 
  # I(F_i;F_j;C) = I(F_i,F_j;C) - I(F_i;C) - I(F_j;C)
  # Note that the author of the package infotheo defines the interaction gain 
  # exactly the other way: I(F_i,F_j;C) = I(F_1;target) - I(F_1;target;|F_2) 
  # see my question as crossvalidated: 
  # http://stats.stackexchange.com/questions/194192/computing-the-interaction-gain-is-there-an-error-in-the-infotheo-package-in-r
  # and the paper Bontempi & Meyer (2010). 
  # There we have to take -interinformation()  
  ## Compute Interaction weigth
  # IW(F_i,F_j) = 1 + I(F_i,F_j,C)/H(F_i) + H(F_j)
  # No braces around H(F_i) + H(F_j) in the paper, but then the IW is not beween 0 and 2
  IW <- 1 + (-interinformation(data.frame(x,y,target), method="emp"))/ 
                (entropy(x, method="emp") +
                   entropy(y, method="emp"))
  return(IW)
}
################## Function for IWFS #############################
# 
## The function needs the following inputs
# target - target variable
# feature - features without target variable
# K - maximum number of feature to be choosen 
#     (note: K must be K < # of total features!!!) 
# colnames - a boolean to derminate wether column names are provided or not
# minfo.criteria - a boolean wether to stop the algorithm if 
#                  I(F,C)*percent.info=I(F_Subset;C) is satisfied
# percent.info - percentage of total information gain that determines the stopping 
#                criteria
## output: a vector of the feature names choosen by the algorithm or an index if 
#          column names are not provided

iwfs <- function(target, features, K){  
    
  ######### First Round
  candidateFeatures <- features
  # Initialize weights to 1 for each feature, w(F_i) in the paper
  weights <- c(rep(1,ncol(candidateFeatures)))
  
  
  # calculate the entropy of the target and MI 
  entropy.target <- entropy(target,method="emp")
  #### Caculate I(f;C) for all features using the MI.fast() function (InfoTheo)
  I_fc <- c(rep(0,ncol(features)))
  for(i in 1:ncol(features)){
    I_fc[i] <- MI.fast(x=features[,i],y=target,entropy.y=entropy.target)
  }#end for 
  
  # Calculate the symmetrical uncertainty
  SU <- rep(0,ncol(features))
  for(i in 1:ncol(features)){
    SU[i] <- sym.uncertainty.fast(target=target, feature = features[,i],
                                  MI=I_fc[i],entropy.target)
  }
  # make sure that candidateFeature and SU have the same names
  names(SU) <- colnames(candidateFeatures)
  
  ### calulate the Relevance measure with all features for the first round
  adjR <- weights * (1 + SU) # weight adjusted Relevance Measure  
  choosenFeatures <- cutoff.k(data.frame(adjR),k=1) # choose the best feature
  idx.new <- which(colnames(candidateFeatures) %in% choosenFeatures) # index of the choosen feature       
    
  ### exclude selected feature out of the candidate Features
  candidateFeatures <- candidateFeatures[,-idx.new]
  
  ###### end First Round
  
  ### Round 2 to K
  #infoFull <- multiinformation(data.frame(features,target), method ="emp") 
  K <- K     
  k <- 1
  while(k < K){
    ### update weights
    update.weights <- c(rep(0,ncol(candidateFeatures))) 
    for(i in 1:ncol(candidateFeatures)){                  
      update.weights[i] <- IW(candidateFeatures[,i],
                              features[,idx.new], target)
    }#end for
    weights <- weights[-idx.new] * update.weights 
    
    ### Calculate Relevance Measure
    SU <- SU[-idx.new]  
    adjR <- weights * (1 + SU)
    
    ### choose the the feature with the largest Relevance Measure (adjR)  
    choosenFeatures <- c(choosenFeatures,cutoff.k(data.frame(adjR),k=1))
    idx.new <- which(names(candidateFeatures) %in% choosenFeatures)
    
    ### exclude selected feature out of the candidate Features
    candidateFeatures <- candidateFeatures[,-idx.new]
  
    k <- k + 1
  }#end while

  ## return the feature that are choosen
  return(choosenFeatures)
}#end iwfs

################# some tests for the iwfs function ################################
# 
#system.time(iwfs.corral <- iwfs(target=corral_100[,"target"],
#                                features=corral_100[,-100],K=10))
## get the feature subset
#iwfs.corral 
## test on monk3 data
#iwfs.monk <- iwfs(target=monk.tr[,"target"], features=monk.tr[,-c(1,8)],
#                    K=3)
## test on madelon 
# takes about 5min
#iwfs.madelon <- iwfs(target=madelon[,"target"], features=madelon[,-501],
#                    K=54, minfo.criteria=T, colnames=FALSE)


