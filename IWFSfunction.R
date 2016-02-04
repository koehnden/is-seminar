######################### Interaction Weight Feature Selection ########################
#
# Zheng et al. (2015)
# Definition Interactive Features: 
#  A Feature is interactive if it provides more information for the classification 
#  problem in combination with other features than by itself 
#                                   
# Idea to find such features.
# use Symmetical uncertainty plus an interaction weight factor (IWFS)
# 
#
###### Interaction Weight function
# Calculates IW = 1 + I(F_i;F_j;C)/H(F_i) + H(F_i)
# where a H(X) is the entropy function of X and I(X;Y;Z) is the interaction gain 
# of three random variables X, Y and Z
## The function needs the following input
# x - feature i
# y - feature j 
# target - target variable of the classification problem
# Note that all inputs must be discrete. So contiuos variables must be discretized first. 
IW <- function(x,y,target){
  
  ## Compute Interaction Gain
  # I(F_i,F_j,C) = H(F_i) + H(F_j) + H(C) - H(F_i,F_j,C)  
  IG <- interinformation(data.frame(x,y,target), method="emp")
  ## Compute Interaction weigth
  # IW(F_i,F_j) = 1 + I(F_i,F_j,C)/H(F_i) + H(F_j)
  # No braces around H(F_i) + H(F_j) in the paper, but then the IW is not beween 0 and 2
  IW <- 1 + IG / (entropy(x, method="emp") +
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

iwfs <- function(target, features, K=NA, colnames=TRUE,
                 minfo.criteria=FALSE, percent.info=0.75){  
  C <- target
  allFeatures <- features
    
  ######### First Round
  candidateFeatures <- allFeatures
  # Initialize weights to 1 for each feature, w(F_i) in the paper
  weights <- c(rep(1,ncol(candidateFeatures)))
  
  # Calculate Symmetrical Uncertainty using symmetrical.uncertainty() function
  # from the FSelector packages
  df <- data.frame(C,candidateFeatures)
  SU <- symmetrical.uncertainty(C~.,data=df); SUfull <- SU
  
  ### calulate the Relevance measure with all features for the first round
  adjR <- weights * (1 + SU) # weight adjusted Relevance Measure
  # in case no column names are provided
  if(colnames==FALSE){
    rownames(adjR) <- NULL
    choosenFeature <- cutoff.k(adjR,k=1) # choose the bset feature
    idx.new <- as.numeric(choosenFeature) # index of the choosen feature
    } else {
      choosenFeature <- cutoff.k(adjR,k=1) # choose the bset feature
      idx.new <- which(names(allFeatures) %in% choosenFeature) # index of the choosen feature
    }#end if else 
  
  ### exclude selected feature out of the candidate Features
  candidateFeatures <- candidateFeatures[,-idx.new]
  ###### end First Round
  
  ### Round 2 to K
  infoFull <- multiinformation(data.frame(features,target), method ="emp") 
  K <- K     
  k <- 1
  while(k < K){
    ### update weights
    #print(ncol(candidateFeatures))
    update.weights <- c(rep(0,ncol(candidateFeatures))) 
    for(i in 1:ncol(candidateFeatures)){                  
      update.weights[i] <- IW(candidateFeatures[,i],
                              features[,idx.new], C)
    }#end for
    weights <- weights[-idx.new] * update.weights 
    
    ### Calculate Relevance Measure
    if(colnames==FALSE){rownames(SU) <- NULL}
    SU <-  subset(SU,rownames(SU) %in% colnames(candidateFeatures))   
    adjR <- weights * (1 + SU)

    ### choose the the feature with the largest Relevance Measure (adjR)
    choosenNew <- cutoff.k(adjR,k=1)  
    choosenFeature <- append(choosenFeature,choosenNew, after=k)
    idx.new <- which(names(candidateFeatures) %in% choosenFeature)
    
    ### exclude selected feature out of the candidate Features
    candidateFeatures <- candidateFeatures[,-idx.new]
    ## if mutinformation as stopping criteria is defined calculate I(F_subset;C)
    if(minfo.criteria==TRUE){
      infoSubset <- multiinformation(data.frame(features[,choosenFeature],target),
                             method ="emp")
    }#end if  
    if(minfo.criteria==TRUE & infoSubset >= infoFull*percent.info){
      break
    }
    k <- k + 1
  }#end while

  ## return the feature that are choosen
  if(colnames==FALSE){ 
    return(list(selected.features=as.numeric(choosenFeature),
                symmetrical.uncertainty=SUfull))
  }
  return(list(selected.features=choosenFeature,
              symmetrical.uncertainty=SUfull))
}#end iwfs

################# some tests for the iwfs function ################################
# 
# note: Error in rep(0, ncol(candidateFeatures)) : invalid 'times' argument probably
#       means that the maximum number of selected features is too high e.g.
#       K >= # of features in the full set

## test on the corral data
#iwfs.corral <- iwfs(target=corral[,7],features=corral[,-7],
#                  K=4, minfo.criteria=T, colnames=T)
## get the feature subset
iwfs.corral$selected.features 
# note that iwfs works not well with corral and xor because the SU fails (univariate Filter)
## corral: SU=0 for all features except 6
## xor: SU=0 for all feature

## test on monk3 data
#iwfs.monk <- iwfs(target=monk.tr[,"target"], features=monk.tr[,-c(1,8)],
#                    K=5, minfo.criteria=T)

## test on madelon 
# takes about 5min
#iwfs.madelon <- iwfs(target=madelon[,"target"], features=madelon[,-501],
#                    K=54, minfo.criteria=T, colnames=FALSE)
