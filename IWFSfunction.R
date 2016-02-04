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
## The function needs the following inputs
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
# K - number of feature to be choosen 
# colnames - a boolean to derminate wether column names are provided or not
# minfo.criteria - a boolean wether to stop the algorithm if I(F,C)=I(F_Subset;C)
#                  is satisfied
## output: a vector of the feature names choosen by the algorithm or an index if 
#          column names are not provided

## TODO minfo.criteria=T does not work (I(F,C)=I(F_Subset;C) rule)  
iwfs <- function(target, features, K=NA, colnames=TRUE,
                 minfo.criteria=FALSE){
  
  C <- target
  allFeatures <- features
    
  ######### First Round
  candidateFeatures <- allFeatures
  # Initialize weights to 1 for each feature, w(F_i) in the paper
  weights <- c(rep(1,ncol(candidateFeatures)))
  
  # Calculate Symmetrical Uncertainty using symmetrical.uncertainty() function
  # from the FSelector packages
  df <- data.frame(C,candidateFeatures)
  SU <- symmetrical.uncertainty(C~.,data=df)
  
  ### calulate the Relevance measure with all features for the first round
  adjR <- weights * (1 + SU) # weight adjusted Relevance Measure
  if(colnames==FALSE){rownames(adjR) <- NULL} # in case no column names are provided
  choosenFeature <- cutoff.k(adjR,k=1) # choose the bset feature
  idx.new <- which(names(allFeatures) %in% choosenFeature) # index of the choosen feature
  
  ### exclude selected feature out of the candidate Features
  candidateFeatures <- candidateFeatures[,-idx.new]
  ###### end First Round
  
  ### Round 2 to K
  if(minfo.criteria==T){
    infoFull <- multiinformation(data.frame(features,target), method ="emp") 
  }
  K <- K    
  k <- 1
  while(k < K){
    ### update weights
    update.weights <- c(rep(0,length(candidateFeatures))) 
    for(i in 1:ncol(candidateFeatures)){                  
      update.weights[i] <- IW(candidateFeatures[,i],
                              features[,idx.new], C)
    }#end for
    weights <- weights[-idx.new] * update.weights 
    
    ### Calculate Relevance Measure
    if(colnames==FALSE){rownames(SU) <- NULL}
    SU <-  subset(SU,rownames(SU) %in% colnames(candidateFeatures))### ERROR HERE!!!   
    adjR <- weights * (1 + SU)
    print(length(SU$attr_importance))

    ### choose the the feature with the largest Relevance Measur
    choosenNew <- cutoff.k(adjR,k=1) #ERROR probably happens here
    choosenFeature <- append(choosenFeature,choosenNew, after=k)
    idx.new <- which(names(candidateFeatures) %in% choosenFeature)
    
    ### exclude selected feature out of the candidate Features
    candidateFeatures <- candidateFeatures[,-idx.new]
    ## if mutinformation as stopping criteria is defined calculate I(F_subset;C)
    if(minfo.criteria==TRUE){
      infoSubset <- multiinformation(data.frame(features[,choosenFeature],target),
                             method ="emp")
    }#end if  
    if(minfo.criteria==TRUE & infoSubset >= infoFull*0.75){
      break
    }
    k <- k + 1
  }#end while

  ## return the feature that are choosen
  if(colnames==FALSE){ return(as.numeric(choosenFeature))}
  return(choosenFeature)
}#end iwfs


iwfs.corral <- iwfs(target=corral_100[,7],features=corral_100[,-7],
                  K=98, minfo.criteria=T, colnames=FALSE)

iwfs.monk <- iwfs(target=monk[,"target"], features=monk[,-c(1,8)],
                    K=14, minfo.criteria=T)

iwfs.madelon <- iwfs(target=madelon[,"target"], features=madelon[,-501],
                    K=5, minfo.criteria=T, colnames=FALSE)
