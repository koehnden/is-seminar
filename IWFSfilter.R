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
# Calculates IW = 1 + I(F_i;F_j;C)/H(F_i) + H(F_j)
# where a H(X) is the entropy function of X and I(X;Y;Z) is the interaction gain 
# of three random variables X, Y and Z
## The function needs the following input
# x - feature i
# y - feature j 
# target - target variable of the classification problem
# Note that all inputs must be discrete. So contiuos variables must be discretized first. 
IW <- function(x,y,target){
  
  # compute I(F_i;target;|F_j)
  I_cond <- condinformation(X=x, Y=y, S=target, 
                            method="emp")
  # compute I(F_i;target)
  I <- mutinformation(X=x, Y=target, method="emp")
  
  ## Compute Interaction Gain with I(F_i;F_j;C) = I(F_1;target;|F_2) - I(F_1;target)
  IG <- I_cond - I
  # Note that the author of the package infotheo defines the interaction gain exactly
  # the other way: I(F_i,F_j;C) = I(F_1;target) -I(F_1;target;|F_2) 
  # see my question as crossvalidated: 
  # http://stats.stackexchange.com/questions/194192/computing-the-interaction-gain-is-there-an-error-in-the-infotheo-package-in-r
  # and the paper Bontempi & Meyer (2010)
  #IG <- -interinformation(data.frame(x,y,target), method="emp")
  
  ## Compute Interaction weigth
  # IW(F_i,F_j) = 1 + I(F_i,F_j,C)/H(F_i) + H(F_j)
  # No braces around H(F_i) + H(F_j) in the paper, but then the IW is not beween 0 and 2
  IW <- 1 +  IG / (entropy(x, method="emp") +
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

iwfs <- function(target, features, K=NA, colnames=TRUE){  
  C <- target
    
  ######### First Round
  candidateFeatures <- features
  # Initialize weights to 1 for each feature, w(F_i) in the paper
  weights <- c(rep(1,ncol(candidateFeatures)))
  
  # Calculate Symmetrical Uncertainty using symmetrical.uncertainty() function
  # from the FSelector packages
  df <- data.frame(C,candidateFeatures)
  SU <- symmetrical.uncertainty(C~.,data=df); SUfull <- SU
  
  # make sure that candidateFeature have the same names as the row in SU
  # this caused the error "e[[j]] out of bounds" 
  colnames(candidateFeatures) <- rownames(SU)
  
  ### calulate the Relevance measure with all features for the first round
  adjR <- weights * (1 + SU) # weight adjusted Relevance Measure
  # in case no column names are provided
  if(colnames==FALSE){
    #rownames(adjR) <- NULL
    choosenFeature <- cutoff.k(adjR,k=1) # choose the bset feature
    idx.new <- as.numeric(choosenFeature) # index of the choosen feature
    } else {   
      choosenFeature <- cutoff.k(adjR,k=1) # choose the best feature
      idx.new <- which(names(candidateFeatures) %in% choosenFeature) # index of the choosen feature       
    }#end if else 
  
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

## test on the xor
iwfs.xor <- iwfs(target=xor_100[,"target"],features=xor_100[,-98],
                  K=10, colnames=T)
## get the feature subset
iwfs.xor$selected.features
#iwfs.xor$symmetrical.uncertainty
# SU=0 for all features --> IWFS looks for the Features that interact the most
# with the first column (that is f1) than chooses f2 because it interacts the most
# so somehow this is random. On the other hand, in reality there will probably
# not a situation where SU=0 for all features as in the xor problem

## test on monk3 data
#iwfs.monk <- iwfs(target=monk.tr[,"target"], features=monk.tr[,-c(1,8)],
#                    K=3)
#iwfs.monk$selected.features

## test on madelon 
# takes about 5min
#iwfs.madelon <- iwfs(target=madelon[,"target"], features=madelon[,-501],
#                    K=54, minfo.criteria=T, colnames=FALSE)


