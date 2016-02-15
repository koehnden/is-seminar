##### Feature Selection algorithm based on inFormation Criteria (FSFC)
#
#
# requires infoTheo and loadPackages to estimate Information theoretic quantities
source("NewFilter/InfoTheo.R") # to estimate Mututal Information and Symmetrical Uncertainty
# Filter proposed by Liu et al. (2014)
# Basic Idea: To measure relevance and redudancy the mutual Information criteria 
#             and coefficient of relevance CR(X,Y) = I(X;Y)/H(X) are used.
#             The algoritm works in a similar way as a algglomerative 
#             cluster for features.
#
#

############## Function for evaluating features in the second round
#
# algglomerative cluster like approach using the evaluation function:
# J(f) = S_b(C,s) + I(f;C)/ |S| + S_w(S) + S(f) where S is the choosen Subset
# s a single element of S, f is a candidate feature,
# S_b(C,s) = sum(I(C;s)), S_w(S) = sum(CR(s;S)), S_w(f) = sum(CR(s;f))
#
FSFC.evaluation <- function(allFeatures,candidateFeatures,
                            choosenFeatures,target,I_fc,I_full){
  
  ### S_b(S,C) = sum of all I(s;C)
  # caculate I(s;C) for all feature s in the subset S
  ## I(s;C) is included in I_full therefore we do not need to calculate it again
  # just elements of all choosenFeatures in I_full and take the sum
  S_b <- sum(I_full[choosenFeatures])
  
  ### S_w(S) = sum of all I(s;S)/H(S)   
  # calculate CR(s,S)=I(S;s)/H(S)
  # I(s;S) = H(s) + H(S) - H(s;S) --> H(S) is fixed so use MI.fast
  entropy.S <- entropy(allFeatures[,choosenFeatures],method="emp")
  CR_sS <- rep(0,length(choosenFeatures))
  for(i in 1:length(choosenFeatures)){
    CR_sS[i] <- MI.fast(x=allFeatures[,choosenFeatures[i]],
                        y=allFeatures[,choosenFeatures],
                        entropy.y=entropy.S)/
      entropy(allFeatures[,choosenFeatures], method="emp")
  }#end for
  
  # take the sum
  S_wS <- sum(CR_sS)
  
  ### Calculate S_w(f) = sum(I(s;f)/H(f)) for all candidate features
  # a length(candidateFeature) x length(choosenFeatures)-Matrix with zeros
  CR_sf <- matrix(rep(0,length(candidateFeatures)*length(choosenFeatures)),
                  nrow=length(candidateFeatures),ncol=length(choosenFeatures))
  # for each candidate Feature not already choosen
  for(i in length(candidateFeatures)){
    # for each choosen Feature in the choosen Subset
    for(j in 1:length(choosenFeatures)){
      CR_sf[i,j] <- mutinformation(candidateFeatures[,i],
                                   allFeatures[,choosenFeatures[j]], 
                                 method="emp")/ entropy(candidateFeatures[,i])  
    }#end inner for(j)
  }#end outer for(i)
  
  ## take the sum for each feature: S_w(f) = sum(CR(f;s))
  # for the second round rowSum does not work because CR_sS is a vector
  if(length(choosenFeatures) < 2){
    S_wf <- sum(CR_sf) # exception for 2nd round
  } else {
    S_wf <- rowSums(CR_sf) # normal case
  }#end if else
  
  ## Calculate the final measure
  J <- (S_b + I_fc)/(length(choosenFeatures) + S_wS + S_wf)
  return(J)
}# FSFC.evaluation

################# Function to perform the actual FSFC ####################
#
# requires the FSFC.evaluation function above
#
# target - a column named target from a data.frame
# features - a dataframe of features to consider for the selection
# K - number of feature to choose. Note this must be smaller than the total
#     number of features in the feature matrix
#
fsfc <- function(target,features,K=ceiling(ncol(features)*0.1)){
  
  ############ First Round ######################################
  # first feature is choosen by the mutual information criteria exclusively
  #
  #### Caculate I(f;C) for all features using MI.fast
  entropy.target <- entropy(target,method="emp")
  I_fc <- c(rep(0,ncol(features)))
  for(i in 1:ncol(features)){
    I_fc[i] <- MI.fast(x=features[,i],y=target,entropy.y=entropy.target)
  }
  ## get the feature with the highest I(f;C)
  # make sure feature names stay 
  names(I_fc) <- colnames(features) 
  # cutoff best feature
  choosenFeatures <- cutoff.k(data.frame(I_fc),k=1) 
  ## get the index of the choosen Feature
  idx.choosen <- which(colnames(features) %in% choosenFeatures) 
  # new candidate feature excluding the feature choosen
  candidateFeatures <- features[,-idx.choosen]
  # save all I(f;C) as I_full to use it later! This avoid a one loop in the 
  # calculation of FSFC.evaluation since we need the I(subset;C) to calculate this
  I_full <- I_fc
  # exclude the choosen feature form the vector of I(f;C)
  I_fc <- I_fc[-idx.choosen]
  
  # set stopping parameters for while loop
  k <- 1; K <- K
  # while k<K do:
  while(k<K){
  ## Calculate J(f) for evaluating features
    J_f <- FSFC.evaluation(features,candidateFeatures,choosenFeatures,target,
                           I_fc,I_full)
    # retain row names for output
    names(J_f) <- colnames(candidateFeatures) 
    # choose best feature according to J_f
    choosenFeatures <- c(choosenFeatures,cutoff.k(data.frame(J_f),k=1))
    # get the index of the choosen feature
    idx.choosen <- which(colnames(candidateFeatures) %in% choosenFeatures)
    # remove choosen feature from the candidate features list
    candidateFeatures <- candidateFeatures[,-idx.choosen]
    # remove the row of I(f,C) for the choosen features
    I_fc <- I_fc[-idx.choosen]
    # add one to k
    k <- k + 1
  }#end while  
  ## return list of choosen features
  return(choosenFeatures)
}#end fsfc

#### some tests
#fsfc(target=monk.tr[,"target"],features=monk.tr[,-c(1,8)],K=3)
#system.time(fsfc(target=corral_100[,"target"],features=corral_100[,-100], K=50))
#fsfc(target=parity[,"target"],features=parity[,-11], K=6)
#fsfc(target=xor_100[,"target"],features=xor_100[,-98], K=6)




