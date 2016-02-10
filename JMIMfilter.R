########### Features Selection using Joint Mutual Information Maximization ####
#
# Filter proposed by Bennasar et al. (2015)
# 
# Basic Idea: use the joint mutual information criteria: I(f_i,f_s;C) to 
#             evaluate features. Apply the maximum of the minimum approach to
#             choose a feature in each round: max(min(I(f_i,f_s;C))). 
#             This means that the feature with highest smallest value of 
#             I(f_i,f_s;C) is choosen. 
#
### Function to compute the Joint Mutual Information between the candidate 
#   features, elements in the choosen Subset and the target.
# Mathmatically: I(f_i,f_s;C) = I(f_s;C) + I(f_i;C|f_s)
JMI <- function(allFeatures, candidateFeatures, choosenFeatures, target){
  
  # a length(candidateFeature) x length(choosenFeatures)-Matrix with zeros
  JMI <- matrix(rep(0,length(candidateFeatures)*length(choosenFeatures)),
                nrow=length(candidateFeatures),ncol=length(choosenFeatures))
  for(i in 1:length(candidateFeatures)){
    for(j in 1:length(choosenFeatures)){
      JMI[i,j] <- mutinformation(candidateFeatures[,i], target, method="emp") + 
        condinformation(candidateFeatures[,i], target, 
                        S=allFeatures[,choosenFeatures[j]], method="emp")
    }#end out for(j)
  }#end inner for(i)
  return(JMI)
}#end JMI

jmim <- function(target,features,K=ceiling(ncol(features)*0.1)){
  ############ First Round ######################################
  # first feature is choosen by the mutual information criteria exclusively
  #
  #### Caculate I(f;C) for all features
  MI_fc <- c(rep(0,ncol(features)))
  for(i in 1:ncol(features)){
    MI_fc[i] <- mutinformation(features[,i], target, method="emp")
  }#end for
  
  ### get the feature with the highest I(f;C)
  MI_fc <- data.frame(MI_fc)
  # make sure feature names stay 
  rownames(MI_fc) <- colnames(features) 
  # cutoff best feature
  choosenFeatures <- cutoff.k(MI_fc,k=1) 
  ## get the index of the choosen Feature
  idx.choosen <- which(colnames(features) %in% choosenFeatures) 
  
  # new candidate feature excluding the feature choosen
  candidateFeatures <- features[,-idx.choosen]
  
  # greedy forward search
  K <- K;  k <- 1
  while(k<K){
    jmi <- JMI(features, candidateFeatures, choosenFeatures, target)
    # retain row names for output
    rownames(jmi) <- colnames(candidateFeatures)
    
    ## choose the feature that has the highest minimal joint mutual information
    choosenFeatures <- c(choosenFeatures,
                         cutoff.k(data.frame(apply(jmi,1,min)),k=1))
    # get the index of the choosen feature
    idx.choosen <- which(colnames(candidateFeatures) %in% choosenFeatures)
    # remove choosen feature from the candidate features list
    candidateFeatures <- candidateFeatures[,-idx.choosen]
    k <- k + 1
  }#end while
  return(choosenFeatures) 
}#end jmim

##### some tests #####################################
#jmim(target=monk.tr[,"target"],features=monk.tr[,-c(1,8)],K=4)
#jmim(target=corral_100.tr[,"target"],features=corral_100.tr[,-100],K=5)
