########### Features Selection using Joint Mutual Information Maximization ####
#
## requires InfoTheo and Load Packages to estimate Information Theoretic quantities
source("NewFilter/InfoTheo.R") 
#
# Filter proposed by Bennasar et al. (2015)
# 
# Basic Idea: use the joint mutual information criteria: I(f_i,f_s;C) to 
#             evaluate features. Apply the maximum of the minimum approach to
#             choose a feature in each round: max(min(I(f_i,f_s;C))). 
#             This means that the feature with highest smallest value of 
#             I(f_i,f_s;C) is choosen. 
#
jmim <- function(target,features,K=ceiling(ncol(features)*0.1)){
  ############ First Round ######################################
  # first feature is choosen by the mutual information criteria exclusively
  #
  # calculate the entropy of the target for the JMI input needed as an input for
  # MI.fast and JMI.fast
  entropy.target <- entropy(target,method="emp")
  #### Caculate I(f;C) for all features using the MI() function (InfoTheo)
  I_fc <- c(rep(0,ncol(features)))
  for(i in 1:ncol(features)){
    I_fc[i] <- MI.fast(features[,i], target, entropy.target)
  }#end for
  
  ### get the feature with the highest I(f;C)
  # make sure feature names stay 
  names(I_fc) <- colnames(features) 
  # cutoff best feature
  choosenFeatures <- cutoff.k(data.frame(I_fc),k=1) 
  ## get the index of the choosen Feature
  idx.choosen <- which(colnames(features) %in% choosenFeatures) 
  
  # new candidate feature excluding the feature choosen
  candidateFeatures <- features[,-idx.choosen]
  
  # greedy forward search
  K <- K;  k <- 1
  while(k<K){
    # a length(candidateFeature) x length(choosenFeatures)-Matrix with zeros
    jmi <- matrix(rep(0,length(candidateFeatures)*length(choosenFeatures)),
                  nrow=length(candidateFeatures),ncol=length(choosenFeatures))
    
    for(i in 1:length(candidateFeatures)){
      for(j in 1:length(choosenFeatures)){
        # compute the joint mutual information criteria 
        # I(f_i,f_s;C) = I(f_i;C|f_s) + I(f_s;C)
        # this is the slow part of the the function!!
        jmi[i,j] <- JMI.fast(candidateFeatures[,i],
                                             features[,choosenFeatures[j]],
                                 target,entropy.target)
      }#end out for(j)
    }#end inner for(i)
    
    # retain row names for output
    rownames(jmi) <- colnames(candidateFeatures)
    
    ## choose the feature that has the highest minimal joint mutual information
    choosenFeatures <- c(choosenFeatures,
                         cutoff.k(data.frame(apply(jmi,1,min)),k=1))
    # get the index of the choosen feature
    idx.new <- which(colnames(candidateFeatures) %in% choosenFeatures)
    idx.choosen <- which(colnames(features) %in% choosenFeatures) 
    # remove choosen feature from the candidate features list
    candidateFeatures <- candidateFeatures[,-idx.new]
    k <- k + 1
  }#end while
  return(choosenFeatures) 
}#end jmim

### Not really faster using vectorization of the Joint mutual information
jmim.fast <- function(target,features,K=ceiling(ncol(features)*0.1)){
  ############ First Round ######################################
  ### first feature is choosen by the mutual information criteria exclusively
  # calculate the entropy of the target for the JMI input needed as an input for
  # MI.fast and JMI.fast
  entropy.target <- entropy(target,method="emp")
  #### Caculate I(f;C) for all features using MI.fast
  I_fc <- c(rep(0,ncol(features)))
  for(i in 1:ncol(features)){
    I_fc[i] <- MI.fast(features[,i], target,entropy.target)
  }#end for
  
  ### get the feature with the highest I(f;C)
  # make sure feature names stay 
  names(I_fc) <- colnames(features) 
  # cutoff best feature
  choosenFeatures <- cutoff.k(data.frame(I_fc),k=1) 
  ## get the index of the choosen Feature
  idx.choosen <- which(colnames(features) %in% choosenFeatures) 
  
  # new candidate feature excluding the feature choosen
  candidateFeatures <- features[,-idx.choosen]
  
  
  # greedy forward search
  K <- K;  k <- 1
  while(k<K){
    grid <-expand.grid(1:length(candidateFeatures), 
                       which(colnames(features) %in% choosenFeatures))
    
    vectorized <- apply(grid, 1, function(i)  
                         JMI.fast(candidateFeatures[,i[1]], 
                                        features[,i[2]], target,entropy.target))
    jmi.fast <- matrix(vectorized, nrow = ncol(candidateFeatures))
  
    # retain row names for output
    rownames(jmi.fast) <- colnames(candidateFeatures)
    
    ## choose the feature that has the highest minimal joint mutual information
    choosenFeatures <- c(choosenFeatures,
                         cutoff.k(data.frame(apply(jmi.fast,1,min)),k=1))
    # get the index of the choosen feature
    idx.choosen <- which(colnames(candidateFeatures) %in% choosenFeatures)
    # remove choosen feature from the candidate features list
    candidateFeatures <- candidateFeatures[,-idx.choosen]
    k <- k + 1
  }#end while
  return(choosenFeatures) 
}#end jmim.fast


##### some tests #####################################
#jmim.fast(corral_100[,"target"],corral_100[,-100],K=5)
#jmim(corral_100[,"target"],corral_100[,-100],K=5)

#system.time(jmim(xor_100[,"target"],xor_100[,-98],K=50))#slow
#system.time(jmim.fast(xor_100[,"target"],xor_100[,-98],K=50))
#### jmim.fast seems to be not :(

