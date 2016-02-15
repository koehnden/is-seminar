########### Conditional Mutual Information Maximization (CMIM) ###############
#
source("NewFilter/InfoTheo.R") 
#
# proposed by Fleuret (2004)
#
# Code and comments borrow heavly from: 
# http://www.cis.jhu.edu/~yqin/cvrg/Feature_Selection.htm#Index
#
### Basic Idea: minimize the following function: min I(f_i,Y|f_s) 
#               where f_s in an feature that is in the selected subset S and
#               f_i the considered candidate features
#
# K - number of features to be selected
#
cmim <-function(target,features,K){
  ## Assign N the number of features.
  N<-dim(features)[2]
  
  # Set ps (partial score), m (counting indicator), and
  # nu (selection result) to zero.
  ps<- m <- nu <- rep(0,N)
  # Assign ps with mutual information between target and every feature
  for(n in 1:N){
    ps[n] <- mutinformation(features[,n],target,method="emp") 
  }#end for 
  
  ##
  # Assign k from 1 to N, and select the kth feature each time.
  for(k in 1:K){
    # Assign sstar a initial value of 0. It is used in the future to
    # identify the maxium of sorce vector s
    sstar<-0
    # Look forthe kth feature with the maxium conditional mutual information 
    # by assigning n from 1 to N

    for (n in 1:N){
      # If s (partial score) is larger than sstar, and m[n] is smaller
      # than k-1, then execute the following program.
      #
      # This while program will execute only once for each k in 1:N
      # Since the ps is decreacing, as long as ps[n] is smaller than sstar,
      # then we don't need to update ps any more. That is the reason of    
      # (ps[n]>sstar)
      #
      # If we find a ps[n] which is larger than sstar, then we update ps[n]
      # with the all the conditional mutual information of given the
      # features we already picked ( from nu[1] to nu[k-1] ). The reason why
      # (m[n]<k-1) is needed is that the loop need to update all the
      # features we picked. from nu[1] to nu [k-1].

      # Note that for k=1, k-1=0, so this part of program is never executed
      # when k is 1.
      while((ps[n]>sstar)&&(m[n]<k-1)){
        
        # Increace m[n] by 1.
        m[n]<-m[n]+1
        
        # Update the ps with the minimum of current ps[n] and the
        # conditional mutual information given variable nu[m[n]], where
        # m[n] is between 1 and k-1.
        ps[n] <- min(ps[n], condinformation(features[,n], target, 
                                            S=features[,nu[m[n]]], method="emp"))
      }#end while
      
      # If, after updating, the ps[n] is still larger than sstar, then
      # we replace the old feature nu[k] with this new feature n, whose ps is
      # larger than the former. And we set sstar to the current ps[n], so
      # we can compare it with the ps of the rest of features.
      if (ps[n]>sstar) {
        # We set sstar to the current ps[n], so that we can compare it
        # with the ps of the rest of variables later.
        sstar<-ps[n]
        # Record the current variable n to the nu vector, which stores
        # all the feature selection result variables.
        #
        nu[k]<-n
      }#end if
    }#end inner for(n)
  }#end outer for(k)
  
  # List the feature selection result and ps.
  return(list(selected.features=nu,ps=ps))
}

###### Some tests
#CMIM.monk <- cmim(monk.tr[,"target"],monk.tr[,-7],K=4)$selected.features
#colnames(monk[,CMIM.monk]) 

