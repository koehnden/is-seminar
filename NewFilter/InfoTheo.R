#### Information Theoretic Functions as Alternatives for the infotheo package #########
#
## requires LoadPackages 
source("loadPackages.R") # to estimate Entropy
# The infotheo package is quite slow and a lot of the implemented filters rely 
# heavivly on estimation of entropies and information criteria. Therefore we
# test these function against self implemented function
#
# note that all entropies H() are estimated in nats (and not in bits)
#
###### Functions to calculate the mutual information of two random variables x and y
## Mathematical: I(X;Y) = H(X) + H(Y) - H(X;Y)
# x - a random variable
# y - another random variable
MI <- function(x,y){
  # Based on formula
  entropy(x,method="emp") + entropy(y,method="emp") - 
    entropy(data.frame(x,y),method="emp")
}

### Faster version for a for-loop if one argument is fixed
# entropy.y - estimated entropy of the random variable y, H(y) 
MI.fast <- function(x,y,entropy.y){
  # Based on formula
  entropy(x,method="emp") + entropy.y - 
    entropy(data.frame(x,y),method="emp")
} 
####### Performance Tests 
#microbenchmark(mutinformation(x1,x2,method ="emp"))
#microbenchmark(MI(x1,target)) # a little faster
# for use in a loop when the second argument is fixed
#microbenchmark(MI.fast(x1,target,entropy.target)) 

############# Function to estimate the joint mutual information ####################
# I({x_1,x_2};y) = H(x_1,x_2) + H(y) - H(x_1;x_2;y)
JMI <-function(x1,x2,y){
  entropy(cbind(x1,x2),method="emp") + entropy(y, method="emp") - 
    entropy(cbind(x1,x2,y),method="emp")
}
### a faster version for for-loops in case the input target is fixed
# entropy.target - estimated entropy of the target 
JMI.fast <- function(x1,x2,target,entropy.target){
  entropy(cbind(x1,x2),method="emp") + entropy.target - 
    entropy(cbind(x1,x2,target),method="emp")
}
#### some performance tests
#microbenchmark(mutinformation(cbind(x1,x2),y, method ="emp"))
#microbenchmark(JMI(x1,x2,y))
#microbenchmark(JMI.fast(x1,x2,y,entropy(y))) # a little faster

#### Function for symmetrical uncertainty 
# the symmetrical.uncertainty function of the FSelector package is estimated in bits
# while all other inputs are estimate in nats. Therefore we use a selfwritten version.
## SU(F_i;C) = 2* I(F_i;C)/H(F_i) + H(C)
sym.uncertainty <- function(target,feature){
  2 * mutinformation(feature, target, method="emp")/
    (entropy(feature, method="emp") +
    entropy(target, method="emp"))
}

### a faster version for for-loops when the targets is fixed
# additional input I(f,C) and H(C) which are fixed and can be calculated before
sym.uncertainty.fast <-  function(target,feature,MI,entropy.target) {
  2 * MI/ (entropy(feature, method="emp") + entropy.target)
}
### Some tests
#mutinformation(corral_100[,99],corral_100[,"target"])        
#MI(x=corral_100[,"target"],y=corral_100[,99])
#microbenchmark(sym.uncertainty(corral_100[,"target"],corral_100[,99]))

#entropy.target <- entropy(corral_100[,"target"],method="emp")
#I_fc <- MI.fast(x=corral_100[,99],y=corral_100[,"target"],entropy.y=entropy.target) 
#microbenchmark(sym.uncertainty.fast(corral_100[,"target"],corral_100[,99],
#                     I_fc,entropy.target))





