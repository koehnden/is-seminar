##################### Artificial Data Sets for IS-Seminar Paper ################
#
# create Artificial Data Sets and loads them
#
############################## Artifical Data Sets ###############################
#
# see Bolon-Canedo et al. (2011)
#
################## Corral ######################################################
#  Corral : correlated attribute
#  see: description: https://www.sgi.com/tech/mlc/db/corral.names
#
# dealing with correlation and redudancy 
#
corral <- read.table("corral.txt", header=F, sep=",")
# corral[,7] is the target
colnames(corral) <- c("f1","f2","f3", "f4", "f5", "f6", "target")
# baseline accurracy
# 1-mean(corral[,7]) # 56.25% like in Bolon-Canedo et al. (2011)

# Feature 6 is redudant
sum(corral[,6]==corral[,7])/128 
# as described feature 6 is to 75% identical with the target 
# but with including f1-f4 feature 6 get redudant

# feature 5 is irrelevant: correlation = 0
cor(corral[,5],corral[,7])

#### create 93 other irrelevant features to obtain Corral-100
# Kim et al. (2010): An MLP-based feature subset selection for HIV-1 protease cleavage site analysis
#
# we use sample() and the irrelevant feature 5 to generate the irrelevant features
# and repeat the random sampling 93 using replicate() 
set.seed(12345)
Sample.Irrelevant <- function(x) sample(x,replace=F)
f_irrelevant <- replicate(93,Sample.Irrelevant(corral[,5]))
# merge to get corral-100
corral_100 <- data.frame(corral,f_irrelevant)

############### XOR-100 ##################################
#
# dealing with nolinearity and interaction (only the subset matters)
#
#### randomly generate binary variables from a bernoulli distribution
set.seed(1010101)
f1 <- rbinom(n=50,size=1,prob=0.5)
f2 <- rbinom(n=50,size=1,prob=0.5)

## creat target using xor operation of the two feature: f1 XOR f2
#  this mean that target is 1 if f1[i]!=f2[i] and 0 otherwise
target <- ifelse(f1==f2,0,1)
xor <- data.frame(target,f1,f2)

## randomly generate 97 irrelevate features
set.seed(1010101)
f_irrelevant <- replicate(97,Sample.Irrelevant(x= rbinom(n=50,size=1,prob=0.5)))
xor_100 <- data.frame(xor,f_irrelevant)

# baseline accurary like in Bolon-Canedo et al. (2011)
1-mean(xor_100[,"target"])
R_xor <- cor(xor_100) # class-correlation would not work!


#################### parity5+5 ###################################################
#
# Cite John et al. (1994)!
#
# see description: https://www.sgi.com/tech/mlc/db/parity5+5.names
# note: bolon-canedo use parity3+3 (problem?)
# again dealing with non-linearity and interaction (only the subset is important)
# 5 relevant, 5 irrelevant features, maybe add 10 irrelevant
# 
# Parity of bits 2,3,4,6,8 (1,5,7,9,10 irrelevant):
# target = 1 if sum(x_relevant) == uneven and target = 0 otherwise 
#

parity <- read.table("Parity5+5.txt", header=F, sep=",")
colnames(parity) <- c("f1_irr", "f2_rel", "f3_rel" , "f4_rel", "f5_irr",
                     "f6_rel", "f7_irr","f8_rel", "f9_irr", "f10_irr", "target")  
### code to check parity
f_relevant <- parity[,c(2,3,4,6,8)]
#row_sum <- apply(f_relevant,1,sum)
## modulo division to find out if a number is even
is.even <- function(x) x %% 2 == 0 
ifelse(is.even(row_sum)==TRUE,0,1)==parity[,11] # ifelse result equal target variable 

#### add 5 irrelevant features
# Bolon-Canedo et al. (2011) modified the data set 3 Relevant, 6 Irrelevant, 3 Redudant
# so we ad 5 Irrelevant Features
# note: if this is to tough for every filter we could reduce redundant feature
#
## permute the rows of the relevant feature randomly holding the sum of each row 
#  fixed so that the psrity of the new 5 feature still predict the target perfectly 
#  use permatswap from the vegan package
permute.rowsum_fixed <- permatswap(as.matrix(f_relevant), times = 100, mtype = "prab", 
                  fixedmar="both")
# create redundant
f_redundant <- permute.rowsum_fixed$perm[[2]]
#row_sum <- apply(f_redundant,1,sum) # to check if it work use the row sums and
                                     # execute the code to above to check parity 
# merge everything
parity <- data.frame(parity,f_redudant)

######################### Toy Data Sets ####################################
#
# dealing with noise (instead of LED-Data)
# additionally we add weakly important features
# contiuos features
#
# used in Weston et al. (2003) to test FS for SVM and 
# in Genauer et al. (2010) to test Random Forrest Variable Importance
# cite both papers!

toy.data <- function(n=200,p=100,balance=0.6,strong.corr=0.7){
  target <- rbinom(n, size=1, prob=balance)
  target[target==0] <- -1
  
  #### create high relevant features eg. strong correlation with the target
  # 70% (strong.corr) percent of the datapoints in the features is correlated 
  # with the target and 30% (1-strong.corr) is noise
  f_strong <- matrix(rep(0,n*3),n,3)
  for(i in 1:3){
    f_strong[,i] <- c(rnorm(n*strong.corr,mean=target*i,sd=1),
                      rnorm(n*(1-strong.corr),mean=0,sd=1))
  }
  
  #### create weak relevant features eg. weakly correlation with target  
  # 30% (1-strong.corr) percent of the datapoints in the features is correlated 
  # with the target and 70% is noise 
  f_weak <- matrix(rep(0,n*3),n,3)
  for(i in 1:3){
    f_weak[,i] <- c(rnorm(n*(1-strong.corr),mean=target*i,sd=1),
                    rnorm(n*strong.corr,mean=0,sd=1))
  }
  
  #### create noise 
  f_noise <- replicate(p-6, rnorm(n,mean=0,sd=1))
  
  ### combining the data
  toy.data <- data.frame(f_strong,f_weak,f_noise,target)
  return(toy.data)
}

### set up different data sets
set.seed(12345)
toy.data1 <- toy.data(n=500,p=200)  # normal structure
toy.data2 <- toy.data(n=500,p=500)  # easy
toy.data3 <- toy.data(n=100,p=500)  # p>>n 

################################ Monk 3 #########################################
#
# see description: https://archive.ics.uci.edu/ml/machine-learning-databases/monks-problems/monks.names
# dealing with noise in the target
# target: (f5 = 3 and f4 = 1) or (f5 /= 4 and f2 /= 3)
# (5% class noise added to the training set)
# note: Bolon-Canedo use only the training data (122 instances), 

### whole data set 432 instances
# we split the data set here, because only the defined training set has noise in the
# target
monk <- read.table("Monk3.txt", header=F, sep=" ")
# column 9 is the id of the instances and column 1 has only NAs
monk <- monk[,-1]       

### training data with noise in the target 128
monk.tr <- read.table("Monk3_train.txt", header=F, sep=" ")
monk.tr <- monk.tr[,-1]  

### get the test set without noise in the target with 310 instances
# %in% to compare vectors with different length
idx.tr <- which(monk[,8] %in% monk.tr[,8]) 
monk.test <- monk[-idx.tr,]

## names columns
# features f2, f3, f5 are relevant, but selecting only f2 and f5 can lead to better
# classification result (see John & Kohavi, 1997)
col.monk <- c("target", "f1_irr", "f2_rel", "f3_rel" , "f4_irr", 
                       "f5_rel", "f6_rel", "id")
colnames(monk.tr) <- colnames(monk.test) <- col.monk

############################ MADELON ###########################################
#
# Aritificial Data set from NIPS Challenge 2003
# source: https://archive.ics.uci.edu/ml/machine-learning-databases/madelon/
#
# note: labels are coded as 1 and -1
madelon.tr <-read.table("madelon_train.txt", header=F, sep=" ")
madelon.tr_labels <-read.table("madelon_train_labels.txt", header=F, sep=" ")
# merge training data and labels
madelon.tr <- data.frame(madelon.tr[,-501], madelon.tr_labels)
# note: last column is full of NA( sep=" ")
colnames(madelon.tr) <- c(1:500,"target")

### get the "validation" set and labels
madelon.val <-read.table("madelon_val.txt", header=F, sep=" ")
madelon.val_labels <-read.table("madelon_val_labels.txt", header=F, sep=" ")
# merge validation set and labels
madelon.val <- data.frame(madelon.val[,-501], madelon.val_labels)
# note: last column is full of NA( sep=" ")
colnames(madelon.val) <- c(1:500,"target")

#### merge the whole data set
madelon <- rbind(madelon.tr,madelon.val)
colnames(madelon) <- c(1:500,"target")

# baseline accurary 
# mean(madelon[,"target"]==1)  # 0.5 

###### remove all unneccessary objects #####################################
rm(f_irrelevant, f_noise, check.parity, f_redundant, f_relevant,madelon.tr,
   madelon.tr_labels, madelon.val, madelon.val_labels, target, train.id,
   f1, f2, i, idx.tr, col.monk,permute.rowsum_fixed, R_xor)



