####### Function for applying all the filter on one data set #################
#
# requires LoadArtificialData, LoadPackages, InfoTheo, CMIMfilter, JMIMfilter, 
# FSFCfilter and IWFSfilter
source("LoadArtificialData.R")
source("NewFilter/CMIMfilter.R")
source("NewFilter/JMIMfilter.R")
source("NewFilter/FSFCfilter.R")
source("NewFilter/IWFSfilter.R")
#### Inputs 
# data - needs a data frame as an input with the target column named as "target"
# discrete.data - a boolean whether the input data is discrete or nor as some method
#                 based on information theory require discrete input. If true the
#                 data is discretize with the equal width method
# cutoff.percent - an value between 0 and 1 to indicate what percentage of the full
#                  feature set should be choosen
#
applyFS <- function(data, discrete.data=T, cutoff.percent=0.1){
  # get the index of the target variable  
  idx.target <- which(colnames(data)=="target")
  
  # get the number of features
  p <- ncol(data[,-idx.target])
  
  ############ Methods that can handle contiuos inputs
  ## relief
  # note that the relief function in FSelector takes to much time therefore we
  # use the implementation in the CORElearn package
  set.seed(12345)
  # calculate relief variable importance
  # returns a warning message but seems to work properly
  rank.relief <- data.frame(suppressWarnings(attrEval(target~., 
                              data = data, estimator = "Relief")))
  # cutoff the cutoff.percent best features
  relief <- cutoff.k.percent(rank.relief,k=cutoff.percent)
  
  ## consistency-based filter
  # consistency() function in the FSelector package quite slow
  consistency <- consistency(target~., data=data) # output feature names 
  
  ## cfs
  cfs <- cfs(target~., data=data) # output feature names
  
  # library(Biocomb) can somehow not be installed
  #select.fast.filter(as.matrix(data), disc.method,threshold,attrs.nominal)
  
  #################### Methods that need discrete inputs #####################
  # discretize if data contains contious variables
  if(discrete.data==FALSE){
    data <- discretize(data, disc="equalwidth", 
                       nbins=dim(unique(data))[1])
  }#end if
  
  ## mRMR
  # returns the index of the choosen features
  # X needs to be a matrix
  mRMR.idx <- filter.mRMR(X=as.matrix(data[,-idx.target]), Y=data[,idx.target],
                          nbreVarX_=ceiling(p*cutoff.percent))
  # return the colnames of the choosen subset 
  mRMR <- colnames(data[,mRMR.idx$filter]) 
  
  ## CMIM
  # returns the index of the choosen features
  CMIM.idx <- cmim(target=data[,"target"],features=data[,-idx.target],
                   K=ceiling(p*cutoff.percent))$selected.features
  # return the colnames of the choosen subset 
  CMIM <- colnames(data[,CMIM.idx])
  
  ## JMIM
  JMIM <- jmim(target=data[,"target"],data[,-idx.target],K=ceiling(p*cutoff.percent))
  
  ## IWFS
  IWFS <- iwfs(target=data[,idx.target], features=data[,-idx.target], 
                K=ceiling(p*cutoff.percent))
  
  ## FSFC 
  FCFS <- fsfc(target=data[,idx.target],features=data[,-idx.target], 
                K=ceiling(p*cutoff.percent))
  
  # return a list with all the filter results
  return(list(Relief=relief,Consistency=consistency,CFS=cfs,mRMR=mRMR, CMIM=CMIM,
              JMIM=JMIM,IWFS=IWFS, FCFS=FCFS))
}#end apply FS 

### apply all filter to the data sets
corral.fs <- applyFS(data=corral_100)
xor.fs <- applyFS(data=xor_100)
monk.fs <- applyFS(data=monk.tr[,-8], cutoff.percent=0.5) # exclude id (-8)
parity.fs <- applyFS(data=parity, cutoff.percent=0.5) 
toy1.fs <- applyFS(data=toy.data1,discrete.data=F)
toy2.fs <- applyFS(data=toy.data2,discrete.data=F)
toy3.fs <- applyFS(data=toy.data3,discrete.data=F)



