# load the necessary packages
source("LoadPackages.R")

# load up the datasets and filter, performance functions
source("applyFeatureSelection.R")
#source("LoadArtificialData.R")
#source("LoadRealData.R")
source("performance.R")

#################### Analysis ##############################################
# AUC Performance of each subset using SVM, NN, RF and GBM
auc.subset <- function(data,selectedFeatures, level = c(0,1), linear=T){
  targetVariable <- factor(data$target,levels=level,labels=c("yes","no"))
  for (i in 1:length(selectedFeatures)){
    df <- data[,names(data) %in% unlist(unlist(selectedFeatures[i]))]
    df <- data.frame(df)
    names(df) <- unlist(unlist(selectedFeatures[i]))
    performance <- modelPerformance(df,targetVariable,linear=linear)
    assign(paste("performance",i,sep=""),performance)
  }
  return(list(auc.relief=performance1,auc.consistency=performance2,auc.cfs=performance3,
         auc.mRMR=performance4,auc.CMIM=performance5,auc.JMIM=performance6,
         auc.IWFS=performance7,auc.FCFS=performance8,auc.ALL=performance9)) 
}

set.seed(123)
corral.result <- auc.subset(data=corral,selectedFeatures=corral.fs)
corral_100.result <- auc.subset(data=corral_100,selectedFeatures=corral_100.fs)

xor.result <- auc.subset(data=xor_100,selectedFeatures=xor.fs,linear=F)
# result for n=500: only Relief and IWFS identify both features
# result for n=300: only Relief finds the both relevant features

monk_noise.result <- auc.subset(data=monk.tr,selectedFeatures=monk_noise.fs)
monk_test.result <- auc.subset(data=monk.test,selectedFeatures=monk_test.fs)
parity.result <- auc.subset(data=parity,selectedFeatures=parity.fs,linear=F)

# contiunos features with target={-1,1}
toy1.result <- auc.subset(data=toy.data1,selectedFeatures=toy1.fs,level=c(-1,1))
toy2.result <- auc.subset(data=toy.data2,selectedFeatures=toy2.fs,level=c(-1,1))
toy3.result <- auc.subset(data=toy.data3,selectedFeatures=toy3.fs,level=c(-1,1))

nonlinear.result <- auc.subset(data=toy1.nonlinear,
                              selectedFeatures=nonlinear.fs,level=c(-1,1),linear=F)



