########################### Load Real World Data Sets for IS-Seminar ##############
#
#
#################### Gisette  #################################################
# 
# source: https://archive.ics.uci.edu/ml/machine-learning-databases/gisette/
#
### get the competition training data set and labels
# note: labels are coded as 1 and -1
gisette.tr <-read.table("gisette_train.txt", header=F, sep=" ")
gisette.tr_labels <-read.table("gisette_train_labels.txt", header=F, sep=" ")
# merge training data and labels
gisette.tr <- data.frame(gisette.tr[,-5001], gisette.tr_labels)
# note: last column is full of NA( sep=" ")
colnames(gisette.tr) <- c(1:5000,"target")

### get the "validation" set and labels
gisette.val <-read.table("gisette_val.txt", header=F, sep=" ")
gisette.val_labels <-read.table("gisette_val_labels.txt", header=F, sep=" ")
# merge validation set and labels
gisette.val <- data.frame(gisette.val[,-5001], gisette.val_labels)
# note: last column is full of NA( sep=" ")
colnames(gisette.val) <- c(1:5000,"target")


