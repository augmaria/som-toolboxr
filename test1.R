setwd("./")
source("./som-toolboxr.R")

#
# Basic tests, run on labeled and unlabeled data.
#

# Test 1: Test on Iris data, labeled
d1 <- som_read_data("./data/iris.data", hasLabels=T)
s1<- som_train(d1, hasLabels=T)
# Test 2: Test on Iris data, unlabeled
d2 <- som_read_data("./data/iris.data", hasLabels=F)
s2<- som_train(d2, hasLabels=F)