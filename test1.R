setwd("./")
source("./som-toolboxr.R")

#
# Basic tests, run on labeled and unlabeled data.
#

# Test 1: Test on Iris data, labeled
d1 <- som_read_data("./data/iris.data", hasLabels=T)
s1<- som_train(d1, hasLabels=T)
som_plot(s1,d1,toFile=F, hasLabels=T)
# Test 2: Test on Iris data, unlabeled (use label as another column)
d2 <- som_read_data("./data/iris.data", hasLabels=F)
s2<- som_train(d2, hasLabels=F)
som_plot(s2,d2,toFile=F)