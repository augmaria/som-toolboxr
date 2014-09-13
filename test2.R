setwd("./")
source("./som-toolboxr.R")

#
# Basic tests, run on labeled and unlabeled data.
#

# Test 1: Test on Iris data, labeled
d1 <- som_read_data("./data/letter-recognition.data", hasLabels=T)
s1<- som_train(d1, hasLabels=T)
som_plot(s1,d1,toFile=F, hasLabels=T)
