cereals.df <- read.csv("dmba-datasets/dmba/Cereals.csv", header=T)
pcs <- prcomp(na.omit(cereals.df[,-c(1:3)]))
summary(pcs)
pcs$rot[,1:5]
