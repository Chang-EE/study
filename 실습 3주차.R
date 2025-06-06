housing.df <- read.csv("C:/Users/cent0/OneDrive/바탕 화면/데마실/BostonHousing.csv/BostonHousing.csv", header=T)
head(housing.df,9)

hist(housing.df$CRIM, xlab="CRIM", nclass=20)

heatmap(cor(housing.df))

load("NYPDMotorVehicleCollisions.rda")
ls()
missing <- NYPDMotorVehicleCollisions
head(missing)
missing[missing == ""] <- NA
missing <- 1 * is.na(missing)
heatmap(missing, Rowv=NA, Colv=NA)

