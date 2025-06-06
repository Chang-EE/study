housing.df <- read.csv("dmba-datasets/dmba/BostonHousing.csv", header=TRUE)

housing.df

set.seed(42)

train.index <- sample(row.names(housing.df), 0.6*dim(housing.df)[1])
valid.index <- setdiff(row.names(housing.df), train.index)

train.df <- housing.df[train.index, ]
valid.df <- housing.df[valid.index, ]
new.df <- data.frame(CRIM=0.2, ZN=0, INDUS=7, CHAS=0, NOX=0.538, RM=6, AGE=62, DIS=4.7, RAD=4, TAX=307, PTRATIO=21, LSTAT=10)

library(caret)

train.norm.df <- train.df
valid.norm.df <- valid.df
housing.norm.df <- housing.df

#30 이상인지 이하인지 범주형으로 변환
train.norm.df$MEDV.class <- ifelse(train.norm.df$MEDV >= 30, "High", "Low")
valid.norm.df$MEDV.class <- ifelse(valid.norm.df$MEDV >= 30, "High", "Low")
train.norm.df$MEDV.class <- factor(train.norm.df$MEDV.class)
valid.norm.df$MEDV.class <- factor(valid.norm.df$MEDV.class)

norm.values <- caret::preProcess(train.df[,1:12], method=c("center", "scale"))

train.norm.df[,1:12] <- predict(norm.values, train.df)
valid.norm.df[,1:12] <- predict(norm.values, valid.df)
housing.norm.df[,1:12] <- predict(norm.values, housing.df)
new.norm.df <- predict(norm.values, new.df)

library(FNN)
accuracy.df <- data.frame(k=seq(1,5,1), accuracy=rep(0,5))

for(i in 1:5){
  knn.pred <- FNN::knn(train.norm.df[,1:12], test=valid.norm.df[,1:12], cl=train.norm.df$MEDV.class, k=i)
  accuracy.df[i,2] <- mean(knn.pred==valid.norm.df$MEDV.class)
}

accuracy.df

knn.pred.new <- knn(train.norm.df[, 1:12], new.norm.df,  cl=train.norm.df$MEDV.class, k=5)
knn.pred.new[3]
