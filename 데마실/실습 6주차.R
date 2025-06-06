mower.df <- read.csv("dmba-datasets/dmba/RidingMowers.csv", header=T)
head(mower.df)
dim(mower.df)

print(mower.df)

set.seed(42) #여러분들은 시드 넘버를 다르게 줘볼게요
train.index <- sample(row.names(mower.df), 0.6*dim(mower.df)[1])
valid.index <- setdiff(row.names(mower.df), train.index)
train.df <- mower.df[train.index,]
valid.df <- mower.df[valid.index,]

new.df <- data.frame(Income=60, Lot_Size=20)

library(caret)

train.norm.df <- train.df
valid.norm.df <- valid.df
mower.norm.df <- mower.df

norm.values <- caret::preProcess(train.df[, 1:2], method=c("center", "scale"))
norm.values$mean; norm.values$std

print(str(norm.values))

#표준화화
train.norm.df[, 1:2] <- predict(norm.values, train.df[, 1:2])
valid.norm.df[, 1:2] <- predict(norm.values, valid.df[, 1:2]) #학습데이터셋의 평균 표준편차로 표준화!
mower.norm.df[, 1:2] <- predict(norm.values, mower.df[, 1:2])
new.norm.df <- predict(norm.values, new.df)

print(new.norm.df)

plot(Lot_Size ~ Income, data=train.norm.df, pch=ifelse(train.df$Ownership=="Owner", 1, 3))
text(train.norm.df$Income, train.norm.df$Lot_Size, rownames(train.df), pos=4)
text(new.norm.df[1], new.norm.df[2], "X")
legend("topright", c("owner", "non-owner", "newhousehold"), pch = c(1, 3, 4),cex=0.5)

library(FNN)
nn <- FNN::knn(train = train.norm.df[, 1:2], test = new.norm.df, cl = train.norm.df[, 3], k = 3) #nearest neighbor
print(nn)

row.names(train.df)[attr(nn, "nn.index")]

print(train.norm.df)

#적절한 k의 선택

accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))

for(i in 1:14) {
  knn.pred <- FNN::knn(train=train.norm.df[, 1:2], test=valid.norm.df[, 1:2], cl = train.norm.df[, 3], k = i)
  accuracy.df[i, 2] <- mean(knn.pred==valid.norm.df[,3])
}
accuracy.df



