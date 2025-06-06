# 9장 실습

library(rpart) # means "Recursive PARTitioning"
library(rpart.plot)
mower.df <-read.csv("dmba-datasets/dmba/RidingMowers.csv")
# define rpart.control() in rpart() to determine the depth of the tree.
class.tree <- rpart(Ownership ~ ., data = mower.df,
                    control = rpart.control(maxdepth = 1), method = "class") # See page 231
class.tree # If-then 규칙으로 분류결과를 나타냄


# use prp() to plot the tree.
prp(class.tree, type = 1, extra = 4, split.font = 1, varlen = -3)

class.tree <- rpart(Ownership ~ ., data = mower.df, method = "class",
                    cp=-1, minsplit=2, minbucket=1) # See page 232
prp(class.tree, type = 1, extra = 1,varlen = -10)


####################################################################
library(rpart)
library(rpart.plot)
bank.df <- read.csv("dmba-datasets/dmba/UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)] # Drop ID and zip code columns.

# partition
set.seed(42)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# classification tree
default.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class")
plot(default.ct)

prp(default.ct, type = 1, extra=1, under = TRUE, split.font = 1, varlen = -10)

# classification tree
deeper.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class",cp=0,minsplit=1)
plot(deeper.ct)

# argument xval refers to the number of folds to use in rpart's built-in
# cross-validation procedure
# argument cp sets the smallest value for the complexity parameter.
cv.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class",
               cp = 0.00001, minsplit = 5, xval = 5)
plot(cv.ct)
text(cv.ct,cex=0.7)

# use printcp() to print the table.
printcp(cv.ct)

plotcp(cv.ct)

# prune by lower cp
pruned.ct <- prune(cv.ct,
                   cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
pruned.ct <- prune(cv.ct, cp = 0.006872852)
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])

prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)

##############################################################################

bank.df <- read.csv("dmba-datasets/dmba/UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)] # Drop ID and zip code columns.
# partition
set.seed(1)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]
## random forestin
library(randomForest)


rf <- randomForest(Personal.Loan ~ ., data=train.df, ntree=500,
                   mtry=4, nodesize=5, importance=TRUE)


rf <- randomForest(as.factor(Personal.Loan) ~ ., data=train.df, ntree=500, mtry=4, nodesize=5, importance=TRUE)
## variable importance plot
randomForest::varImpPlot(rf, sort=TRUE)


## confusion matrix
rf.pred <- predict(rf, valid.df)
caret::confusionMatrix(rf.pred, as.factor(valid.df$Personal.Loan))


######################################
library(rpart);library(rpart.plot)
toyotacorolla.df<-read.csv("dmba-datasets/dmba/toyotacorolla.csv", header=TRUE)
# grepl()
select <- grepl(pattern = "Age_08_04|KM|Fuel_Type|HP|Automatic
|Doors|Quarterly_Tax|Mgf_Guarantee|
Guarantee_Period|Airco|Automatic_airco|
CD_Player|Powered_Windows|
Sport_Model|Tow_Bar|Price", x = names(toyotacorolla.df))
select

toyota<-toyotacorolla.df[, select]
toyota$Fuel_Type<-as.factor(toyota$Fuel_Type)
set.seed(709)
tr.ind<-sample(dim(toyota)[1], dim(toyota)[1]*0.6)
train.df<-toyota[tr.ind,]; test.df<-toyota[-tr.ind,]


fit<-rpart(Price~.,train.df,method="anova", minbucket=1, maxdepth=30,cp=0.001)
fit

prp(fit, type=1, extra=1, split.font=1, varlen=-10)

fit$variable.importance

tr.yhat=predict(fit,train.df)
tt.yhat=predict(fit,test.df)
tr.rmse<-sqrt(mean((tr.yhat-train.df$Price)^2));tr.rmse

tt.rmse<-sqrt(mean((tt.yhat-test.df$Price)^2));tt.rmse

tr.err<-tr.yhat-train.df$Price
tt.err<-tt.yhat-test.df$Price
boxplot(tr.err,tt.err, ylab="prediction error")

printcp(fit) # 0.1100494 < 0.133732

