#######################################################################
# 중고차의 가격 예측 (p173) 
#
# Price: 판매가격(EURO)
# Age: 2004년 8월 당시 자동차 사용기간(months) 
# KM: 누적 주행거리 
# FuelType: 연료 유형(휘발유, 경유, 천연가스)
# HP: 마력
# MetColor: 금속색상 (Yes=1, No=0)
# Automatic: 자동변속 (Yes=1, No=0)
# CC: 실린더 부피
# Doors: 자동차 문의 개수
# QuartTax : 분기별 도로 사용세(EURO)
# Weight: 무게(kg)
#
#######################################################################

# 표 6.3 : 데이터 분할 및 모형 적합

car.df <- read.csv("dmba-datasets/dmba/ToyotaCorolla.csv")
# use first 1000 rows of data
car.df <- car.df[1:1000, ]
# select variables for regression
selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)
# partition data
set.seed(42) # set seed for reproducing the partition
train.index <- sample(c(1:1000), 600)
train.df <- car.df[train.index, selected.var]  #학습데이터
head(train.df)

valid.df <- car.df[-train.index, selected.var] # 검증데이터
# use lm() to run a linear regression of Price on all 11 predictors in the
# training set.
# use . after ~ to include all the remaining columns in train.df as predictors.

car.lm <- lm(Price ~ ., data = train.df)
# use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999, digits=3) #exponential expression이 싫어서 이렇게 하는 거래요요
summary(car.lm)

#가변수로 생성된 범주 애들은 기분 변수와의 비교임. 예) 천연가스에 비해 디젤 쓰는 애들이 더 얼마나 더 비싼지 보여줌. 유의하지 않은 애를 제하는게 효율적이라고 하네요 보통.

# 표 6.4: 예측성능 측정

car.lm.pred <- predict(car.lm, valid.df) # hat y_new 
some.residuals <- valid.df$Price[1:20] - car.lm.pred[1:20] # 예측오차(predicted error)
data.frame("Predicted" = car.lm.pred[1:20], "Actual" = valid.df$Price[1:20],
           "Residual"= some.residuals)

AE=sum(some.residuals)
RMSE=sqrt(mean((some.residuals)^2))  # 다중선형회귀모형으로부터 RMSE 성능지표 계산
MAE=mean(abs(some.residuals))
MPE=mean((some.residuals)/valid.df$Price[1:20])
MAPE=mean(abs(some.residuals)/valid.df$Price[1:20])

AE;RMSE;MAE;MAPE


# 정규성 검토 

car.lm.pred <- predict(car.lm, valid.df)
all.residuals <- valid.df$Price - car.lm.pred
length(all.residuals[which(all.residuals > -1406 & all.residuals < 1406)])/400
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")


# 가능한 모든 회귀모형 탐색법 

# use regsubsets() in package leaps to run an exhaustive search.
# unlike with lm, categorical predictors must be turned into dummies manually.
install.packages("leaps")
library(leaps)
# create dummies for fuel type 얘는 가변수를 자동으로 생성해주지 않음
Fuel_Type <- as.data.frame(model.matrix(~ 0 + Fuel_Type, data=train.df))
# replace Fuel_Type column with 2 dummies
train.df <- cbind(train.df[,-4], Fuel_Type[,2:3]) 
head(train.df)

search <- leaps::regsubsets(Price ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2],method = "exhaustive")
# nbest는 변수의 갯수별로 몇 개의 모형을 제시할지 제어함 
sum <- summary(search)
names(sum)

# show models
sum$which
# show metrics
sum$rsq
sum$adjr2
sum$cp

result=with(sum,round(cbind(which,rsq,adjr2,cp,bic),3)) # 모형성능평가지표에 따른 결과 
result
#다수결로 선택

which.max(sum$rsq)
which.max(sum$adjr2)
which.min(sum$cp)
which.min(sum$bic)

plot(search,scale='r2')     # 변수선택의 결과를 "R^2"측면에서  그림으로 나타냄
plot(search,scale='adjr2')  # 변수선택의 결과를 "adj_R^2"측면에서  그림으로 나타냄
plot(search,scale='Cp')     # 변수선택의 결과를 "Cp"측면에서  그림으로 나타냄
plot(search,scale='bic')    # 변수선택의 결과를 "BIC"측면에서  그림으로 나타냄; 디폴트 세팅임  

# regsubsets()결과로부터 각 모형에 대한 적합결과를 확인하기 위해서는 "coef()"함수를 사용
k=which.min(sum$bic) # BIC기준으로 변수선택을 할 경우 가장 작은 값에 해당하는 모형을 선택함 
coef(search, k) # 선형회귀모형의 적합결과를 알려줌

install.packages("glmnet")
library(glmnet)

# 2. design matrix 생성

x.train <- model.matrix(Price ~ ., data = train.df)[, -1]  # (Intercept 제거)
y.train <- train.df$Price

x.valid <- model.matrix(Price ~ ., data = valid.df)[, -1]
y.valid <- valid.df$Price

# model.matrix는 가변수 싹 다 만들기 때문에 돌아는 간대요. 그래도 일단 다 맞추고 분석해야 한대요 가변수 만들때

# 3. LASSO 모델 적합 (alpha = 1 :LASSO)
fit.lasso <- glmnet(x.train, y.train, alpha = 1) #여태껏 스칼라로 다루었는데, 벡터를 쓴다는 점.
# 미리 가변수를 생성한 후에 넘겨줘야 한대요

names(fit.lasso)
fit.lasso$lambda
log(fit.lasso$lambda)

# 4. λ 값에 따른 계수 확인
plot(fit.lasso, xvar = "lambda", label = TRUE)

# 5. λ 값과 log(λ) 확인
fit.lasso$lambda  # lambda 값 목록
log(fit.lasso$lambda) 

coef(fit.lasso, s=154.17) #s는 lambda 값임
coef(fit.lasso, s=140.47) #s는 lambda 값임
