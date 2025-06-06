install.packages("leaps")
library(leaps)


# 원본 데이터 불러오기
car.df <- read.csv("dmba-datasets/dmba/ToyotaCorolla.csv", header = TRUE)
car.df <- car.df[1:1000, ]

# 데이터 분할
set.seed(42)
train.index <- sample(1:1000, 600)
train.df <- car.df[train.index, ]
valid.df <- car.df[-train.index, ]

# 불필요한 범주형 변수 제거
remove_cols <- c("Id", "Model", "Color")
train.df <- train.df[, !(names(train.df) %in% remove_cols)]
valid.df <- valid.df[, !(names(valid.df) %in% remove_cols)]

# Fuel_Type 더미 변수 (기준 범주 제외: 마지막 열 제거)
fuel_train <- as.data.frame(model.matrix(~ 0 + Fuel_Type, data = train.df)[, 1:2])
fuel_valid <- as.data.frame(model.matrix(~ 0 + Fuel_Type, data = valid.df)[, 1:2])

# Fuel_Type 열 제거 후 더미만 붙이기
train.df <- cbind(train.df[, !(names(train.df) %in% "Fuel_Type")], fuel_train)
valid.df <- cbind(valid.df[, !(names(valid.df) %in% "Fuel_Type")], fuel_valid)

# 변수 개수 확인
cat("최종 변수 개수:", ncol(train.df), "\n")

#method = "exhaustive"
search_ex <- regsubsets(Price ~ ., data = train.df, nbest = 1, nvmax = 10, method = "exhaustive",really.big = TRUE)
sum_ex <- summary(search_ex)
best_size_ex <- which.min(sum_ex$cp)
best_vars_ex <- names(coef(search_ex, best_size_ex))[-1]

form_ex <- as.formula(paste("Price ~", paste(best_vars_ex, collapse = "+")))
model_ex <- lm(form_ex, data = train.df)
model_ex_step <- step(model_ex, direction = "both", trace = 0)

pred_ex <- predict(model_ex_step, newdata = valid.df)
mse_ex <- mean((valid.df$Price - pred_ex)^2)
print(paste("Exhaustive MSE:", mse_ex))


#method = "forward"
search_fw <- regsubsets(Price ~ ., data = train.df, nbest = 1, nvmax = 10, method = "forward",really.big = TRUE)
sum_fw <- summary(search_fw)
best_size_fw <- which.min(sum_fw$cp)
best_vars_fw <- names(coef(search_fw, best_size_fw))[-1]

form_fw <- as.formula(paste("Price ~", paste(best_vars_fw, collapse = "+")))
model_fw <- lm(form_fw, data = train.df)
model_fw_step <- step(model_fw, direction = "both", trace = 0)

pred_fw <- predict(model_fw_step, newdata = valid.df)
mse_fw <- mean((valid.df$Price - pred_fw)^2)
print(paste("Forward MSE:", mse_fw))




#method = "backward"
search_bw <- regsubsets(Price ~ ., data = train.df, nbest = 1, nvmax = 10, method = "backward",really.big = TRUE)
sum_bw <- summary(search_bw)
best_size_bw <- which.min(sum_bw$cp)
best_vars_bw <- names(coef(search_bw, best_size_bw))[-1]

form_bw <- as.formula(paste("Price ~", paste(best_vars_bw, collapse = "+")))
model_bw <- lm(form_bw, data = train.df)
model_bw_step <- step(model_bw, direction = "both", trace = 0)

pred_bw <- predict(model_bw_step, newdata = valid.df)
mse_bw <- mean((valid.df$Price - pred_bw)^2)
print(paste("Backward MSE:", mse_bw))



#method = "seqrep"
search_sq <- regsubsets(Price ~ ., data = train.df, nbest = 1, nvmax = 10, method = "seqrep",really.big = TRUE)
sum_sq <- summary(search_sq)
best_size_sq <- which.min(sum_sq$cp)
best_vars_sq <- names(coef(search_sq, best_size_sq))[-1]

form_sq <- as.formula(paste("Price ~", paste(best_vars_sq, collapse = "+")))
model_sq <- lm(form_sq, data = train.df)
model_sq_step <- step(model_sq, direction = "both", trace = 0)

pred_sq <- predict(model_sq_step, newdata = valid.df)
mse_sq <- mean((valid.df$Price - pred_sq)^2)
print(paste("Seqrep MSE:", mse_sq))

# step 모델에서 선택된 변수 추출
vars_ex <- names(model_ex_step$coefficients)[-1]
vars_fw <- names(model_fw_step$coefficients)[-1]
vars_bw <- names(model_bw_step$coefficients)[-1]
vars_sq <- names(model_sq_step$coefficients)[-1]

# 결과 비교 테이블
results <- data.frame(
  Method = c("Exhaustive", "Forward", "Backward", "Seqrep"),
  MSE = c(mse_ex, mse_fw, mse_bw, mse_sq),
  Variables = c(
    paste(vars_ex, collapse = ", "),
    paste(vars_fw, collapse = ", "),
    paste(vars_bw, collapse = ", "),
    paste(vars_sq, collapse = ", ")
  ),
  stringsAsFactors = FALSE
)

# 출력
print(results)


install.packages("glmnet")  # 한 번만 설치
library(glmnet)

# 훈련용 행렬과 벡터
x_train <- model.matrix(Price ~ ., data = train.df)[, -1]  # (Intercept 제거)
y_train <- train.df$Price

# 검증용 행렬과 벡터
x_valid <- model.matrix(Price ~ ., data = valid.df)[, -1]
y_valid <- valid.df$Price

# Lasso 적합
lasso.mod <- glmnet(x_train, y_train, alpha = 1)

set.seed(42)  # 재현성을 위해
cv.out <- cv.glmnet(x_train, y_train, alpha = 1)

# 최적의 lambda
best_lambda <- cv.out$lambda.min
print(paste("Best lambda:", best_lambda))

# 시각화로 lambda 선택도 가능
plot(cv.out)

# 예측
lasso.pred <- predict(lasso.mod, s = best_lambda, newx = x_valid)

# MSE 계산
mse_lasso <- mean((y_valid - lasso.pred)^2)
print(paste("Lasso MSE:", mse_lasso))

lasso.coef <- coef(lasso.mod, s = best_lambda)
selected_vars_lasso <- rownames(lasso.coef)[lasso.coef[, 1] != 0]
selected_vars_lasso <- selected_vars_lasso[selected_vars_lasso != "(Intercept)"]

cat("Lasso가 선택한 변수:\n")
print(selected_vars_lasso)

# 결과 표 추가
results_all <- rbind(
  results,
  data.frame(
    Method = "Lasso",
    MSE = mse_lasso,
    Variables = paste(selected_vars_lasso, collapse = ", "),
    stringsAsFactors = FALSE
  )
)

print(results_all)
