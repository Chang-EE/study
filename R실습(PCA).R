#-----------------------------------------------------------------------------
# 주성분 분석
#-----------------------------------------------------------------------------

# 1. 데이터 생성
library(MASS)
set.seed(42)
mu <- c(0, 0)
sigma <- matrix(c(3, 2, 2, 2), 2)
x <- MASS::mvrnorm(100, mu = mu, Sigma = sigma)

plot(x)

# 2. PCA 수행
pc <- prcomp(x, center=T) # 센터링은 원래는 사실 디폴트임
summary(pc)
center <- colMeans(x)     # 데이터 중심점
pc$center

pc$x[1,1]                 # 1번째 관측값의 첫 번째 방향에서의 PC 점수(lambdad_11)
sum((x[1,]-center)*w[,1]) 
w <- pc$rotation          # 주성분 방향 (고유벡터)
lambda <- pc$sdev         # PCA 고유값의 제곱근
scale <- 3                # 화살표 길이 조정용 스케일

plot(x,asp=1)             # 스케일링 비슷한거 인듯

# 3. 주성분 축 끝점 계산

pc1 <- center + w[,1] * lambda[1] * scale
pc2 <- center + w[,2] * lambda[2] * scale

# 4. 시각화
plot(x, asp = 1, col="grey", pch = 16,
     main = "PCA with plot()", xlab = "x1", ylab = "x2")

# 주성분 축 그리기

arrows(center[1], center[2], pc1[1], pc1[2], col = "red", lwd = 2)
arrows(center[1], center[2], pc2[1], pc2[2], col = "blue", lwd = 2)


#Note.
#  X=UDV'
#  S=1/(n-1) X'X  <- prcomp()에서 계산한 고유값은 이 행렬의 고유값임
#  S=1/(n-1)VD^2V'
#  따라서 lambda_j = D_j^2 / (n-1)  (j=1, ...,p)

# SVD 수행
# 데이터 중심화 (평균을 0으로 만들기 — SVD의 전제 조건)
z <- scale(x, center = TRUE, scale = FALSE)  # 평균만 빼고, 스케일은 유지

result<-svd(z)
U<-result$u   # 좌측 특이벡터 (n x p)
D<-result$d   # 특이값 (벡터 형태, √고유값)
V<-result$v   # 우측 특이벡터 (p x p), 주성분 방향

# PCA와 SVD에서 고유값 계산

(pc$sdev)^2  # PCA 고유값
D^2/(nrow(x)-1) # SVD 특이값 => 고유값으로 환산 


# 주성분 점수 계산 (UD = X V)

scores<-U%*%diag(D)
head(scores)    # head(pc$x)
head(V)         # head (w)

# 주성분 분석 
## 예제 2: Breakfast Cereals (p116)

library(tidyverse)
library(dplyr)

Cereals <- read.csv("dmba-datasets/dmba/Cereals.csv", header=T)

#Cereals<-read.csv("Cereals.csv")
cereals.df <- Cereals%>%dplyr::select(calories, rating) # pipe operator

# compute PCs on two dimensions
pcs <- prcomp(cereals.df)
summary(pcs)
pcs$rot
scores <- pcs$x
head(scores, 5)

# 2. 축 길이 조정
scale1 <- 90
scale2 <- 50

# 3. 각 주성분 축 끝점 계산

center<-pcs$center
pc1 <- center + scale1 * pcs$rotation[, 1]
pc2 <- center + scale2 * pcs$rotation[, 2]


# 4. 산점도
plot(cereals.df,
     pch = 16, col = "black",
     xlim = c(0, 200), ylim = c(0, 110),
     xlab = "Calories", ylab = "Rating", asp = 1)

# 5. 주성분 화살표 (양방향)

arrows(center[1], center[2], pc1[1], pc1[2], col = "red", lwd = 2)
arrows(center[1], center[2], pc2[1], pc2[2], col = "blue", lwd = 2)


## Principal Components

Cereals[1,]
cereals.df <- Cereals%>%column_to_rownames("name")%>%
  dplyr::select(-c(mfr, type))%>%
  drop_na()

pcs <- prcomp(cereals.df)
summary(pcs)
pcs$rotation[,1:5]


## 데이터의 정규화

# Use function prcomp() with scale. = T to run PCA on normalized data
pcs.cor <- prcomp(cereals.df, scale. = T)

summary(pcs.cor)
pcs.cor$rotation[,1:5]



pcs <- prcomp(cereals.df, scale. = TRUE)  # 표준화 포함

# 2. PC1, PC2 산점도 그리기 
plot(pcs$x[, 1], pcs$x[, 2],
     xlab = "PC1", ylab = "PC2",
     pch = 21, cex=0.5,bg = "lightblue", col = "black", asp = 1)

# 3. 관측치 이름(행 이름)으로 라벨 추가
text(pcs$x[, 1], pcs$x[, 2],
     labels = rownames(pcs$x),
     pos = 3, cex = 0.3)  # pos = 3: 위쪽에 표시