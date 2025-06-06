# chapter 2. R 코드 실습 

#---------------------------------------------------------------------------
# <표 2.3>  R 데이터 로딩코드 (p43) 
#---------------------------------------------------------------------------
housing.df <- read.csv("dataset/WestRoxbury.csv", header = TRUE) # load data
dim(housing.df)  #find the dimension of data frame
head(housing.df) #show the first six rows
View(housing.df) #show all the data in a new tab


#---------------------------------------------------------------------------
# <표 2.3>  R 데이터 보기 (p43)
#---------------------------------------------------------------------------

# Practice showing different subsets of the data
housing.df[1:10, 1] # show the first 10 rows of the first column only
housing.df[1:10, ]  # show the first 10 rows of each of the columns
housing.df[5, 1:10] # show the fifth row of the first 10 columns
housing.df[5, c(1:2, 4, 8:10)] # show the fifth row of some columns
mean(housing.df$TOTAL.VALUE) # find the mean of the first column
summary(housing.df) # find summary statistics for each column


#---------------------------------------------------------------------------
# <표 2.4> 샘플링
#---------------------------------------------------------------------------
# random sample of 5 observations
s <- sample(row.names(housing.df), 5);s
housing.df[s,]
# oversample houses with over 10 rooms
s <- sample(row.names(housing.df), 5, prob = ifelse(housing.df$ROOMS>10, 0.9, 0.01))
housing.df[s,]


#---------------------------------------------------------------------------
# <표2.5> 변수를 설명하는 R 코드
#---------------------------------------------------------------------------

names(housing.df) # print a list of variables to the screen.
t(t(names(housing.df))) # print the list in a useful column format
colnames(housing.df)[1] <- c("TOTAL_VALUE") # change the first column's name
class(as.factor(housing.df$REMODEL)) # REMODEL is a factor variable
class(housing.df[ ,14]) # Same.
levels(as.factor(housing.df[ ,14])) # It can take one of three levels
class(housing.df$BEDROOMS) # BEDROOMS is an integer variable


#---------------------------------------------------------------------------
# <표2.6> 가변수 생성
#---------------------------------------------------------------------------

# 범주형 변수에 대한 가변수 생성

# a set of dummy variables. We must then turn the resulting data matrix back into
# a data frame for further work.


#---------------------------------------------------------------------------
# <표 2.9> 데이터 분할
#---------------------------------------------------------------------------

## 1) partitioning into training (60%) and validation (40%)
# randomly sample 60% of the row IDs for training; the remaining 40% serve as
# validation
set.seed(1)
train.rows <- sample(rownames(housing.df), dim(housing.df)[1]*0.6)
# collect all the columns with training row ID into training set:
train.data <- housing.df[train.rows, ]
# assign row IDs that are not already in the training set, into validation
valid.rows <- setdiff(rownames(housing.df), train.rows)
valid.data <- housing.df[valid.rows, ]
# alternative code for validation (works only when row names are numeric):

# collect all the columns without training row ID into validation set
valid.data <- housing.df[-train.rows, ] # does not work in this case


## 2) partitioning into training (50%), validation (30%), test (20%)
# randomly sample 50% of the row IDs for training
set.seed(123)
train.rows <- sample(rownames(housing.df), dim(housing.df)[1]*0.5)
# sample 30% of the row IDs into the validation set, drawing only from records
# not already in the training set
# use setdiff() to find records not already in the training set
valid.rows <- sample(setdiff(rownames(housing.df), train.rows),
dim(housing.df)[1]*0.3)
# assign the remaining 20% row IDs serve as test
test.rows <- setdiff(rownames(housing.df), union(train.rows, valid.rows))
# create the 3 data frames by collecting all columns from the appropriate rows
train.data <- housing.df[train.rows, ]
valid.data <- housing.df[valid.rows, ]
test.data <- housing.df[test.rows, ]

# Note. 주의사항 
set.seed(123)
sample(1:20,7)
sample(1:20,7)
sample(1:20,7)