universities.df <- read.csv("dmba-datasets/dmba/Universities.csv", header=T)
head(universities.df, 10)

univ_num.df <- universities.df[, sapply(universities.df, is.numeric)]
head(univ_num.df, 10)

univ_num_nonna.df <- na.omit(univ_num.df)
head(univ_num_nonna.df, 10)

pcs <- prcomp(univ_num_nonna.df, scale. = TRUE)
summary(pcs)
pcs$rot[,1:5]

pcs <- prcomp(univ_num_nonna.df, scale. = FALSE)
summary(pcs)
pcs$rot[,1:5]
