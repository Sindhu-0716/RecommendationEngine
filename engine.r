
##---------------SVD-----------------------##
DF<-data.frame(titles$userId,titles$movieId,titles$rating,stringsAsFactors = FALSE)
str(DF)
#converting data frame to a matrix
library(tidyverse)
install.packages("tidyverse")
rat_mat <- titles %>% select(-timestamp) %>% 
  spread(movieId, rating) %>%
  remove_rownames %>%
  column_to_rownames(var="userId")
View(rat_mat)
dim(rat_mat)


rat_mat <- as.matrix(rat_mat)
dimnames(rat_mat) <- list(user= rownames(rat_mat), item = colnames(rat_mat))
rat_mattrain <- as.matrix(train_ind)
dimnames(rat_mattrain) <- list(user= rownames(rat_mattrain), item = colnames(rat_mattrain))
View(rat_mattrain)

## 50% of the sample size
smp_size <- floor(0.5 * nrow(rat_mat))
## set the seed to make your partition reproducible
set.seed(123) #randomization`
train_ind <- sample(seq_len(nrow(rat_mat)), size = smp_size)
View(train)

train <- DF[train_ind,]
test <- DF[-train_ind, ]

####dbscan___Density Based Clustering

install.packages("fpc")
install.packages("dbscan")
install.packages("factoextra")
dbscan::kNNdistplot(train, k =  5)
set.seed(1000)
# fpc package
res.fpc <- fpc::dbscan(train, eps =0.02 , MinPts = 1)
print(res.fpc)
# dbscan package
res.db <- dbscan::dbscan(train, 0.02, 1)
fviz_cluster(res.db, train, geom = "point",ellipse= TRUE, show.clust.cent = TRUE,
             palette = "jco", ggtheme = theme_classic())

fviz_cluster(res.fpc, train,  geom = "point", 
             ellipse= TRUE, show.clust.cent = TRUE,
             palette = "jco", ggtheme = theme_classic())


fr <- frNN(train,eps=20)
table(fr)
dbscan(fr, MinPts = 1)
res <- dbscan(fr, eps = 0.02, MinPts = 1)
pairs(train, col = res$cluster + 1L)
View(fr$dist)
plot(kNN(fr, eps = 25), train)

##SVD---------Singular Value Decomposition----
# Computing the SVD
svdtest<-svd(train$titles.rating, nu = min(1, 1), nv = min(3, 3))
approx20 <- svdtest$u[, 1:1] %*% diag(svdtest$d[1:1]) %*% t(svdtest$v[, 1:1])
svdtest

decomp<-irlba(train, nu =1, nv = 1)
Predict <- b + (svdtest$u * sqrt(svdtest$d)) %*% (sqrt(svdtest$d) * t(svdtest$v))
Prediction_matrix<-data.frame(train$titles.userId,train$titles.movieId,Predict[,"titles.rating"])
View(Prediction_matrix)
predictionFunction()

# Renaming the rows and columns for easier lookups
colnames(Predict) <- c("titles.rating")
rownames(Predict) <- rownames(train)
View(Predict)

RMSE <- function(predictionMatrix, actualMatrix){
  sqrt(mean((predictionMatrix - actualMatrix)^2, na.rm=T))
}
View(Predict)
trainNA <- train
is.na(train) <- train== 0
RMSE2<-RMSE(Predict[,'titles.rating'], train$titles.rating)

# calculate RMSE for SVD model using the imputed NA values as the basis
IMP_SVD_RMSE <- sqrt(base::mean((train_ind - Predict)^2) )
IMP_SVD_RMSE


###Building recommender system for making predictions
getrecommendation <- function(titles.rating){
  if(titles.rating!= (RMSE+train$titles.rating)){
    paste("Previously Rated:")
  }
  else{
    paste("Predicted Rating:", round(Predict[,"titles.rating"],1))
  }
}
f("4.005282")
f <- function(titles.rating) {
  ifelse(titles.rating ==4.005282, "predicted","unpredicted")
}
Predict[,"titles.rating"]
my.ran <- function(titles.rating){
  if(titles.rating !=Predict[,"titles.rating"])
    stop("Previously Rated:")
  else if(titles.rating == "Predicted Rating:") return(round(Predict[,"titles.rating"],1))
 
}
p<-my.ran("4.005282")



