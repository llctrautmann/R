# Data, packages, options

options(digits = 10)

# Question 1

iris <- iris
pairs(iris, col = as.factor(iris$Species))

setosa <- iris[1:50,1:4]
versicolor <- iris[50:100, 1:4]
verginica <- iris[100:150, 1:4]
colMeans(setosa)
colMeans(versicolor)
colMeans(verginica)

# Question 2 


iris4D = iris[1:4] # we're pretending we don't know the true group labels
kmeans_TotWSS = function(k) kmeans(iris4D, k, nstart=20)$ tot.withinss

set.seed(2986)
totwss = sapply(1:40, kmeans_TotWSS)
plot(totwss, type='b', ylab="Total WSS") # choose 3 clusters: after 3rd cluster the WSS doesn't drop much


k.means_fit <- kmeans(iris4D, 3, nstart=20)
k.means_fit$cluster

k.means_fit$iter


# Question 3

## Luca
set.seed(10)

kmeans_TotWSS2 = function(k) kmeans(iris4D, k, nstart=1)$ tot.withinss
totwss = sapply(1:4, kmeans_TotWSS2)
plot(totwss, type='b', ylab="Total WSS")

## Sterre
set.seed(10)
wcss <- function(k) {
  kmeans(iris4D, k)$tot.withinss
}
kRange <- 1:4
wcssValues <- c(length = max(kRange))
for (k in kRange) {
  wcssValues[k] <- wcss(k)
}
plot(kRange, wcssValues,
     type="b", pch = 19, frame = FALSE,
     xlab="k",
     ylab="Total within cluster sum of squares")

#'[The results are the same]

# Question 4

k.means_fit <- kmeans(iris4D, 3, nstart=20)

sum(k.means_fit$cluster[1:50] != 3)
sum(k.means_fit$cluster[51:100] != 2)
sum(k.means_fit$cluster[101:150] != 1)

table(iris$Species, k.means_fit$cluster)

1 - 16/150 # accuracy 

# Question 5 
distance <- as.matrix(dist(iris4D, method="euclidean"))

distance[48,141] # euclidean distance for these specific points


# Question 6 

d <- dist(iris4D, method = "euclidean")
clust.average <- hclust(d, method = "average")
plot(clust.average)

max(cutree(clust.average,h = 1.1))

cutree(clust.average,k = 3)

table(cutree(clust.average,k = 3))

# Question 7 

table(cutree(clust.average,k = 3),iris$Species)

1 - 14/150

# Question 8 
clust.average <- hclust(d, method = "complete")
plot(clust.average)

table(cutree(clust.average,k = 3),iris$Species)


1 - (49+27)/150


## Sterre 
clust.complete <- hclust(d, method = "complete")
cutree_complete2 <- cutree(clust.complete, k=3)
table(iris$Species, cutree_complete2)


# Question 9 
clust.average <- hclust(d, method = "single")
plot(clust.average)

table(cutree(clust.average,k = 3),iris$Species)

1- 48/150

# Question 10 

clust.complete <- hclust(d^2, method ="centroid")
plot(clust.complete)
table(cutree(clust.complete,k = 3),iris$Species)

1- 14/150
0.90



# Question 11

## DONE 

# Question 12 
sim_clustdata = function(n=30, delta = 3.5, cor = 0.5, p=4, seed=NULL) {
  cor[1:3] = cor # make sure cor has 3 elements
  n[1:3] = n # make sure n has 3 elements
  
  stopifnot(all(cor > - 1 / (p-1))) # enemy of a friend is an enemy principle
  
  mu_cluster1 = c(-sqrt(2),.5,0,0)*delta
  mu_cluster2 = c(+sqrt(2),.5,0,0)*delta
  mu_cluster3 = c(0,-1,0,0)*delta
  
  Sigma_cluster1 = diag(4)*(1-cor[1]) + cor[1]
  Sigma_cluster2 = diag(4)*(1-cor[2]) + cor[2]
  Sigma_cluster3 = diag(4)*(1-cor[3]) + cor[3]
  
  set.seed(seed)
  Y1 = MASS::mvrnorm(n[1], mu_cluster1, Sigma_cluster1)
  Y2 = MASS::mvrnorm(n[2], mu_cluster2, Sigma_cluster2)
  Y3 = MASS::mvrnorm(n[3], mu_cluster3, Sigma_cluster3)
  g = rep(1:3, n)
  
  Y = rbind(Y1, Y2, Y3)
  data.frame(Y, g=factor(g))
}



cluster_data <- sim_clustdata(seed = 13707)

pairs(cluster_data, col = as.factor(cluster_data$g))

# Question 13 

set.seed(13707)

cluster_data <- sim_clustdata(delta = 3)

pairs(cluster_data, col = as.factor(cluster_data$g))

kmeans_TotWSS13 = function(k) kmeans(cluster_data, k, nstart=20)$ tot.withinss

totwss = sapply(1:40, kmeans_TotWSS13)
plot(totwss, type='b', ylab="Total WSS") # choose 3 clusters: after 3rd cluster the WSS doesn't drop much


# Question 14 

delta = c(3,2,1.5,0.5,0.25,0)

for (i in 1:length(delta)) {
  data <- sim_clustdata(delta = delta[i],seed = 13707)
  print(delta[i])
  totwss = sapply(1:40, kmeans_TotWSS13)
  plot(totwss, type='b', ylab="Total WSS")
}

## STERRE
data3 <- sim_clustdata(delta = 0)
kmeans_TotWSS = function(k) kmeans(data3, k, nstart=20)$tot.withinss
totwss = sapply(1:4, kmeans_TotWSS)
plot(totwss, type='b', ylab="Total WSS")

# Question 15 
delta = c(3,2,1.5,0.5,0.25,0)

for (i in 1:length(delta)) {
  print(paste("These are the results for delta " , delta[i]))
  data <- sim_clustdata(delta = delta[i],seed = 13707)
  algo <- kmeans(data, centers = 3, nstart = 20)
  print(table(algo$cluster,data$g))
  d <- dist(data, method = "euclidean")
  clust.average <- hclust(d, method = "average")
  print(delta[i])
  print(table(cutree(clust.average,k = 3),data$g))
}

## STERRE

Dlt <- c(3, 2, 1.5, 0.5, 0.25, 0)
for(i in 1:length(Dlt)) {
  set.seed(13707)
  print(delta[i])
  Data <- sim_clustdata(delta = Dlt[i])
  Kmean <- kmeans(Data, centers = 3, nstart = 20)
  Hclust <- hclust(dist(Data, method = 'euclidean'), method = 'single')
  Hcutree <- cutree(Hclust, k=3)
  print(table(Kmean$cluster,Data$g))
  print(table(Hcutree,Data$g))
}

# Question 16
eufood = read.csv("https://openmv.net/file/food-consumption.csv")
eufood[which(is.na(eufood), arr=T)] = c(47,72,0)
rownames(eufood) = eufood$Country
eufood = eufood[-(1:2)]


kmeans_TotWSS = function(k) kmeans(eufood, k, nstart=20)$tot.withinss
totwss = sapply(1:15, kmeans_TotWSS)
plot(totwss, type='b', ylab="Total WSS")

# Question 17 
set.seed(1869)
kmeans_nations <- kmeans(eufood, centers = 4, nstart = 20)

# Question 18
PCA <- prcomp(eufood, rank =2)
biplot(PCA, col = c(1,0),expand=1.5,xlim=c(-.5,.5),ylim=c(-.5,.5),cex=0.7)
points(predict(PCA), col = kmeans_nations$cluster, pch = 16,cex = 0.7)

# Question 19
H <- hclust(dist(eufood, method = 'euclidean'), method = 'complete')
Cut <- cutree(H, k=1)
plot(H)

# Question 20 
g1 <- c("Italy","Spain","Portugal","Austria")
eufood_1 <- eufood[g1,]
S1 <- cov(eufood_1)

3 * sum(diag(S1))

cov.wt(eufood_1)

WSS_1 <- 3 * sum(diag(cov.wt(eufood_1)$cov))

eufood_2 <- eufood[c("Germany","Belgium","Luxembourg","France","Switzerland"),]
S2 <- cov.wt(eufood_2)

WSS_2 <- 4 * sum(diag(cov.wt(eufood_2)$cov))

eufood_3 <- eufood[c("England","Holland","Ireland"),]
S3 <- cov.wt(eufood_3)

WSS_3 <- 2 * sum(diag(cov.wt(eufood_3)$cov))


eufood_4 <- eufood[c("Norway","Sweden","Finland","Denmark"),]
S4 <- cov.wt(eufood_4)

WSS_4 <- 3 * sum(diag(cov.wt(eufood_4)$cov))

TSS <- 15 * sum(diag(cov.wt(eufood)$cov))

(WSS_1+WSS_2+WSS_3+WSS_4)/TSS
1- ((WSS_1+WSS_2+WSS_3+WSS_4)/TSS)
# Question 21



























