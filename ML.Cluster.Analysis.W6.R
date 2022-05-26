data <- read.csv2("/Users/luca/Downloads/data_AUT.csv",header = T)

options(digits = 10)
# Q1 

mean(data[,"creativity"]) 

range(data[,"utility"])
range(data[,"creativity"])

summary(data)

# Q2 In this assignment, the goal is to find categories of responses. 
# One category could be “building” - responses in this category could be: 
# building a house, house, tower, build shed, construction etc. Another 
# category could be “games” with responses such as: jenga, use bricks as dominos,
# jeu de boules, playing catch etc. Doing this by hand is very time-consuming,
# that’s why you are going to use both k-means clustering and hierarchical clustering to do this.

length(unique(data[,"original_response"]))

# Question 5: k.means clustering
 
set.seed(2972)

k.means.fit <- kmeans(data[,c(23,35:130)], 4, nstart = 20)
k.means.fit

# Question 8: 
d <- dist(data[,c(23,35:130)], method = "euclidean")

clust.single <- hclust(d, method = "single")
clust.average <- hclust(d, method = "average")
clust.complete <- hclust(d, method = "complete")


single <- cutree(tree = clust.single, k = 40)
table(single)

average <- cutree(tree = clust.average, k = 40)
table(average)

complete <- cutree(tree = clust.complete, k = 40)
table(complete)

# Question 8: 
d <- dist(data.frame.data, method = "euclidean")
clust.single <- hclust(d, method = "single")
clust.average <- hclust(d, method = "average")
clust.complete <- hclust(d, method = 'complete')
plot(clust.single)
plot(clust.average)
plot(clust.complete)
cutree_average <- cutree(clust.average, k = 40)
cutree_complete <- cutree(clust.complete, k = 40)
table(cutree_average)
table(cutree_complete)

# Question 9:

new_data_frame <- data.frame(data$original_response, cutree_average)
new_data_frame_complete <- data.frame(data$original_response, cutree_complete)
# look at the responses in the complete linkage clusters
#complete
new_data_frame_complete[new_data_frame_complete$cutree_complete == 21, 1]
new_data_frame_complete[new_data_frame_complete$cutree_complete == 2, 1]
#average
new_data_frame[new_data_frame$cutree_average == 19, 1]
new_data_frame[new_data_frame$cutree_average == 15, 1]

# Question 9, Leonie Output

clusterd <- cutree(clust.complete, h = 40)
library(dplyr)
datb <- data %>% select(original_response) %>% mutate(clusterd = clusterd)
datb %>% head()
datb[datb$clusterd == 21,1]
datb[datb$clusterd == 2,1]
clustere <- cutree(clust.average, h = 40)
library(dplyr)
date <- dat %>% select(original_response) %>% mutate(clustere = clustere)
date %>% head()
date[date$clustere == 19,1]
date[date$clustere == 15,1]

# Question 10: 
dataset <- data.frame(data$originality, data$utility)
set.seed(87)
wcss <-function(k){kmeans(dataset, k)$tot.withinss}
kRange <- 1:10
wcssValues <- numeric()
for(k in kRange) { wcssValues[k] <-wcss(k) }
plot(kRange, wcssValues, type="b", pch = 19, frame = FALSE, xlab="k", ylab="Total within cluster sum of squares")

# Question 11:
set.seed(87)
kmeans_fit <- kmeans(dataset, 6, nstart=20)
aut_clusters <- cbind(data["original_response"], data["originality"], data["utility"], kmeans_fit$cluster)
head(aut_clusters[kmeans_fit$cluster == 1, 1], 10)
cluster1 <- aut_clusters[(aut_clusters[,4] == 1),] #comma adds a rownumber
cluster3 <- aut_clusters[(aut_clusters[,4] == 3),]
cluster4 <- aut_clusters[(aut_clusters[,4] == 4),]
plot(jitter(cluster1$originality), jitter(cluster1$utility), xlim = c(1,5), ylim = c(1,5))
plot(jitter(cluster3$originality), jitter(cluster3$utility), xlim = c(1,5), ylim = c(1,5))
plot(jitter(cluster4$originality), jitter(cluster4$utility), xlim = c(1,5), ylim = c(1,5))
