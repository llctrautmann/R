install.packages("rattle")
library(rattle)
library(MASS)
data(wine)

# Question 1:
lapply(wine, class)

mod1 <- MASS::lda(Type ~ .,data = wine,CV=T)

plot(mod1)

mod1$class

# 
mod2 <- MASS::lda(Type ~ .,data = wine)
pred = predict(mod2, newdata=wine)
pred$x # LD-axes scores
pred$posterior # posterior class probabilities
pred$class # based on highest posterior probability

# LOOCV confusion table
table(truth = wine$Type, predicted = mod1$class)
mean(wine$Type == mod1$class)

drugzzz <- read.csv("/Users/luca/Downloads/drug_consumption.csv",header = T)
lapply(drugzzz, class)

# Question 5: 

install.packages("biotools")
library(biotools)

options(digits = 10)
boxM(drugzzz[2:13],drugzzz[,19])$statistic



# Question 6: 

mod3 <- MASS::qda(Cannabis~ Age+Gender+Education+Country+Ethnicity+Nscore+Escore+Oscore+Ascore+Cscore+Impulsive+SS,data = drugzzz,CV=T)
M <- table(truth = drugzzz$Cannabis, predicted =mod3$class)

colSums(M)
sum(diag(M))

mean(drugzzz$Cannabis == mod3$class)

# Question 8: 
drugzzz$binary <- ifelse(drugzzz$Cannabis == "CL0"|drugzzz$Cannabis == "CL1",0,1)
mod4 <- MASS::qda(binary ~ Age+Gender+Education+Country+Ethnicity+Nscore+Escore+Oscore+Ascore+Cscore+Impulsive+SS,data = drugzzz,CV=T)
mean(drugzzz$binary == mod4$class)


install.packages("klaR")
library(klaR)

stepclass(binary ~ Age+Gender+Education+Country+Ethnicity+Nscore+Escore+Oscore+Ascore+Cscore+Impulsive+SS,data = drugzzz, method = "qda",criterion = "CR",fold =10,direction = "both")


# Question 9 

drugzzzzz <- read.csv("/Users/luca/Downloads/drug_consumption.csv",header = T)

set.seed(135)
rda(Cannabis ~ Age+Gender+Education+Country+Ethnicity+Nscore+Escore+Oscore+Ascore+Cscore+Impulsive+SS,data = drugzzzzz)
