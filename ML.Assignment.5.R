## Install packages
install.packages("corrplot")
install.packages("GGally")

## Question 1 
breast_cancer_data <- read.csv("/Users/luca/Downloads/breast_cancer.csv",header = T,sep = " ")

summary(breast_cancer_data)

sum_beniging <- sum(breast_cancer_data$diagnosis == "B")

total_cases <- length(breast_cancer_data$diagnosis)

proportion_beniging <- sum_beniging/total_cases


options(digits = 2)
cor(breast_cancer_data[2:11])
psych::corPlot(breast_cancer_data[2:11])

## Question 2 
options(digits = 10)
corrplot::corrplot(breast_cancer_data[,2:11])

plot1 <- GGally::ggpairs(breast_cancer_data[,2:11])
GGally::ggpairs(breast_cancer_data[,2:11])

## Question 3

dim(breast_cancer_data)
A <- cov(breast_cancer_data[2:11])

B <- cor(breast_cancer_data[2:11])


mod1 <- princomp(breast_cancer_data[,2:31],cor = F,scores = T)
mod2 <- princomp(breast_cancer_data[,2:31],cor = T,scores = T)

mod1$center
mod2$center

mod1$sdev

biplot(mod2)
biplot(mod1)

unstandardizedpca <- princomp(breast_cancer_data[,2:31], cor = FALSE)
standardizedpca <- princomp(breast_cancer_data[,2:31], cor = TRUE)
biplot(unstandardizedpca)
biplot(standardizedpca)
## Question 4

## Question 5

## Question 6

## Question 7

## Question 8 

## Question 9 

## Question 10

## Code Mees
breast <- read.csv("/Users/luca/Downloads/breast_cancer.csv", header = TRUE, sep = "")
#q1
sum(with(breast, diagnosis == "B"))
357/569*100
#q2
cor <- psych::corPlot(breast[,2:11])
#som van alle correlaties die hoger zijn dan 0.3 - de som van alle correlaties die gelijk zijn aan 1 want
(sum(cor > 0.3) - sum(cor == 1))/2
#q3
unstandardizedpca <- princomp(breast[,2:31], cor = FALSE)
standardizedpca <- princomp(breast[,2:31], cor = TRUE)
biplot(unstandardizedpca)
#q5
screeplot(standardizedpca, type = "lines", col = 3)
abline(h=1)

summary(standardizedpca)

0.4427202561 + 0.1897118204 + 0.09393163257 + 0.06602134915 + 0.05495768492 + 0.0402452204
#q6
plot(standardizedpca$scores[,c(1,2)],col = breast$diagnosis)
plot(standardizedpca$scores[,1], standardizedpca$scores[,4], col = breast$diagnosis)
#q7
mylda = data.frame(standardizedpca$scores[,1:5], breast$diagnosis)
mylda2 = data.frame(standardizedpca$scores[,1:6], breast$diagnosis)
LDA <- MASS::lda(mylda$breast.diagnosis ~ ., data = mylda, CV = TRUE)
LDA2 <- MASS::lda(mylda2$breast.diagnosis ~ ., data = mylda2, CV = TRUE)
table(truth = mylda$breast.diagnosis, predicted = LDA$class)
mean(mylda$breast.diagnosis == LDA$class)
table(truth = mylda2$breast.diagnosis, predicted = LDA2$class)
mean(mylda$breast.diagnosis == LDA$class) - mean(mylda2$breast.diagnosis == LDA2$class)
#q8
table(truth = mylda$breast.diagnosis, predicted = LDA$class)
1/569
28/569
184/(184+28) #sensitivity
356/357 #specificity
