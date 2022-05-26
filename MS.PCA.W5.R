MS assignment 5
#Q.3
S <- cov(trees) ##covariance matrix
eigenvalues <- eigen(S)
lambda1 <- eigenvalues$values[1]
x1 <- eigenvalues$vectors[,1]
names(trees)
Girth <- trees$Girth
Height <- trees$Height
Volume <- trees$Volume
z1 <- x1[1]*Girth + x1[2]*Height + x1[3]*Volume
var(z1)


#Q.5
lambda2 <- eigenvalues$values[2]
x2 <- eigenvalues$vectors[,2]
z2 <- x2[1]*Girth + x2[2]*Height + x2[3]*Volume
var(z2)
cov(z1,z2)
cov(z2,z1)
t(x1)%*%x2 #t is transpose
x1*x2


#Q.6
lambda3 <- eigenvalues$values[3]
x3 <- eigenvalues$vectors[,3]
z3 <- x3[1]*Girth + x3[2]*Height + x3[3]*Volume
t(x3)%*%x1
t(x3)%*%x2
var(z3)
cov(z1, z3)
cov(z2, z3)

#Q.8
PCA <- prcomp(trees, center=TRUE, scale=F)
summary(PCA)
sqrt(lambda1)
17.18342^2
#sqrt lambda1 is equal to principal component 1
sqrt(lambda2)
lambda2 / (lambda1 + lambda2+lambda3)
#Q.9
biplot(PCA, cex = 0.5)
#Q.10
PCA <- prcomp(trees, center=TRUE, scale=T)
screeplot(PCA, type = "lines", col = 'red')
abline(h=1)
summary(PCA)
#Q.11
library(MASS)
mu_set1 = rep(0, 5)
Sigma_set1 = diag(5)*.4 + .6
mu_set2 = seq(0, 1, len=5)
Sigma_set2 = diag(5)*.5 + .5
n = 100
set.seed(9677)
Y1 = MASS::mvrnorm(n, mu_set1, Sigma_set1)
Y2 = MASS::mvrnorm(n, mu_set2, Sigma_set2)
Y = cbind(Y1,Y2)
dat = data.frame(Y)
?cbind
Y2[1,1]
pairs(dat, pch= ".")
pairs(Y1, pch = ".")
pairs(Y2)
cor(Y1)
cor(dat$X2, dat$X3)
summary(Y1)
cor(dat$X7, dat$X9)

cor(Y1[1,1], Y2[1,1])
cor(dat$X4, dat$X6)
cor(Sigma_set1, Sigma_set2)
cor(Y1, Y2)

#Q.12
psych::corPlot(dat)
mean(cor(Y1, Y2))
#Q.13
psych::cortest.bartlett(dat)
psych::KMO(dat)
#Q.14
pca <- princomp(dat)
screeplot(pca, type = "lines")
abline(h=1)
#Q.15
biplot(pca, cex = 0.5)
#Q.16
print(loadings(pca), cutoff=0.25)

#Q.17
set.seed(8803)
mu_set3 = rep(0, 5)
Sigma_set3 = diag(5)*.1 + .9
Y3 = MASS::mvrnorm(n, mu_set3, Sigma_set3)
dat2 = cbind(dat, V=Y3)
pca2 <- princomp(dat2)
screeplot(pca2, type = "lines")
abline(h=1)
summary(pca2)

0.2836765476+0.2318329160+0.2145306393

#Q.18
humor.zip = tempfile()
download.file("http://openpsychometrics.org/_rawdata/HSQ.zip", humor.zip)
humor.files = unzip(humor.zip, list=TRUE)
file.show(unzip(humor.zip, humor.files$Name[1]))
humor = read.csv(unzip(humor.zip, humor.files$Name[2]), na.string = "-1") #na,string. Voor de NA's
unlink(humor.zip)
humor = na.omit(humor)
data <- humor[1:32]
cor_plot <- psych::corPlot(data, stars = T)
#Q.19
psych::cortest.bartlett(data)
psych::KMO(data)
0.#Q.20
PCA <- princomp(data, center=TRUE, scale=T)
screeplot(PCA, type = "lines", npcs = 32,pch = ".")
abline(h=1)



summary(PCA)
S <- cov(data) ##covariance matrix
eigenvalues <- eigen(S)
#Q.21
dim(data)
PCA <- prcomp(data, rank.= 4)
biplot(PCA, choices = 1:2,xlabs = rep(".", 993))
#Q.22
stats:::print.loadings(PCA$rotation, cutoff=.2)
CVD
