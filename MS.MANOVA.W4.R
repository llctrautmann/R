dataURL = "https://online.stat.psu.edu/onlinecourses/sites/stat505/files/data/pottery.txt"
pottery = read.table(dataURL)
colnames(pottery) = c("region", "Al", "Fe", "Mg", "Ca", "Na")

pottery = subset(pottery, region != "C")
pottery$ region = factor(pottery$ region)
rownames(pottery) = NULL

# Question 1:
colMeans(pottery["Ca"])

# Question 2: 

fit <- manova(cbind(Fe,Ca) ~ region,pottery)



# Question 3
summary(car::Manova(fit))

#'[DF1 is the model df and df2 is the error df]
#'[df1 is a composition of the k-1 * p (number of groups in the IV - 1) * (number of dependent variables)]

1 - 0.049808^(1/2) # effect size = 1 - Wilks minimum of the two = which ever has a lower value between the dependent and independent variable

#'[k is the number of groups in the IV and p is the number of DV]

# Question 4

options(digits = 10)
biotools::boxM(pottery[c("Fe","Ca")],grouping = pottery$region)
# insignificant means that the assumptions of homoscedacity cannot be rejected and therefore holds

car::spm(resid(fit), groups = pottery$region, ellipse=TRUE)



# Question 5
install.packages("mvnormtest")
mvnormtest::mshapiro.test(t(resid(fit))) # if the data has a low curtosis then the power of the MANOVA decreases
psych::mardia(resid(fit))
# Question 6
E = resid(fit) 
centroid = c(0,0) # the expected values of variables in E are zero 
V = var(E) # the covariance matrix is estimated from the observed residuals 
d2 = mahalanobis(E, centroid, V)

cutoff <- 2.5 * mean(d2)

d2[d2 > cutoff]

# Question 7

summary(car::Manova(fit),univariate = T,p.adjust.method = "holm") # univariate apparently not possible here

f.region <- factor(pottery$region)

a1 <- aov(Fe ~ region,data = pottery)
summary(a1)

a2 <- aov(Ca ~ region,data = pottery)
summary(a2)

# Question 8 + 9
options(digits = 10)
MASS::lda(region ~ Fe+Ca,data = pottery)

## Optional
summary(car::Manova(fit))
W <- matrix(c(10.9495957143, -0.15544428571,-0.1554442857, 0.05143571429),2,2)
B <-matrix(c(132.277266786,4.517069286,4.517069286,0.156947619),2,2)

WInv <- solve(W)

WB <- WInv %*% B
sum <- sum(eigen(WB)$values)

eigen(WB)$values / sum # qed. 

# Question 10
lda_fit <- MASS::lda(region ~ Fe+Ca,data = pottery)
predict(lda_fit, newdata = pottery)

# Question 11
plot(lda_fit)

# Question 12
lda_fit <- MASS::lda(region ~ Fe+Ca,data = pottery,CV = T)
summary(lda_fit)

lda_fit$class == pottery$region

install.packages('caret')
library(caret)
confusionMatrix(lda_fit$class,pottery$region)

20/length(pottery$region)

# Question 13
lda_fit <- MASS::lda(region ~ Fe+Ca+Al,data = pottery,CV = T)
confusionMatrix(lda_fit$class,pottery$region)

# Question 14
Sigma_E = ability.cov[["cov"]][2:4, 2:4]
mu_a = c(-1, 1, 0)
mu_b = c( 0, 0, 1)
mu_c = c( 1, 1, 0)

n = 20
set.seed(22747)

# Simulate multivariate observations for groups a, b, c
Y_a = MASS::mvrnorm(n, mu = mu_a, Sigma_E) 
Y_b = MASS::mvrnorm(n, mu = mu_b, Sigma_E)
Y_c = MASS::mvrnorm(n, mu = mu_c, Sigma_E)

# Bind the simulated data into a single data frame
df = rbind(Y_a, Y_b, Y_c) 
df = as.data.frame(df)

# Add a group variable
df[["group"]] = gl(3,n,labels=c('a','b','c'))

## my work

df[55,]
fit <- manova(cbind(picture,blocks,maze) ~ group,df)

# 15
options(digits = 10)
summary(car::Manova(fit),univariate = T,p.adjust.method = "holm")
# 16

mvnormtest::mshapiro.test(t(resid(fit))) # normality
biotools::boxM(df[c("picture","blocks","maze")],grouping = df$group) # homo

# 17
E = resid(fit) 
centroid = c(0,0) # the expected values of variables in E are zero 
V = var(E) # the covariance matrix is estimated from the observed residuals 
d2 = mahalanobis(E, centroid, V)

cutoff <- 2 * mean(d2)

d2[d2 > cutoff]

# 18 
options(digits = 10)
lda_fit <- MASS::lda(group ~ picture+blocks+maze, data = df)


# 19
lda_fit <- MASS::lda(group ~ picture+blocks+maze, data = df,CV = T)
confusionMatrix(lda_fit$class,df$group)

# 20 
natmerit = foreign::read.spss("https://www.dropbox.com/s/sfl4aj573tx8olq/NATMERIT.sav?dl=1", to.data.frame=TRUE)
summary(natmerit)
head(natmerit)

sum(natmerit$Group == "Eighth Grade")

# 21
fit <- manova(cbind(Real,Intell,Social,Conven,Enterp,Artis,Status,Aggress) ~ Group,natmerit)
summary(car::Manova(fit))

## Effect size 
1 - 0.5639239067^(1/3)
0.078

# 22 
mvnormtest::mshapiro.test(t(resid(fit))) # normality
biotools::boxM(natmerit[c("Real","Intell","Social","Conven","Enterp","Artis","Status","Aggress")],grouping = natmerit$Group) # homo

# 23
E = resid(fit) 
centroid = c(0,0) # the expected values of variables in E are zero 
V = var(E) # the covariance matrix is estimated from the observed residuals 
d2 = mahalanobis(E, centroid, V)

cutoff <- 2 * mean(d2)

d2[d2 > cutoff]

# 24
summary(car::Manova(fit),univariate = T,p.adjust.method = "holm")

# 25 
lda_fit <- MASS::lda(Group ~Real+Intell+Social+Conven+Enterp+Artis+Status+Aggress, data = natmerit)
lda_fit <- MASS::lda(Group ~Real+Intell+Social+Conven+Enterp+Artis+Status+Aggress, data = natmerit,CV = T)
confusionMatrix(lda_fit$class,natmerit$Group)



mean(c(78,87,74,96))
96-83.75

12.25/22
