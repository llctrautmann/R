########################################################################################################################################################################################################
# Packages
########################################################################################################################################################################################################


library(psych)
library(MASS)
library(car)
library(ggplot2)
library(mvnormtest)
library(biotools)
library(dplyr)

########################################################################################################################################################################################################
# Week 1: Data Science and Matrix Algebra
########################################################################################################################################################################################################

## MATRIX ALGEBRA
A <- matrix(c(-6,-7,-9,-7,4,-4,8,5,2),3,3,byrow = T)

B <- matrix(c(83,23,-61),3,1)


tr(A) # tr() gives the trace of a matrix; requires A to be square

solve(A) # solve() finds the inverse of a matrix, if the inverse exists

t(A) # t() transforms the matrix

det(A) # det() provides the determinant of a matrix; if det != 0 then the matrix has an inverse

round(A %*% t(A),5) # if the result of the A * A(transposed) = I then the Matrix is orthogonal


## Eigenvalues & Eigenvector (Av = cv (eqation for the eigenvalues and Eigenvector))

eigen(A)$value # get the eigenvalues
eigen(A)$vector # get the eigenvectors 


########################################################################################################################################################################################################
# Week 2: Multiple linear Regression
########################################################################################################################################################################################################

## LINEAR REGRESSION

model <- lm(y ~ x1 + x2 + ... + xn, data = data)

### PREDICTED VALUES
ypred <- beta_1*data$x_1-intercept # equal to predict()
predict(object = model) # provides the predicted values; basically uses the regression equation and inserts the values for the variables
predict(object = model, newdata = new_data) # applies a model onto new data 


### MODEL ERROR
error <- data$y - ypred # (all are equal)
model$residuals
resid(model)


### VARIANCE
var(ypred) + var(error) == var(data$y)
var(ypred) / var(data$y) == SSR / SSR + SSE # SSR + SSE = TSS; and this is obtained from an anova output
anova(model) # to get the SSR and SSE



### STATISTICS AND COEFFICIENTS
#### COEFFICIENTS
coef(model)['variable_name'] # gives the coefficient for the selected variable name

#### STATISTICS
summary_model <- summary(model) # required first step
summary_model$fstatistic[1:3] # F statistic, df1, df2

summary_model$r.squared # r.squared this is equal to 
cor(model$y,predict(model))^2

pf(summary_model$fstatistic[1], summary_model$fstatistic[2], summary_model$fstatistic[3], lower.tail = FALSE) # P value


### ASSUMPTIONS
#### Testing the assumption of NORMALITY
shapiro.test(model$residuals) # if significant then the assumption is violated, visual equivalent is QQ-plot


#### Testing no constant error variance / Homoscedasticity
car::ncvTest(model) #if significant Homoscedasticity is violated


#### ZERO CONDITIONAL MEAN OF THE RESIDUALS / MODEL MISSPECIFICATION
lmtest::resettest(model,type = "regressor",data = data)  # assumes prediction equation is correct; significance == violation 


### OUTLIER

plot(model, which=5); # plot number 5 Residuals vs. Leverage
abline(v = 2*mean(hatvalues(model)), col=3, lty=3) # count on the x axis

#### Independent variable space
sum(hatvalues(model)>2*mean(hatvalues(model))) # gives a calculated number
which(hatvalues(model)>2*mean(hatvalues(model))) # for labled values

#### Dependent variable space
sum(abs(rstandard(fit)) > 2) # on the y axis; outside of [-2:2] is considered problematic
sum(abs(rstandard(fit)) < -2)

#### Independent and Dependent
sum(cooks.distance(fit) > 0.5) # Cook’s Distance of > 1 is usually considered problematic


### OTHER CONSTRAINTS
#### Multicollinearity

car::vif(model) # values larger than 5 are noteworthy and larger than 10 are issues

### PLOTS FOR REGRESSION

plot(model,which = number) # gives diagnostic plots


### HIRACHICAL REGRESSION

car::Anova(model) # both models add predictors hierarchically 
drop1(model, test = "F")


#### MODEL SELECTION

MASS::stepAIC(model, direction = "both/forward/backward") #choose one; lower AIC better fit
MASS::stepAIC(model)[['anova']][['AIC']][1:xxx] # add number for xxx for model number


## MACHINE LEARNING
### sub-setting the data set with a randomised vector

sample <- sample(1:nrow(data),size_of_the_sample,replace = F)

training_set <- data[sample,]
testing_set <- data[-sample,]

### CROSS-VALIDATION

#### R.squared for new data 
new_predictions <- predict(model,newdata = testing_set)
correlation <- cor(testing_set$cnt,new_predictions)
r.squared <- correlation^2

#### LOOCV
fitted_value <- NULL
for(i in 1:nrow(data)) {
  sample <- i
  train <- data[-sample,]
  test <- data[sample,]
  model <- lm(y ~ x1+ ... + xn, data = train)
  fitted_value[i] <- predict(model,newdata = test)
}

confusionMatrix(fitted_value, data$y) # requires caret package


PRESS = sum(rstandard(model, type="pred")^2) # PRESS has to be equal to the sum((observed.values(y)-fitted_value)^2)

RMSE <- sqrt(PRESS/(nrow(model)-1))

RMSE_LOOCV <- sqrt(mean((model$y- fitted_value)^2))


#### FORWARD, BACKWARD REGRESSION
# difference between step wise and forward regression
library(MASS)
# define a start model and full model
start_model <- lm(y ~ 1, data = data) # no features included
full_model <- lm(y ~ ., data = data) # all features included

# forward regression
fit_forward<- stepAIC(start_model, scope=list(upper=full_model), direction = "forward")
fit_forward$ anova # check with anova to decide which model to select

# stepwise regression
fit_stepwise <- stepAIC(start_model, scope=list(upper=full_model), direction = "both")
fit_stepwise$ anova # check with anova to decide which model to select



########################################################################################################################################################################################################
# Week 3: Logistic Regression
########################################################################################################################################################################################################

## LOGISTIC MODEL 

model <- glm(y ~ x,data = data,family = binomial("logit"))

### LOG-ODDS

logOdds = -0.1 + 1.2*x1 - 0.5*x2 # the regression equation computes the logodds 
prob = plogis(logOdds) # the probablities are then taken from the logistic distribution 
y = rbinom(n, prob = prob, size = 1)

### PREDICTED VALUES

predictedValues <- predict(model,type = 'response') # response give the probabilities

### TABLE CONFUSION MATRIX 

ref <- predictedValues > 0.5

predictedValues[ref] # above ref
predictedValues[-ref] # below ref

table(ref,data$y) # confusion matrix for the prediction

### PREDICTION ACCURACY FUNCTION

accuracy = function(y, yhat) sum(diag(table(y >= 0.5, yhat >= 0.5))) / length(y) # requires vector with observed and predicted values
boot::cv.glm(data = data,glmfit = model,cost = accuracy,K = 10)



## MACHINE LEARNING

### K-FOLD CROSS VALIDATION

k <- 5
covid <- covid[sample(nrow(covid)),] # shuffle data see sample().R in functions.explained

folds <- cut(seq(1,nrow(covid)), breaks=k, labels=FALSE) # break into k segments
acc <- numeric()

for(i in 1:k) {
  train <- covid[folds != i,]
  testing <- covid[folds == i,]
  model <- stats::glm(y ~ x1 + ... + xn,family = binomial(),data = train)
  predictions <- predict(model,newdata = testing,type = "response")
  predictions <- ifelse(predictions >= .5, 1, 0)
  acc[i] <- mean(predictions == testing$hospitalized) #'[mean = accuracy]
}

mean(acc)

### ADDITIONAL NOTES

# accuracy (TN+TP)/total: strikes a balance between sensitivity and specificity
# specificity (TN)/ (TN+FP): because we should not waste healthcare resources on false alarms
# sensitivity (TP)/ (TP+FN): because we need to prevent suicides the best we can


########################################################################################################################################################################################################
# Week 4: MANOVA and DISCRIMINANT ANALYSIS
########################################################################################################################################################################################################

## MANOVA
model <- manova(cbind(dependent_variable_1,..., dependent_variable_1) ~ independent_variable_1,...,independent_variable_n)
summary(car::Manova(model)) # e.g. fit <- manova(cbind(Fe,Ca) ~ region,pottery)

#'[DF1 is the model df and df2 is the error df]
#'[df1 is a composition of the k-1 * p (number of groups in the IV - 1) * (number of dependent variables)]

summary(car::Manova(fit)) # MANOVA Main Effect
## ASSUMPTIONS IN MANOVA

### Homoscedasticity
biotools::boxM(data[c("Fe","Ca")],grouping = pottery$region) # insignificant means that the assumptions of homoscedacity holds

### Normality
mvnormtest::mshapiro.test(t(resid(model)))


### Equality of covariance matrices
BoxM.test(fit, test = "Chi")

### Effect size approximation
1 - Wilks_Statistic^(1/2) # effect size = 1 - Wilks minimum of the two = which ever has a lower value between the dependent and independent variable

### Outlier 
E = resid(model) 
centroid = c(0,0) # the expected values of variables in E are zero 
V = var(E) # the covariance matrix is estimated from the observed residuals 
distances = mahalanobis(E, centroid, V)

cutoff <- 2.5 * mean(distances) # some literature uses the value 2 as cutoff score other uses 2.5

distances[distances > cutoff] # provides a list of the values that exceed the norm


## UNIVARIATE ANOVA
summary(car::Manova(model), univariate=TRUE, p.adjust = "holm") # holm adjustment has more power than bonferroni == better
# if the p-values are significant then there are significant differences between levels of the independent variable for the dependent variables 


## DISCRIMINANT ANALYSIS

model <- MASS::lda(dependent_variable ~ independent_variable(s),data = data)# e.g. lda(region ~ Fe+Ca,data = pottery); use for linear equations

predict(lda_fit, newdata = data) # to confirm the fit of the model
predict(lda_fit, newdata = newdata) # to apply it to new external data

### PLOTTING THE MODEL

plot(lda_fit) # seperation of the groups visualized

### CROSS-VALIDATION

cv_model <- MASS::lda(dependent_variable ~ dependent_variable(s),data = data,CV=TRUE) # use for confusion matrix 

#### Cross-validated (LOOCV) Confusion table
table(data[["dependent_variable"]], cv_model[["class"]])
mean(data[["dependent_variable"]] == cv_model[["class"]]) # accuracy

#### Alternative (not possible in exam)

library(caret)
confusionMatrix(lda_fit$class,data$dependent_variable) # confusion Matrix is only possible for categorical outcome variables








########################################################################################################################################################################################################
# Week 5: PRINCIPAL COMPONENT ANALYSIS
########################################################################################################################################################################################################

## COVARIANCE MATRIX
S <- cov(data) # creates a covariance matrix of the data

## EIGENVALUES
eigenvalues <- eigen(S) # There are as many vectors and values as there are variables
eigenvalue_1 <- eigenvalues$values[1]
eigenvector_1 <- eigenvalues$vectors[,1]

z1 <- x1[1]*variable_1 + x1[2]*variable_2 + x1[3]*variable_3
var(z1) # this has to be identical to the first eigenvalues


t(x3) %*% x1 # eigenvector * eigenvector = 0
t(x3) %*% x2 # numerically 0
var(z3)
cov(z1,z3) # numerically 0
cov(z2,z3) # numerically 0 all Principle Components are orthogonal of each other 

## PRINCIPAL COMPONENT ANALYSIS

PCA <- prcomp(data, center=TRUE, scale=F) # center is only relevant on non Covariance data., scale means ...
summary(PCA)

PCA <- princomp(data, cor = T) # for correlational data; not the covariance matrix


### PCA OUTPUT

sqrt(eigenvalue_1) # Standard deviation reported for PC1

sqrt(eigenvalue_2) # Standard deviation reported for PC2

eigenvalue_1/sum(eigenvalues) # Proportion of Variance reported for PC1 



### SCREEPLOT; Visual interpretation of the cutoff in the data
screeplot(pca, type = "lines", col = "red")

### PLOTS
biplot(pca, cex=.5) # arrow loadings are not in the direction that they are pointing but to which axes they are parallel

psych::corPlot(data) # correlation plot

### MEASURE OF APPROPRIATENESS

psych::KMO(cor(data)) #The Kaiser–Meyer–Olkin (KMO) test; > 0.6 acceptable, the higher the better   
psych::cortest.bartlett(data) # super significant -> correlations among variables which is necessary for PCA




########################################################################################################################################################################################################
# Week 6: CLUSTER ANALYSIS
########################################################################################################################################################################################################
# Researchers use clustering methods when they suspect a latent grouping in their data. 

## k means clustering

km = kmeans(data,k = 3, nstart=20) 
kmeans_TotWSS = function(k) kmeans(data, k, nstart=20)$ tot.withinss
totwss = sapply(1:40, kmeans_TotWSS)
plot(totwss, type='b', ylab="Total WSS") # obtain the total with in-group sums of squares
# [NOTE: we choose the number of components after/including the elbow.]

### TABLE OF CLUSTERING
table(model$cluster)

### NO. INTERATIONS
model$iter # = 2

### SUCCESS/CONFUSION MATRIX

table(data$to_be_clustered_variable, k.means_fit$cluster) # e.g. table(iris$Species, k.means_fit$cluster)

1 - misclassified/total # accuracy 

### HIRACHICAL CLUSTERING
#### DISTANCE NECESSARY FOR THE ALGORITHM TO WORK
d <- dist(data, method = "euclidean")

as.matrix(dist(data))[point_1,point_1] # provides the distance between point 1 and point 2

#### MODELING
hc = hclust(d, method='average') # average gives almost the same as kmeans
plot(hc)
table(cutree(hc, h=1.1000))
table(cutree(hc, k=3)) 

1 - misclassified/total # accuracy 

##### AVERAGE
model = hclust(d, method="average")
table(found = cutree(model,k = 3), truth = iris$Species)
table(found = cutree(model,k = 3), truth = km$cluster)

1 - misclassified/total # accuracy 

##### COMPLETE
model = hclust(d, method="complete")
table(found = cutree(model,k = 3), truth = iris$Species) # in the table always choose the lowest number as error
table(found = cutree(model,k = 3), truth = km$cluster)

1 - misclassified/total # accuracy 

##### SINGLE
model = hclust(d, method="single")
table(found = cutree(model, 3), truth = iris$Species)
table(found = cutree(model, 3), truth = km$cluster)

1 - misclassified/total # accuracy 


#### HIRACHICAL MODEL

hc = hclust(dist(eufood), method="complete"); 
plot(hc)

group = cutree(hc,4)
group_subsets = split(eufood, group)
statistics = lapply(group_subsets, cov.wt)

# centroids
sapply(statistics, function(obj) obj$ center)

# Within Sums of Squares
trSk = sapply(statistics, function(obj) psych::tr(obj$ cov))
nk = sapply(statistics, function(obj) obj$ n.obs)
WSSk = (nk-1) * trSk 

TotWSS = sum(WSSk) # = 58124.2
TSS = psych::tr(cov(eufood)) * (nrow(eufood)-1)

# Proportion
TotWSS / TSS # = 0.46951219










# DATA GENERATION

library(MASS)
mu_set1 = rep(0, 5)
Sigma_set1 = diag(5)*.4 + .6
mu_set2 = seq(0, 1, len=5)
Sigma_set2 = diag(5)*.5 + .5
n = 100
set.seed(9677)
Y1 = MASS::mvrnorm(n, mu_set1, Sigma_set1)
Y2 = MASS::mvrnorm(n, mu_set2, Sigma_set2)
Y3 = MASS::mvrnorm(n, rep(0,5), diag(5)*.1 + .9) # generate observations from correlated variables with a multivariate normal distribution. 
# This function needs the number of observations n, a mean vector (centroid), and a covariance matrix.

# SIMULATE CLUSTER ANALYSIS

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

cluster_data$g


km = kmeans(cluster_data,k = 3, nstart=20) 
kmeans_TotWSS = function(k) kmeans(cluster_data, k, nstart=20)$ tot.withinss
totwss = sapply(1:40, kmeans_TotWSS)
plot(totwss, type='b',pch=20, ylab="Total WSS")

values <- c(3,2,1.5,0.5,0.25,0)
for (i in values) {
  dat <- sim_clustdata(n = 50, delta = i,seed = 13707)
  km <- kmeans(x = dat[1:4],3,nstart = 20)
  print(i)
  print(table(dat$g, km$cluster))
}

### Plotting General
plot(r, t, pch = 1:25, cex = 3, yaxt = "n", xaxt = "n",
     ann = FALSE, xlim = c(3, 27), lwd = 1:3)
text(r - 1.5, t, 1:25) # pch = type of dot
