gaf.data <- structure(list(id = factor(1:10), gaf = c(25, 25, 0, 43, 18, 35,51, 20, 16, 47),
                           age = c(21, 19, 18, 30, 27, 39, 36,20, 23, 35),
                           pretherapy = c(52, 61, 50, 57, 54, 55,61, 52, 51, 55), 
                           n_therapy = c(6, 17, 5, 31, 10, 27,11, 7, 8, 10)),
                      .Names = c("id", "gaf", "age", "pretherapy","n_therapy"),
                      class = "data.frame", 
                      row.names = c(NA, -10))

summary(gaf.data)

pairs(gaf.data)

## Question 2: 

mod1 <- lm(gaf~age,data = gaf.data)

summary(mod1)


## Question 3: 

### They do the same job: 
ypred <- 1.63*gaf.data$age-15.68
predict(object = mod1)

### They do the same job: 
error <- gaf.data$gaf - ypred
mod1$residuals
resid(mod1)

var(gaf.data$gaf)

### ANOVA

anova(mod1)


## Variance of Residuals and Predicted Values

var.predicted.values <- var(predict(object = mod1))
var.residuals <- var(resid(object = mod1))

total.variance <- sum(var.predicted.values,var.residuals)

var(gaf.data$gaf)

variance.ratio <- var.predicted.values/total.variance

## Question 4: 
### 4.1 Create a simple regression model where you predict gaf from pretherapy
mod2 <- lm(gaf ~ pretherapy,data = gaf.data)
### 4.2 Subtract the predicted values from the observed gaf; store the resulting values in gaf_corrected
gaf_corrected <- gaf.data$gaf - predict(mod2)

### check with resid()
resid(mod2)

### Do the same for age: 
mod3 <- lm(age ~ pretherapy,data = gaf.data)

### 4.3 same thing 
age_corrected <- gaf_corrected <- gaf.data$age - predict(mod3)
age_corrected
#### check 
resid(mod3)

### 4.4 Finally, predict gaf_corrected from age_corrected
mod4 <- lm(gaf_corrected~age_corrected)
summary(mod4)


# Pretherapy

model_gaf_pretherpy  <- lm(gaf ~ pretherapy,data = gaf.data)

gaf_corrected <- gaf.data$gaf - predict(model_gaf_pretherpy)

## resid(model_gaf_pretherpy) easier

# Age
model_age_pretherapy <- lm(age ~ pretherapy, data = gaf.data)

age_corrected <- gaf.data$age - predict(model_age_pretherapy) 

# Corrected Model: 

corrected_model <- lm(gaf_corrected ~ age_corrected)


# Multiple Regression Model

age_pretherapy_MRM <- lm(gaf ~ age + pretherapy,data = gaf.data)

## ROLES SWAPPED: 

mod1 <- lm(pretherapy ~ gaf, data = gaf.data)

pretherapy_cor <- resid(mod1)


mod2 <- lm(age ~ gaf,data = gaf.data)

corr2 <- resid(mod2)

Model3 <- lm(pretherapy_cor ~ corr2)


# Question 5: 
fit = lm(gaf ~ age + pretherapy, gaf.data)
X = model.matrix(fit)

M <- solve(t(X) %*% X) %*% t(X)

M[2,2]

# Dependent Variable
y = gaf.data$gaf

M %*% y

coef(fit)

## Question 6
n = 50 # the number of observations we will generate

# Example:
set.seed(65386)
x1 = runif(n, -1, 1)
x2 = sample(-3:3, n, replace=TRUE) # see help(sample) 
x3 = rchisq(n, 3) - 3
e = rnorm(n,0,1.2)
y = x1 + x2 + e

# Question Model II:
set.seed(65386)
x1 = runif(n, -1, 1)
x2 = sample(-3:3, n, replace=TRUE) # see help(sample) 
x3 = rchisq(n, 3) - 3
e = rnorm(n,0,2.5)
y = -1.5 + 2*x1 - 0.75*x2 + 0.5*x3 + e

# Question 6
n = 50 # the number of observations we will generate

set.seed(65386)
x1 = runif(n, -1, 1)
x2 = sample(-3:3, n, replace=TRUE) # see help(sample) 
x3 = rchisq(n, 3) - 3
e = rnorm(n,0,2.5)
y = -1.5 + 2*x1 - 0.75*x2 + 0.5*x3 + e

y[1]

mod_q6 <- lm(y ~ x1+x2+x3)
summary(mod_q6)

# Question 7; 

n = 200 # the number of observations we will generate

set.seed(65386)
x1 = runif(n, -1, 1)
x2 = sample(-3:3, n, replace=TRUE) # see help(sample) 
x3 = rchisq(n, 3) - 3
e = rnorm(n,0,2.5)
y = -1.5 + 2*x1 - 0.75*x2 + 0.5*x3 + e

model_q7 <- lm(y ~ x1 + x2 + x3)
summary(model_q7)


# Question 8:

n = 800 # the number of observations we will generate

set.seed(65386)
x1 = runif(n, -1, 1)
x2 = sample(-3:3, n, replace=TRUE) # see help(sample) 
x3 = rchisq(n, 3) - 3
e = rnorm(n,0,2.5)
y = -1.5 + 2*x1 - 0.75*x2 + 0.5*x3 + e

model_q8 <- lm(y ~ x1 + x2 + x3)

summary(model_q8)

#'[Question 9]: 


morrison = structure(list(INSTEVAL = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4 ), CLARITY = c(1, 2, 1, 1, 1, 2, 3, 3, 2, 2, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4, 4, 3, 3, 4, 4, 3, 4, 4, 3, 3, 5, 4), STIMUL = c(2, 2, 1, 2, 3, 4, 3, 4, 3, 2, 3, 2, 2, 4, 3, 4, 2, 4, 3, 3, 3, 4, 4, 3, 5, 5, 4, 4, 3, 5, 5, 5), KNOWLEDG = c(1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 2, 3, 1, 2, 1, 1, 1, 3, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 2, 1, 2, 2), INTEREST = c(1, 1, 1, 1, 2, 1, 1, 2, 3, 2, 1, 3, 1, 2, 1, 1, 1, 2, 1, 2, 2, 3, 3, 1, 1, 2, 2, 1, 1, 1, 3, 3 ), COUEVAL = c(2, 1, 2, 2, 2, 2, 2, 3, 3, 2, 2, 2, 2, 2, 3, 2, 2, 2, 4, 3, 3, 3, 3, 2, 3, 3, 3, 3, 3, 2, 4, 4)), class = "data.frame", row.names = c(NA, -32L), codepage = 65001L)

#'[Question 10]
#'[Run an regression with INSTEVAL as depedent variable and the other as predictors]:

mod9 <- lm(INSTEVAL ~ CLARITY+STIMUL+KNOWLEDG+INTEREST+COUEVAL, data = morrison)

summary(mod9)

#'[Question 11: R and R^2]

predict(mod9)

#'[ correlation between the predicted and the observed values of the model]

cor(morrison$INSTEVAL,predict(mod9))

cor(morrison$INSTEVAL,predict(mod9))^2
#'[Question 12]

#'[Question 13]

plot(mod9)

install.packages("lmtest")

lmtest::resettest(mod9,type = "regressor",data = morrison)

install.packages("car")

car::?ncvTest(mod9)

shapiro.test(mod9$residuals)

#'[Question 14]

car::vif(mod9)

#'[Question 15]
#'[The ANOVA table produced by anova() is in fact a version of hierarchical regression,] 
#'[in which predictors are added sequentially and the increase in R2 is tested.]
#'[The order of predictor entry is the order in the model formula.]

anova(lm(INSTEVAL ~ CLARITY + STIMUL + INTEREST, morrison))
anova(lm(INSTEVAL ~ CLARITY + INTEREST + STIMUL, morrison))

#'[Example]
fit0 <- lm(INSTEVAL ~ CLARITY + INTEREST + STIMUL, morrison)
drop1(fit0, test = "F")

drop1(mod9,test = "F")

#'[Question 16]

plot(mod9, which = 5)
mean_yhat <- 2*mean(hatvalues(mod9))

abline(v = mean_yhat)
abline(h = 2)
abline(h = -2)

#'[Question 16]
MASS::stepAIC(mod9,direction = "both")
