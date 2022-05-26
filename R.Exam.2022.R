
###################################################################
########################### Final Exam ############################
###################################################################

### NAME: Guy Montag
### STUDENT NR: 
# You have 1.5 hours! Good luck!
# Possible points in this exam: 15 points


############################ Stats Part ###########################


### 1: Confidence Intervals (2 points)
# Means or Medians? Make some R code in which you
# take a sample of n=20 from a normal density of  with mu=0.3 en sigma=1. 
# Do this a 1000 times. In each run calculate the mean and the median of the 20 values.
means <- numeric(1000L)
medians <- numeric(1000L)


for(i in 1:1000) {
  x <- rnorm(20, 0.3,1)
  print(x)
  means[i] <- mean(x)
  medians[i] <- median(x)
}

#### A (1 point)
# Use your simulated values to calculate the average and the SD of the two vectors 

mean.means <- mean(means)
sd.means <- sd(means)

mean.medians <- mean(medians)
sd.medians <- sd(medians)

# (that contain your simulated means and of the medians). Based on these values 
# calculate the two 95\% confidence intervals. What statistic results in the smallest interval?

CI.means.upper <- mean.means + 1.96*(sd.means/sqrt(length(means)))
CI.means.lower <- mean.means - 1.96*(sd.means/sqrt(length(means)))

CI.medians.upper <- mean.medians + 1.96*(sd.medians/sqrt(length(medians)))
CI.medians.lower <- mean.medians - 1.96*(sd.medians/sqrt(length(medians)))

print(CI.means.upper)
print(CI.means.lower)

print(CI.medians.upper)
print(CI.medians.lower)

CI.means.upper-CI.means.lower

CI.medians.upper-CI.medians.lower

# The means result in a smaller intervals than the medians.

#### B (1 point)
# Based on these numbers and your statistical intuition, which statistic would 
# you pick if you would want to reject H_0: mu=0? The means or the medians?

# Based on the statistics and my common understanding I would use the mean to to reject H_0: mu=0?



### 2: Regression (3 points)
# Inspect the code below:
set.seed(10)
p <- numeric(100)
for(i in 1:100) {
  x <- rnorm(20, 2, 2)
  y <- 3 + .15 * x + rnorm(20)
  mod <- lm(y ~ x)
  p[i] <- coefficients(summary(mod))[2,4]  
}
table(p < .05)

#### A (1 point)
# Carefully study the code. Explain why `table(p < .05)' represents the power 
# of the regression test.

# Power represent the amount of experiments in which the null gets rejected successfully
# and the Null is false. Therefore the proportion of models with  significant coefficient (slope)
# is the power of the experiment. p[i] is a vector with the coefficients of 100 models. If 
# there is a true linear relationship between y and x the coefficient should be significant
# therefore the power is proportion of models with a significant coefficient. table(p < .05)
# gives this relationship in absolute terms but it still represents the power. 


#### B (1 point)
# The p-values refer to the significance level of the slope parameter in the 
# regression model (b_1=.15). What is the null-hypothesis (H0) for the slope
# parameter? And, is the H0 in this case true? Explain.

# The null hypothesis of this regression model states that the coefficient $\beta$ is equal to zero (i.e there is no effect of x on y).
# if the p-value is low there is likely no meaningful addition to your model because changes in the predictors do not lead to changes 
# in the response variable. 

# H0 is never true within the frequentest framework, as it is technically not possible to find evidence for H0 with this setup. In the 
# simulation above it is only possible to find evidence against the null. Based on the simulation in roughly 3/4 of the cases it was not 
# possible to reject the null. 


#### C (1 point)
# Show, using the R-code above, how we can increase the probability of
# rejecting the H0, if the H0 is false.

set.seed(10)
p <- numeric(100)

for(i in 1:100) {
  x <- rnorm(20, 2, 2)
  y <- 3 + .15 * 2x + rnorm(20)
  mod <- lm(y ~ x)
  p[i] <- coefficients(summary(mod))[2,4]  
}

table(p < .05)

# incresing the values of y by a factor of 2 ( 2x in y <- 3 + .15 * 2x + rnorm(20)) will increase the power substantially.


############################ Programming Part ###########################

### 1: Matrix (4 points)

#### A (1 point)
# Make the following matrix without typing it out:
#       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
# [1,]    5    6    7    8    9   10   11   12   13    14
# [2,]   15   16   17   18   19   20   21   22   23    24
# [3,]   25   26   27   28   29   30   31   32   33    34
# [4,]   35   36   37   38   39   40   41   42   43    44
# [5,]   45   46   47   48   49   50   51   52   53    54
# [6,]   55   56   57   58   59   60   61   62   63    64
# [7,]   65   66   67   68   69   70   71   72   73    74
# [8,]   75   76   77   78   79   80   81   82   83    84
# [9,]   85   86   87   88   89   90   91   92   93    94
# [10,]   95   96   97   98   99  100  101  102  103   104


myMatrix <- matrix(5:104,10,10,byrow = T)



#### B (1 point)
# Select all odd rows, without simply writing out the odd numbers.

myMatrix[seq(1,10,2),]



#### C (1 point)
# Calculate the median of each column of the whole matrix using an explicit loop. 

for(i in 1:10) {
  print(median(myMatrix[,i]))
}


#### D (1 point)
# Do the same with an implicit loop.

apply(myMatrix,2,median)


### 2: Response Time Analysis (3 points)

#### A (1 point)
# Read in the data file BasicSkillsExam2022.txt and store the data in 
# an R object called 'dat'.

dat <- read.table("/Users/luca/Downloads/BasicSkillsExam2022.txt",header = T)

dat  <- dat[!rowSums(is.na(dat[,])),]

dim(dat) # check if the removal of NA was successful

#### B (1 point)
# What is the mean response time (RT) for each of the conditions? 
# If you did not succeed in the previous question, you can use the code below to generate the data set:



tapply(dat$RT, list(dat$condition), mean)

# set.seed(167)
# dat <- data.frame(pp = rep(1:40), 
#                   condition = rep(letters[1:5], each = 8), 
#                   age = rep(sample(18:40, 10, T), each = 4), 
#                   RT  = rlnorm(40, .15, .4), 
#                   ACC = sample(0:1, 40, T))
# naPP <- sample(1:40, 6, FALSE)
# dat$ACC[naPP] <- dat$RT[naPP] <- NA

  


#### C (1 point)
# Reproduce the histogram of the response times in the exam pdf, and write the plot 
# to a .pdf you do not need to include the pdf when you hand in your answers). 
# Note that your results might differ due to sampling variance -- be sure to have
# the same axis limits and graphical parameters. 
# N.B. the histogram contains colors, please see the exam pdf for the color version.

pdf(file = "~/Desktop/my.beautiful.histogram.pdf")

hist(x = dat$RT,freq = F,bty='n',ylim=c(0.0,1.2),xlim=c(0.0,3.0),xlab = "Response Time (sec)",main="My Beautiful Histogram",breaks = 15, col = c('purple','green'))

dev.off()


### 3: Functions (3 points)

#### A (1 point)
# Write a function that takes in a data frame with a single variable and returns
# a list with the mean and the median of that variable.  

x1 <- rnorm(20)

dat2 <- data.frame(x1)

converter <- function(x) {
  x <- apply(dat2,2, mean)
  y  <- apply(dat2,2, median)
  out <- list(x,y)
  return(out)
}

converter(dat2)

#### B (1 point)
# Adjust your function such that, if the data frame has more than 1 variable, 
# the function computes all correlations between the variables, and returns the
# highest correlation.

converter_adjusted <- function(x) {
  if(ncol(x) == 1) {
    x <- apply(dat2,2, mean)
    y  <- apply(dat2,2, median)
    out <- list(x,y)
    return(out)} 
  else {
    x <- cor(dat3)
    diag(x) <- 0
    print(x)
    return(max(x))
  }
}

#### C (1 point)
# Make two data frames: 
#   (1) a data frame with 1 numeric variable 
#   (2) a data frame with 5 numeric variables. 
# You can simulate these variables from the standard normal distribution. 
# Apply your function to both of these data frames.

dat2 <- data.frame(x1)
dat3 <- data.frame(replicate(5,rnorm(5)))


converter_adjusted(dat2)
converter_adjusted(dat3)

###################################################
#################     THE END     #################
###################################################
