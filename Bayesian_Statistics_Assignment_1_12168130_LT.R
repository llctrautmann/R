# Bayesian Statistics Assignment 1
# Luca Trautmann
# 12168130

##########################################################################################################################################################
# Question 1
##########################################################################################################################################################
## Exercise 1a: Rescue Rate

## p(Rescued | 3rd Class ) = p(Rescued, 3rd Class)/p(Rescued)

### JOINT PROBABILITY

joint_probability <- 178.54/2201

### MARGINAL PROBABLITY

marginal_probability_3rd <- 706/2201


p_Rescued_3rd_Class <- joint_probability/marginal_probability_3rd


# p(Rescued | 3rd Class) is equal to 0.252889518413598, which rounds to 25,3 %


## 1b: Number of Souls lost at sea

lost_at_sea <- 1490*0.354

print(lost_at_sea)

## 527 people were lost at sea during the titanic catastrophy.


# 1c: Dependency of the survival rate: 

# p(lost at sea |3rd) == p(lost at sea)

0.354 # p(lost at sea AND 3rd) joint probability

conditional_Lost_At_sea_3rd_class <- 0.354/marginal_probability_lost_at_sea

marginal_probability_lost_at_sea <- 1490/2201 

# The marginal probability is different to the conditional probability, therefore knowing something about the passenger
# class adds information and therefore the events are not independent.



##########################################################################################################################################################
# Question 2
##########################################################################################################################################################

## Exercise 2a: Get Midpoints 
# BUILD GET.MIDPOINTS 

get.midpoints <- function(lb, ub, n) { # Add your solution here
  
  midpoints = numeric()
  distance <- ub - lb 
  segment <- distance/n
  lower_bound = lb 
  
  for(i in 1:n) {
    midpoints[i] <- mean(c(lower_bound, lower_bound + segment))
    lower_bound <- lower_bound + segment
  }

  return(midpoints)
}

get.midpoints(0.5,2.5,2)


## Exercise 2b: 
# BUILD GET.HEIGHS

get.heights <- function(f, lb, ub, n) { 
  midpoints <- get.midpoints(lb, ub, n)
  heights <- f(midpoints)

return(heights)

}

f = function(x) {x^2}

get.heights(f, lb = 0.5, ub = 2.5, n = 2)


## Exercise 2c: 
# BUILD MY.INTEGRATION 

my.integrate <- function(f, lb, ub, n) {
  heights <- get.heights(f, lb, ub, n)
  
  segment <- (ub - lb) /n
  
  areas <- heights * segment
  
  integral <- sum(areas)
  
  return(integral)
}

## Exercise 2d: 
## VERIFICATION  

options(digits = 15)

f = function(x) {x}
my.integrate(f, 0.5, 5.5,4000000)
integrate(f,0.5,5.5)



##########################################################################################################################################################
# Question 3
##########################################################################################################################################################
## Question 3a: 


fn <- function(t) {alpha * t^(alpha - 1)*exp(1)^(-t^alpha)}

alpha <- 100

integrate(f = fn, 0, Inf)


# Exercise 3B to 3D:
# CREATING THE FUNCTION 

fn <- function(t) {alpha * t^(alpha - 1)*exp(1)^(-t^alpha)}


fn2 <- function(t) {t*fn(t)} # gives you the means

alpha <- 1
a <- integrate(fn2, 0, Inf)[1]


expected_values <- numeric(20L)

# ADDING THE EXPECTED VALUES FROM THE INTEGRATION

for(i in 1:20) {
  fn <- function(t) {i * t^(i - 1)*exp(1)^(-t^i)}
  fn2 <- function(t) {t*fn(t)}
  expected_values[i] <- integrate(fn2, 0, Inf)[[1]]
}

# CALCULATING THE GAMMA VALUES

p <- numeric()
for(i in 1:20) {
  p[i] <- gamma(1+1/i)
}


# PLOTTING THE EXPECTED VALUES BOTH CALCULATED WITH THE GAMMA FUNCTION AND INTEGRATION

plot(expected_values,pch = 5, col = "red")
points(p, pch = 2, col = 'blue',cex = 0.5)
legend(10, 1, legend=c("Integration", "Gamma Function"),
       col=c("red", "blue"), lty=1:2, cex=0.8)



# Exercise 3d - 3f 

# Exercise 3d
alpha <- 2.5 
fn <- function(t) {alpha * t^(alpha - 1)*exp(1)^(-t^alpha)}

bug_in_first_minute <- integrate(fn,0,1)[[1]]

# The probability to make an error in the first minute is 0.6321205 



# Exercise 3e 

bug_in_second_minute <- integrate(f = fn,1,2)[[1]] # The probability of making a mistake in the second minute implies no bug in the first minute 
# therefore the probability of introducing a bug in the second minute should be 0.36438595, rounded 36,439 percent.


# Exercise 3f 
# Answer: 99.05037%
no_bug_in_first_minutes <- 1 - bug_in_first_minute # P(first_min_free), the marginal probability of writing bug-free R code during the fist minute

bug_in_second_minute <- integrate(f = fn, lower = 1, upper = 2)[[1]] # I am implying here that this is the joint probability equal to p(no_bug_first_minute AND bug_second_minute)

bug_in_second_given_no_bug_first <- bug_in_second_minute / no_bug_in_first_minutes

# Answer: 99.05037 This answer assume however that the events are independent of each other and that therefore 
# the answer to 3e can be seen as the joint probability. 














 # 1 - area of p(error in first minute)

 # create new function that is normalized to have area of 1 
