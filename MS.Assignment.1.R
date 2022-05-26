options(digits = 22)
set.seed(12345)
runif(1, 1, 10)

R.Version()

RNGversion(3.6); set.seed(1975); Coins <- rbinom(n=1000, size= 1, prob = 0.6)
mean(Coins)

RNGversion(3.6); set.seed(1975); Coins25 <- rbinom(n= 25 , size= 1, prob = 0.6)
RNGversion(3.6); set.seed(1975); Coins500 <- rbinom(n= 500 , size= 1, prob = 0.6)
RNGversion(3.6); set.seed(1975); Coins1050 <- rbinom(n= 1050 , size= 1, prob = 0.6)

l <- list(Coins25,Coins500,Coins1050)
for(i in 1:3) {
  print(t.test(l[[i]],mu = 0.5))
}

sqrt(var(Coins25))

## Question 9
cohensd25 <- (mean(Coins25)-0.5)/sqrt(0.25)
cohensd500 <- (mean(Coins500)-0.5)/sqrt(0.25)
cohensd1050 <- (mean(Coins1050)-0.5)/sqrt(0.25)

round(cohensd1050,5)
a = (sqrt(2))^2
b = 2

## Question XX

all.equal(a,b)

a == b 

S = ability.cov[["cov"]]
R <- cov2cor(S)

R[6, 5] 

round(0.7913779,5)

## Question 16: 

A <- matrix(c(2,2,4,5),2,2,byrow = T)
b <- matrix(c(-3,3),2,1)

solve(A,b)

## Question 17

A <- matrix(c(-6,-7,-9,-7,4,-4,8,5,2),3,3,byrow = T)
b <- matrix(c(83,23,-61),3,1)
round(solve(A,b),5)

## Question 18
ability.cov$cov

library(psych)
tr(ability.cov$cov)

tr(S)
tr(R)

## Question 19

i = c(2,4,6);
S = ability.cov[["cov"]][i,i]

R <- cov2cor(S)

det(S)
det(R)

## Question 20:

RNGversion(3.6);
set.seed(3259);
B = matrix(rnorm(25), 5)

solve(B)

r <- function(x,y) {
  a <- round(x,y)
  return(a)
}

r(0.200329351,5)

## Question 21
RNGversion(3.6); set.seed(3727); A = svd(matrix(runif(10*5),,5))$v

det(A)

solve(A)
t(A)
all.equal(solve(A),t(A))

## Question 22
RNGversion(3.6); set.seed(3754); X = MASS::mvrnorm(100, mu = c(0,0,0,0,0), Sigma = ability.cov$cov[-1,-1]); A = cov(X)
eigen(A)
z <- eigen(A)$value
z2 <- eigen(A)$vectors

z3 <- z2[,1]

A%*%z3

265.981411%*%z3






## Question 23
A = matrix(c(3,2,2,1,3,3,2,4,3,1,1,1,2,3,5,2,2, 2,1,1,2,6,1,2,3,1,2,1,7,6,3,1,2,2,6,8),6)
x = c(32861/541996, 266452/561921, 2804/5423, 3029/6317, -241/634, -30321/84188)
z <- eigen(A)$value

for(i in z){
  print(all.equal(A %*% x,i%*%x))
}

all.equal(A %*% x,z[i]*x)

z[2]%*%x

