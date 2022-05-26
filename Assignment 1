# Guy Montag
# 
# Assignment 1 

#Q1 

a <- b <- ab <- ba <- 1

#The command assigns the same value (1) to four different variables within the environment. 
# There are now four objects in the workspace.

ls()

# lists all the objects in the workspace

 

#Q2 Save your workspace image and close R-studio. Check whether your objects exist
# when you start R-Studio again. How can you remove those objects from your workspace
# image?

# rm(list=ls()) # removes all objects from the workspace use: 

# rm(a) # removes the object a from the workspace

#Q3

set.seed(1)
x <- sample(1:10)

# selecting the first three numbers
x[1:3]

# finding the index of the highest number in the vector x
which.max(x)

#Q4

## %% is the modular operator. It takes a number x divides it by y and outputs the remainder of the division.
## ?%% doesn't work because %% is an operator and R requires the formal calling of the operator formally with
# quotes [''] or [""]

?'%%' 

# opens the help for arithmetic operators.

#Q5

# The issue about the statements 
# a <- b <- 3 
# a*B
# is that R is case sensitive and therefore B is not defined/assigned.  

#Q6

# a <- c(1,2); a[1] + 10 # Statement 1 gives 11
# a <- c(1,"hello"); a[1] + 10 # Statement 2 gives? Why?
# In the second case not all values in the vector are numerical. R therefore coerces the vector to 
# be of the type character. Which doesn't allow arithmetic operations. 


#Q7


data("women")
force(women)
 
cor(women$height,women$weight)


#Q8

vector_08 <-seq(23,28,1)

# or

vector_08 <- (23:28)

#Q9

vector_09 <-seq(10,-10,-1)

length(vector_09)


#Q10

vector_10 = seq(0,-100,-10)


#Q11

vector_11 = rep(1:3,10)

#Q12

vector_12 <- 1:50
sum(vector_12)
prod(vector_12)

#Q13
vector_13 = seq(4,40,4)


#Q14

# The two statements are equivalent because "decreasing = FAlSE" is an optional argument that doesn't add anything here. 

#Q15

matrix_15 = matrix(c(seq(1,100)),ncol = 10, nrow = 10)

print(matrix_15)

#Q16
vector_16 = c(1:10) 

matrix(vector_16, ncol = 2, byrow = TRUE)

# another option is: 

vec_16.1 <- subset(vector_16, subset = vector_16 %% 2 == T)
vec_16.2 <- subset(vector_16, subset = vector_16 %% 2 == F)

cbind(vec_16.1,vec_16.2)

#Q17
 
subject = 1:10
sex = rep(c("m","f"),5)
set.seed(1)
score = round(rnorm(10,mean = 1,sd = 3),1)

data_17 <- data.frame(subject,sex,score)

print(data_17)

#Q18
dat_18 <- data.frame(x=rnorm(100), y=rnorm(100))
mod_18 <- lm(y ~ x, data=dat_18)

# 1. Mod_18 is a list. 
mode(mod_18)

# 2. Reveals all elements in the list in the console
ls(mod_18) 
# "assign" "call" "coefficients" "df.residual" "effects" "fitted.values" "model" "qr" "rank" "residuals" "terms" "xlevels"  

# 3. To reveal a element from the list it has to be called. 

mod_18[1]


#Q19 

sqrt(15129) # 123

# sqrt(-5) 
# In sqrt(-5) : NaNs produced

# it is not possible to get the square root of a negative number. R therefore throws an error.

#Q20
# It is. 

12 %% 4 == 0

#Q21

v <- c(1:5, 5:1)

# alternative
vector_21.1 <- 1:5
vector_21.2 <- 5:1

v <- c(vector_21.1,vector_21.2)

#Q22

h <- 1:20
j <- rep(c(1,2),5)

sum(h,j)

#Q23

data_23 <- read.table(file = "data_soccer.txt", sep = ';', header = TRUE)

score <- data_23[["HomeFinalScore"]] - data_23[["RoadFinalScore"]] 

# subset function

sub_23 <- subset(data_23, subset = score > 0)

HomeClubs <- sub_23[["HomeClub"]]

HomeClubs

# indexing function

data_23[data_23$HomeFinalScore - data_23$RoadFinalScore > 0,][["HomeClub"]]

#Q24

l <- list(student_number = 12168130,name = "Luca Trautmann",combo = NULL)
l[3] <- paste(l[1],l[2])
