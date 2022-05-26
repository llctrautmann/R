# Guy Montag
# Assignment 1 

#Q1 Make a function of the following formula: y = sqrt(3 * x + 10). So x should be your input and y is the output.

f_01 = function(x) {
  return(sqrt(3 * x + 10))
  }

#Q2 What do you expect? Is a 3 or 5? Explain?

a <- 3
f <- function(x) {
  a <- x
  }
f(5)

# a = 3. The function only declares variables locally within the function. a remains 3 in the global environment. 

#Q3 Make an absolute function for numeric vectors that passes all tests as discussed in the tutorial

v <- c(-1,-1,1,-5,-8)
s <- c("hello","a",'hong_kong')

absolute_value <- function(x) {
  if(!is.numeric(x)) {
    stop("Please provide an integer")
  } 
  else {
    return(ifelse(x < 0, x * -1,x*1))
  }
}

# testing of assumptions 

absolute_value(5)

absolute_value(-5)

absolute_value(0)

absolute_value(v)

absolute_value(s)


#Q4 Make a function that returns TRUE when the maximum in the vector occurs before the minimum. So 3 2 5 1 gives TRUE, 5 5 5 6 FALSE. Tip: use which.min and which.max

f_04 = function(x) {
  if(which.max(x) < which.min(x)) {
    return(TRUE)
    }
  else {
    return(FALSE)
    }
}

# testing the function for multiple random samples 

for (i in 1:10) {
  x <- round(rnorm(4),1)
  print(x)
  print(f_04(x))
}

#Q5 Make a function that returns TRUE when input is an even number and FALSE if it is an uneven number. *Include an informative error, checking that the input is indeed numeric.

function_05 <- function(x) {
  if (!is.numeric(x)) {
    stop("Please provide an number.")
  }
  else if (x%%1 != 0) {
    stop("Please provide an integer.")
  }
  else if(x%%2 == 0) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

# A little test to see if the function executes for multiple numbers.

for(i in 1:10)
{
    print(i)
    print(function_05(i))
}



