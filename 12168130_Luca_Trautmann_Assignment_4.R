# Name: Guy Montag
# 

#Q1: Make this plot with the vector

x <- rep(1:5, times = 5)

plot(x,type = 'b', xlab = "Index",ylab = "")

#Q2: First make these data objects. Then make the following plot using x and y:

x <- 1:10

y <- 1 + 1.5 * x + rnorm(10, 0, 2)

y2 <- 10 - .1 * x + rnorm(10, 0, 2)

plot(x,y, main = "Random Data",xlab = "Age",ylab = "Score",col = 'red', pch = 2, ylim = c(1,19)) # pch is shape


plot(x,y, main = "Random Data",xlab = "Age",ylab = "Score",col = 'red', pch = 2, ylim = c(1,19)) # pch is shape
points(y2, col = 'blue', pch = 19)

#Q3

points(y2, col = 'blue', pch = 19)

#Q4

pdf(file = "/Users/luca/Desktop/coding/r/amsterdam./basic.math/test_plot.pdf")
plot(x,y, main = "Random Data",xlab = "Age",ylab = "Score",col = 'red', pch = 2, ylim = c(1,19), xlim = c(1,10)) # pch is shape
points(y2, col = 'blue', pch = 19)

dev.off()
#Q5

plot(x,y, main = "Random Data",xlab = "Age",ylab = "Score",col = 'red', pch = 2, ylim = c(1,19), xlim = c(1,10),bty = 'n') # pch is shape
abline(lm(y ~ x), col = 'red', lty = 3)
points(y2, col = 'blue', pch = 19)
abline(lm(y2 ~ x), col = 'blue', lty = 2, lwd = 5)

# Q6 

# layout(matrix(c(1:4,5,6,6,7,8,6,6,9,10:13),4,4))
# layout.show(13) # Use to inspect the layout.
# layout(matrix(c(1:4,5,6,6,7,8,6,6,9,10:13),4,4))#Run layout function again when you start filling in the slots
# plot(rnorm(25),main="",xlab="",ylab="") # Fill in the first slot of the grid

layout(matrix(c(1:5,6,7,7,7,8,9,7,7,7,10,11,7,7,7,12,13:17),5,5))
layout.show(17) # Use to inspect the layout.

for (i in 1:17) {
  plot(rnorm(25),main="",xlab="",ylab="") # Fill in the first slot of the grid
}

# Q7 

m <- matrix(round(rnorm(256),2),16,16)
m

layout(matrix(c(1:16),4,4))
for (i in 1:4) {
  for (d in 1:4) {
    plot(m[,i],m[,d],main = "Random Data")
  }
}

