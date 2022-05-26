#Q1 Create if statement for the following expressions if a > 2 then x <- else x <- -1
a <- -5

if(a > 2) {
  x <- 1
} else {x <- -1}

x

#Q2 Lets include a second statement. Make an if statement for the following: 
# if a > 2 then x <- 1 if not, then if a < 0, then x <- 0
# else x <- -1 


print(if(a > 2) {
  x <- 1
  } else {
    if (a < 0) {
      x <- 0
      } else {
        x <- -1}
  })



#Q3 Change NAs to 0 in the vector: 

v3<- c(2,3,NA,1,2,NA,4)

which(is.na(v3))
 
v3[is.na(v3)] <- 0

v3


#Q4 What is wrong with this expression? 

# a <- 2 
# if (a = 3) {
#  a <- 4 
#}

# = and == error. [=] assigns while [==] is the logical equivalent operator 
# necessary to make the function work


#Q5 Change the code below such that v = c(1,2,3,4,5)

v <- numeric()

for (i in 1:5) {
  v[i] <- i
}

v

#Q6 Make the following vector with a for loop 1:10*4. Start with an empty numeric vector 
# and fill this vector with the for loop

x <- numeric()

for (i in 1:40) {
  if(i %% 4 == 0) {
    x <- c(x, i)
    }
  }

x

#Q7 Would like to know the median in each row of matrix M. I can do that using an explicit
# loop. What is the difference between method 1 and 2? 

M <- matrix(sample(1:100),10,10)

# Method 1 

for (i in 1:10) {
  print(median(M[i, ]))
  }

# Method 2

medians <- numeric()

for (i in 1:10) {# method 2
  medians[i] <- (median(M[i, ]))
}

# The main difference is that method 1 does not assign the results of the for loop to any variable.
# therefore the output cannot be used later for future analyses. 

# Q8) How could I get the same result using implicit looping (see lecture)? 

medians_2 <- apply(M, 1, median)

medians == medians_2 # check equivalence
  
# Q9) max(x) where x is a matrix returns one number. How can we get the max of each column? Use implicit looping. 


x <- matrix(1:9,3,3)
apply(x,MARGIN = 2, FUN = max)


# Q10 read the file Q11.txt into R (include the header). What are the mean pre-scores for group a and b? What are the mean post-scores 
# for male and females younger than 29, in both groups a and b? 
  
q10_data <- read.table("Q10-1.txt", header = T)

tapply(q10_data$prescore, q10_data$group, mean)

q10_data_2 <- subset(q10_data, q10_data$age < 29)


q10_subscore_l <- split(q10_data_2, q10_data_2$group)

df_group_a <- q10_subscore_l$a

df_group_b <- q10_subscore_l$b

tapply(df_group_a$postscore, df_group_a$sex, mean)

tapply(df_group_b$postscore, df_group_b$sex, mean)




# Q11 Make a function that returns the sum or the max of a vector depending on how the user specifies the argument in the function.


x <- sample(1:10,10)


f_11 <- function(x, sum = T) {
  if (sum == T) {
    return(sum(x))
  } else {
    return(max(x))}
}  

x

f_11(x,sum = T)

# Q12 Make a for loop that finds the index of the maximum of a vector (see which.max) Don’t use which.max() or Max

v <- sample(1:15,10,replace = T)

find_max <- function(x) {
  counter <- 1 # there will always be one highest number selected
  baseline <- -Inf
  for(i in 1:length(x)) {
    ifelse((x[i] > baseline),baseline <- x[i],next)
  }
  output <- match(baseline,x)
  return(output)
}

which.max(v)
find_max(v)


# Q13: Read the soccer data file into R. What team was the champion of the season? Answer: there are more then 100 solutions to this. 


football_data <- read.table("data_soccer.txt",header = T,sep = ";")

points_home <- numeric()

points_away <- numeric()

Clubs <- sort(unique(football_data$HomeClub))


# separate the data frame into data frames for each club

Data_list <- split(football_data, football_data$HomeClub)
Data_list_away <- split(football_data, football_data$VisitingClub)

# calculate the final scores for each team 
# assign point for each game and calculate final points tally

Final_Scores_Function <- function(x, away = FALSE) {
  if(away == FALSE) {
    Game_Scores <- x[["HomeFinalScore"]] - x[["RoadFinalScore"]]
    achieved_points <- sum(ifelse(Game_Scores > 0,3,ifelse(Game_Scores == 0,1,0)))
    points_home <<- c(points_home,achieved_points)
    # return(achieved_points)
} else if(away == TRUE) {
    Game_Scores <- x[["HomeFinalScore"]] - x[["RoadFinalScore"]]
    achieved_points <- sum(ifelse(Game_Scores > 0,0,ifelse(Game_Scores == 0,1,3)))
    points_away <<- c(points_away,achieved_points)
    # return(achieved_points)
    }
}

# Results
# Points Home Games
invisible(lapply(Data_list, Final_Scores_Function))

# Points Away Games
invisible(lapply(Data_list_away, Final_Scores_Function, away = T))

standings <- data.frame(Clubs,points_home,points_away)
standings$points_total <- standings$points_home + standings$points_away

standings[order(standings$points_total,decreasing = T),] # Note: don't forget the comma... takes ages to find this.

# Manchester United won the Premier League. Wigan, Charlton and Watford were relegated.
