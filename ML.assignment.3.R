covid <- read.table("/Users/luca/Documents/mind/SUPPLEMENTS/DATA/RIVM_hospitalisation_2.txt",header = T)

covid <- covid[covid$Hospital_admission != "Unknown", ]
covid$Hospital_admission <- as.factor(covid$Hospital_admission)

summary(covid)
library(stats)

mod1 <- stats::glm(Hospital_admission ~ AgegroupC+Sex+Number_days+Total_cases_reported,family = binomial(),data = covid)

#'[Randomly split the data into k equally sized folds (or data subsets).]
#'[Train the model on k-1 folds and reserve the remaining fold.]
#'[Test the model on the remaining fold and compute the test accuracy.]
#'[Repeat steps 2 and 3 k times, until each fold has served as a test set.]
#'[The accuracy is then average of the prediction errors across the k folds.]

covid$hospitalized <- ifelse(covid$Hospital_admission == "Yes", 1, 0) # create a binary variable for hospitalization


set.seed(135)
k <- 5
covid <- covid[sample(nrow(covid)),] # shuffle data see sample().R in functions.explained

folds <- cut(seq(1,nrow(covid)), breaks=k, labels=FALSE) # break into k segments
acc <- numeric()

?sample

for(i in 1:k) {
  train <- covid[folds != i,]
  testing <- covid[folds == i,]
  mod2 <- stats::glm(hospitalized ~ AgegroupC+Sex+Number_days+Total_cases_reported,family = binomial(),data = train)
  predictions <- predict(mod2,newdata = testing,type = "response")
  predictions <- ifelse(predictions >= .5, 1, 0)
  acc[i] <- mean(predictions == testing$hospitalized) #'[mean is the average = accuracy stupid!]
}

mean(acc)

#'[Question 5: K-fold Cross Validation with cv.glm]

accuracy <- function(y,y_hat) {mean((y>= .5) ==(y_hat>=0.5))}

library(boot)
set.seed(135)
mod3 <- boot::cv.glm(covid,mod1,K=5,cost = accuracy)
mod3$delta
mean(mod3$delta)


#'[Question 7: K-fold Cross Validation with cv.glm]
#'
set.seed(135)
predict(mod1, newdata=covid[1:2,],type = "response")

covid$hospitalized

#'[Question 8:]

mod4 <- boot::cv.glm(covid,mod1,K=10,cost = accuracy)
mod4$delta

#'[Question 9: K-fold CV versus LOOCV ]

start_time <- Sys.time()
mod4 <- boot::cv.glm(covid,mod1,K=10,cost = accuracy)
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
mod4 <- boot::cv.glm(covid,mod1,K = k,cost = accuracy)
end_time <- Sys.time()
end_time - start_time


#'[Question 11: Confusion Matrix]
model <- stats::glm(hospitalized ~ AgegroupC+Sex+Number_days+Total_cases_reported,family = binomial(),data = train)
probs <- predict(model,testing.type = response)
predictions <- ifelse(probs >= .5, 1, 0)
table(predictions,train$hospitalized)

737529/(737529+723)

723/(723+18234)
mean(predictions == train$hospitalized)

(737529+723)/(737529+723+800+18234)
              

