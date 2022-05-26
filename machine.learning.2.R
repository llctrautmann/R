bike_sharing <- read.csv("/Users/luca/Desktop/Bike-Sharing-Dataset/day.csv",header = T)

drop <- c("casual","registered","dteday","instant")
bike_sharing <- bike_sharing[,!(names(bike_sharing) %in% drop)]

# simple solution 
set.seed(135)
sample <- sample(1:731,584,replace = F)

training_set <- bike_sharing[sample,]
testing_set <- bike_sharing[-sample,]

## Question 6: 

model <- lm(cnt ~ season+yr+mnth+holiday+weekday+workingday+weathersit+temp+atemp+hum+windspeed, data = training_set)
summary(model)

## Question 7
## Getting R^2 in the Data
new_predictions <- predict(model,newdata = testing_set)
correlation <- cor(testing_set$cnt,new_predictions)
r.squared <- correlation^2

# Question 8
fitted_value <- NULL
for(i in 1:nrow(bike_sharing)) {
  sample <- i
  train <- bike_sharing[-sample,]
  test <- bike_sharing[sample,]
  model <- lm(cnt ~ season+yr+mnth+holiday+weekday+workingday+weathersit+temp+atemp+hum+windspeed, data = train)
  fitted_value[i] <- predict(model,newdata = test)
}

# Question 9
x <- sum(fitted_value - bike_sharing$cnt)
x^2

# Question 10
model_total <- lm(cnt ~ season+yr+mnth+holiday+weekday+workingday+weathersit+temp+atemp+hum+windspeed, data = bike_sharing)

PRESS = sum(rstandard(model_total, type="pred")^2)

# question 11
RMSE <- sqrt(PRESS/(730-1))

sqrt(mean((bike_sharing$cnt- fitted_value)^2))

# Question 12 
summary(model_total)


# Question 13: 

library(MASS)
model5 <- lm(cnt ~ 1,data = bike_sharing)
stepwise_model <- MASS::stepAIC(model5,cnt ~ season+yr+mnth+holiday+weekday+workingday+weathersit+temp+atemp+hum+windspeed,direction = 'forward')

summary(stepwise_model)

stepwise_model$coefficients



# Question 15: 
## Forward
seoul_data <- read.table("/Users/luca/Desktop/BikeSharingSeoul.txt",header = T,sep = " ")
model_forward <- lm(rnt ~ 1, data = seoul_data)
seoul_forward <- MASS::stepAIC(model_forward,rnt ~ mnth+workingday+weekday+temp+hum+wind_speed+visibility+dew_temp+solar_rad+rainfall+snowfall,direction = 'forward')
summary(seoul_forward)

### best model formula: rnt ~ temp + solar_rad + rainfall + mnth + wind_speed; AIC: 4032.12

## Backward
model_backward <- lm(rnt ~ mnth+workingday+weekday+temp+hum+wind_speed+visibility+dew_temp+solar_rad+rainfall+snowfall,data = seoul_data)
seoul_backward <- MASS::stepAIC(model_backward,rnt ~ mnth+workingday+weekday+temp+hum+wind_speed+visibility+dew_temp+solar_rad+rainfall+snowfall,direction = 'backward')
summary(seoul_backward)

# best model: rnt ~ mnth + hum + wind_speed + dew_temp + solar_rad + rainfall; AIC: 4032.92

model.matrix(cnt ~ season+yr+mnth+holiday+weekday+workingday+weathersit+temp+atemp+hum+windspeed, data = bike_sharing)
