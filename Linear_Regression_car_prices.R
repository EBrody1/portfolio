# Evan Brody excercise 4 Linear Regression
# load the data
cars <- read.csv("C:/Users/Yissa/OneDrive/data_UMGC/630/module_2/imports-85.csv", as.is= FALSE)
# remove variables that won't help the model
cars$fuel_system <- NULL
cars$num_of_cylinders <- NULL
cars$make <- NULL
cars$engine_type <- NULL
# check if variables were removed
str(cars)
# check if at least one variable appears linear
plot(cars$Price, cars$engine_size)
cor(cars$Price, cars$engine_size, use = "complete.obs")
# explore
# display the descriptive statistics for all variables
summary (cars)
# Number of Missing Values for Each Variable
apply(cars, 2, function (cars) sum(is.na(cars)))
# lots of na for normalized losses check if this variable correlates with price
plot(cars$Price, cars$normalized_losses)
cor(cars$Price, cars$normalized_losses, use = "complete.obs")
# not strong enough variable to justify filling in so many missing values
cars$normalized_losses <- NULL
# Display the Rows with Missing Values
cars[!complete.cases(cars),]
# more than one missing values per row so check if values for those missing correlate with engine size 
library(dplyr)
plot(summarise_at(group_by(cars,engine_size),vars(horsepower),list(mean = mean)))
plot(summarise_at(group_by(cars,engine_size),vars(peak_rpm),list(mean = mean)))
cor(cars$horsepower, cars$engine_size, use = "complete.obs")
cor(cars$peak_rpm, cars$engine_size, use = "complete.obs")
#  engine size and horsepower are strongly related
# we should fill in the na value based on engine size
#find if there are other entries for that engine size to fill in with
cars[cars$engine_size == 132,]
# there were none so we'll remove those two rows
cars<- cars[!is.na(cars$horsepower), ]
# Display the Rows with Missing Values to check what is gone
cars[!complete.cases(cars),]

# 4 rows with 2 na values per row so check if values for those missing correlate
# with engine size 
plot(summarise_at(group_by(cars,engine_size),vars(bore),list(mean = mean)))
plot(summarise_at(group_by(cars,engine_size),vars(stroke),list(mean = mean)))
cor(cars$bore, cars$engine_size, use = "complete.obs")
cor(cars$stroke, cars$engine_size, use = "complete.obs")
# bore seems pretty correlated with engine size
# lets find if there are other entries for that engine size to fill in with
cars[cars$engine_size == 70,]
# there were none so we will remove those two rows
cars <- na.omit(cars)
# check that we got all the na's 
apply(cars, 2, function (cars) sum(is.na(cars)))

# check for outliers
boxplot(cars$Price)
max(cars$Price)

# convert symboling to factor as it is levels of rankings
cars$symboling <- factor(cars$symboling)

# train test split
set.seed(1234)
ind <- sample(2, nrow(cars), replace = TRUE, prob = c(0.7, 0.3))
train.cars <- cars[ind == 1, ]
test.cars <- cars[ind == 2, ]


#  train the model
model<-lm(Price ~., data=train.cars)
print(model)
summary(model)

# test the model with test set
pred <- predict(model,  newdata=test.cars)
# graph actual data vs. predictions
plot(test.cars$Price, pred, xlab = "Observed", ylab = "Prediction")
# this is the line of when actual = predicted observe how close dots are to the line
abline(a = 0, b = 1) # add 45 degree line
# test the correlation of results 
cor(pred,test.cars$Price)

# evaluate residuals to make sure they are linear
plot(model)
cooksd <- cooks.distance(model)
summary(cooksd)
which.max(cooksd)
# remover outlier
cars <- cars[-17, ]


# remove the top  leverage point that was not plotting properly
hv <- hatvalues(model)
which.max(hv)
cars <- cars[-94, ]
# re split
set.seed(1236)
ind <- sample(2, nrow(cars), replace = TRUE, prob = c(0.7, 0.3))
train.cars <- cars[ind == 1, ]
test.cars <- cars[ind == 2, ]

#  retrain the model
model<-lm(Price ~., data=train.cars)
summary(model)
plot(model)

#  Minimal adequate model
Model2<-step(model, direction="backward")
# run that final model
summary(Model2)


# test the model with test set
pred <- predict(Model2,  newdata=test.cars)
# test the correlation
cor(pred,test.cars$Price)
# find confidence intervals to understand impact of each varaible 
confint(Model2)
