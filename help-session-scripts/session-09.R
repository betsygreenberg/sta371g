# checking for missing entries

grades <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/class-grades.csv")

grades[!complete.cases(grades),]

#to deal with missing data, you can 
#1) remove them from data set - but this removes all the other variables too
#2) use the mean of the values
#3) use regression to predict them

#use mean
grades$Final[is.na(grades$Final)] <- mean(grades$Final, na.rm = T)
grades
grades$TakeHome[is.na(grades$TakeHome)] <- mean(grades$TakeHome, na.rm = T)
grades

#use regresssion to predict 
#build a model with missing variable as Y and the rest of the predictors as X 
names(grades)
model1 <- lm(TakeHome ~ Prefix + Assignment + Tutorial + Midterm + Final, data = grades)



#exploring multicollinearity
model <- lm(Final ~ TakeHome + Midterm + Tutorial + Assignment + Prefix, data = grades) 
library(car)
vif(model)


#exploring influential points
plot(model)
grades[59,]
#etc


#SIMULATION
#coin flipping - equal probability 
sample(c(0, 1), 5, replace=T) 


#
results <- replicate(100000, {
  flips <- sample(c(0, 1), 10, replace=T)
  sum(flips)
})
results

results <- replicate(10,
                     {
                       flips <- sample(c(0, 1), 10, replace=T)
                       sum(flips)
                     })
results


#compare it using E(X) = np and SD(X) = sqrt(np(1-p))
np = 10*0.5 = 5
SD = 1.58 

mean(results)
sd(results)

#if your variable comes from a normal distribution, you can use the rnorm command
grades <- replicate(100000, {
  midterm1 <- 75
  midterm2 <- rnorm(1, mean=90, sd=5)
  final.exam <- rnorm(1, mean=90, sd=5)
  return(.25*midterm1 + 0.25*midterm2 + 0.5*final.exam >= 90)
})
sum(grades)/100000










