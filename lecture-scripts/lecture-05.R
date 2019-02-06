cars <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/cars.csv")
model <- lm(MPG ~ Weight, data=cars)
plot(cars$Weight, residuals(model))
plot(model)

plot(cars$Weight, cars$MPG)
cars$WeightSq <- cars$Weight^2
plot(cars$WeightSq, cars$MPG)

cars$WeightRoot <- sqrt(cars$Weight)
plot(cars$WeightRoot, cars$MPG)

cars$LogWeight <- log(cars$Weight)
plot(cars$LogWeight, cars$MPG)
logmodel <- lm(MPG ~ LogWeight, data=cars)
plot(cars$LogWeight, residuals(logmodel))
summary(logmodel)
predict(logmodel, list(LogWeight=log(3000)))

cars$LogMPG <- log(cars$MPG)
loglogmodel <- lm(LogMPG ~ LogWeight, data=cars)
plot(cars$LogWeight, cars$LogMPG)
plot(cars$LogWeight, residuals(loglogmodel))
summary(loglogmodel)
exp(3.04)


log(2.72)
log(2.72^2)
log(2.72^3)
exp(1)
exp(2)
log(exp(81))
exp(log(81))


beer <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/beer.csv")
plot(beer$Num.seconds, beer$Beer.height)
beer$ExpHeight <- exp(beer$Beer.height)
plot(beer$Num.seconds, beer$ExpHeight)
