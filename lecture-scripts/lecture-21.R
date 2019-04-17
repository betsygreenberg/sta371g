houses <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/houses.csv")
nrow(houses) * .3
test.cases <- sample(1:1728, 518)
test.set <- houses[test.cases, ]
training.cases <- setdiff(1:1728, test.cases)
training.set <- houses[training.cases,]

mean(training.set$Price)
mean(test.set$Price)

library(leaps)
plot(regsubsets(Price ~ Lot.Size + Waterfront + Age + Land.Value +
             New.Construct + Central.Air + Fuel.Type + 
             Heat.Type + Sewer.Type + Living.Area +
             Pct.College + Bedrooms + Fireplaces +
             Bathrooms, data=training.set), scale="adjr2")

model <- lm(Price ~ Living.Area + Land.Value, data=training.set)
mean(abs(residuals(model)))

price.hat <- predict(model, test.set)
mean(abs(price.hat - test.set$Price))

titanic <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/titanic.csv")

