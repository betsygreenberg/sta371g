houses <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/houses.csv")

model <- lm(Price ~ Fireplace, data=houses)
summary(model)

with.fireplace <- subset(houses, Fireplace == "Yes")
without.fireplace  <- subset(houses, Fireplace == "No")
mean(with.fireplace$Price) - mean(without.fireplace$Price)
tapply(houses$Price, houses$Fireplace, mean)

tapply(houses$Living.Area, houses$Fireplace, mean)

model2 <- lm(Price ~ Living.Area + Fireplace, data=houses)
summary(model2)

# Full equation: Predicted price = 13599 + 111L + 5567F
# No fireplace:                  = 13599 + 111L
# Yes fireplace:                 = 13599 + 111L + 5567*1 = 19166 + 111L

model3 <- lm(Price ~ Living.Area * Fireplace, data=houses)
summary(model3)
