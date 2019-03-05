cars <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/cars.csv")

cars$LateModel <- ifelse(cars$After1975 == "Yes", 1, 0)
cars$EarlyModel <- ifelse(cars$After1975 == "No", 1, 0)

# These are all effectively the same model!
summary(lm(MPG ~ HP + Weight + LateModel, data=cars))
# Using EarlyModel instead of LateModel just flips the sign of the dummy coefficient
summary(lm(MPG ~ HP + Weight + EarlyModel, data=cars))
# R automatically generates an "After1975 == Yes" dummy variable
summary(lm(MPG ~ HP + Weight + After1975, data=cars))

cars$OriginUS <- ifelse(cars$Origin == "US", 1, 0)
cars$OriginEU <- ifelse(cars$Origin == "EU", 1, 0)
cars$OriginJP <- ifelse(cars$Origin == "JP", 1, 0)

# These three models are essentially equivalent; just interpret the dummy variable coefficients appropriately!
summary(lm(MPG ~ HP + Weight + OriginUS + OriginEU, data=cars))
summary(lm(MPG ~ HP + Weight + OriginUS + OriginJP, data=cars))
summary(lm(MPG ~ HP + Weight + OriginJP + OriginEU, data=cars))
# Also equivalent -- R generates dummy variables for you and leaves one out
summary(lm(MPG ~ HP + Weight + Origin, data=cars))
