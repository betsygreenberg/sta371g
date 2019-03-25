apple <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/apple.csv")
apple$QuarterCat <- as.factor(apple$Quarter)

model1 <- lm(Revenue.Billions ~ Time, data=apple)
summary(model1)

model2 <- lm(Revenue.Billions ~ Time + QuarterCat, data=apple)
summary(model2)
predict(model2, list(Time=51, QuarterCat="1"))

model3 <- lm(log(Revenue.Billions) ~ log(Time) + QuarterCat, 
             data=apple)
summary(model3)
exp(predict(model3, list(Time=51, QuarterCat="1")))

predict(model3)
plot(Revenue.Billions ~ Time, data=apple, type="l")

apple$TimCookEra <- ifelse(apple$Time >= 22, 1, 0)
