stock.market <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/stock-market-returns.csv")
dwtest
install.packages("lmtest")
library(lmtest)

model <- lm(AMZN ~ W5000, data=stock.market)
dwtest(model)

model2 <- lm(SBUX ~ W5000, data=stock.market)
dwtest(model2)

apple <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/apple.csv")
apple.model <- lm(Revenue.Billions ~ Time, data=apple)
summary(apple.model)

acf(residuals(apple.model))
