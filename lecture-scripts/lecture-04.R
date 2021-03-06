stock.market <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/stock-market-returns.csv")
hist(stock.market$AMZN)
plot(stock.market$W5000, stock.market$AMZN)
plot(AMZN ~ W5000, data=stock.market)
model <- lm(AMZN ~ W5000, data=stock.market)
summary(model)
confint(model)
hist(residuals(model))
qqnorm(residuals(model))

plot(stock.market$W5000, stock.market$PG)
model2 <- lm(PG ~ W5000, data=stock.market)
plot(stock.market$W5000, residuals(model2))
qqnorm(residuals(model2))
summary(model2)
plot(model2)

lm(AMZN ~ W5000, data=stock.market)
lm(PG ~ W5000, data=stock.market)
lm(SBUX ~ W5000, data=stock.market)
lm(BAC ~ W5000, data=stock.market)
