library(okcupiddata)
predicted.male <- (predict(model, type="response") >= 0.5)
actual.male <- (my.profiles$male == 1)
sum(predicted.male == actual.male) / nrow(my.profiles)

x <- c(1, 2, 3)
y <- c(1, 2, 5)
x == y
sum(x == y)

empirical.logit.plot(model)

income.model <- glm(male ~ income, data=my.profiles, family=binomial)
summary(income.model)
empirical.logit.plot(income.model)
table(my.profiles$income)
hist(my.profiles$income)

my.profiles$logIncome <- log(my.profiles$income)
income.model2 <- glm(male ~ logIncome, data=my.profiles, family=binomial)
empirical.logit.plot(income.model2)
