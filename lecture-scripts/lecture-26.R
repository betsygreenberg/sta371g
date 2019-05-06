chips <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/chips.csv")

# Q1
model <- lm(log(Transistor.count) ~ Date.of.introduction, data=chips)

# Q2
exp(predict(model, list(Date.of.introduction=2020)))

# Q3
# New case is far from the mean of X => high leverage
# New case is also far outside of the general trend => high influence

# Q4
titanic <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/titanic.csv")
model.a <- lm(Age ~ Survived, data=titanic)
summary(model.a)
model.b <- lm(Age ~ Survived + PClass, data=titanic)
summary(model.b)
model.c <- lm(Age ~ Survived + Sex, data=titanic)
summary(model.c)

# Q5
model <- lm(Age ~  PClass, data=titanic)
summary(model)
-14.4592 - (-11.3676)

# Q6
model <- lm(Age ~ PClass * Survived, data=titanic)
summary(model)

# Q7
model <- glm(Survived ~ Age + PClass + Sex, data=titanic, family=binomial)
summary(model)

# Q8
predict(model, list(Age=20, Sex="female", PClass="1st"), type="response")

# Q9
# Coefficient of PClass2nd is -1.29; exp(-1.29) = 0.275, which is the multiplier
# for odds when changing from 1st to 2nd class.

# Q10
results <- replicate(10000, {
  if (runif(1) < 0.5) {
    winnings <- rnorm(1, mean=1, sd=1)
  } else {
    winnings <- 1
  }
  return(winnings)
})
mean(results)

# Q11
results <- replicate(10000, {
  if (runif(1) < 0.5) {
    winnings <- rnorm(1, mean=1, sd=1)
  } else {
    winnings <- 1
  }
  return(winnings > 0)
})
sum(results) / 10000
# Or table(results)

# Q12
results2 <- replicate(10000, {
  if (runif(1) < 0.5) {
    winnings <- rnorm(1, mean=1, sd=1)
  } else {
    winnings <- 2
  }
  return(winnings)
})
results3 <- replicate(10000, {
  if (runif(1) < 0.5) {
    winnings <- rnorm(1, mean=1, sd=1)
  } else {
    winnings <- 3
  }
  return(winnings)
})
results4 <- replicate(10000, {
  if (runif(1) < 0.5) {
    winnings <- rnorm(1, mean=1, sd=1)
  } else {
    winnings <- 4
  }
  return(winnings)
})
mean(results2)
mean(results3)  # <= This one results in expected payout of ~$2.00
mean(results4)

