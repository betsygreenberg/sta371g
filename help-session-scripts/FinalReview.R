
# 2. ----
results = replicate(1e4, {
  supply = rnorm(1, mean = 10000, sd = 100)
  demand = rgamma(1, shape = 3, rate = 1/2500)
  sold = min(demand, supply)
  profit = sold*20 - supply*5
  return(profit)
})
mean(results)

# 3. ----
quantile(results, probs = 0.9)

# 4. ----
check = results < 0
sum(check)/length(check)

# 5. ----
results = replicate(1e5, {
  supply = rnorm(1, mean = 15000, sd = 100)
  demand = rgamma(1, shape = 3, rate = 1/2500)
  sold = min(demand, supply)
  profit = sold*20 - supply*5
  return(profit)
})
mean(results)

# 6. ----
results = replicate(1e5, {
  balance = 1e5
  default = runif(1) < 0.2
  if (default) {balance = 0}
  else {balance = balance * (1 + rnorm(1, mean = 0.2, sd = 0.05))}
  return(balance)
})
mean(results)

# 8. ----
results = replicate(1e5, {
  balance = 1e5
  for (year in 1:10) {
    default = runif(1) < 0.2
    if (default) {balance = 0}
    else {balance = balance * (1 + rnorm(1, mean = 0.2, sd = 0.05))}
  }
  return(balance)
})
mean(results)

# 10. ----
# Price = 304377 - 32212*log(Age)
age = 100
price = 304377 - 32212*log(age)
print(price)

# 11. ----
# Think about logical connection.
# Use k-fold validation.

# 13. ----
# All except the one quantitative response.

# 14. ----
rm(list = ls())
admissions = read.csv("~/Desktop/STA 371G/FinalReview/admissions.csv")
model1 = glm(admitted ~ sat + hsgpa, data = admissions, family = "binomial")
summary(model1)
# The difference is 1 in GPA.
# For each unit increase in GPA, the odds get multiplied by exp(0.3911134).
exp(0.3911134)
# Odds increase by 47.86%.

# 15. ----
predict(model1, list(sat=1200, hsgpa=3.0), type = "response")

# 16. ----
# Most common:
sum(admissions$admitted)/nrow(admissions)
# Our model in general:
predictions = predict(model1, type = "response") >= 0.5
check = predictions == admissions$admitted
sum(check)/length(check)
# Our model among admitted people:
prop.table(table(predictions, admissions$admitted), 2)

# 17. ----
rm(list = ls())
field_goals = read.csv("~/Desktop/STA 371G/FinalReview/field_goals.csv")
model1 = lm(salary ~ distance, data = field_goals)
plot(model1)
plot(salary ~ distance, data = field_goals)
model2 = lm(log(salary) ~ distance, data = field_goals)
plot(model2)
plot(log(salary) ~ distance, data = field_goals)

# 18. ----
summary(model2)

# 19. ----
exp(predict(model2, list(distance=75)))

# 20. ----
model3 = glm(success ~ distance * league, data = field_goals, family = "binomial")
summary(model3)
intercept = 7.30368 + 2.19829
slope = - 0.17708 - 0.14003

