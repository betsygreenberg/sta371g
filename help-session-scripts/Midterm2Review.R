load("midterm2-practice.RData")

# 1. ----
library(leaps)
plot(regsubsets(rate ~ len + adt + trks + sigs1 + slim + shld + lane 
                + acpt + itg + lwid + htype, data = highway), scale="adjr2")

# 2. ----
mod1 = lm(rate ~ len + acpt + slim, data = highway)
influences = cooks.distance(mod1)
sort(influences, decreasing = T)[1]
# leverage = hatvalues(mod1)


# 5. ----
mod2 = lm(rate ~ len + htype, data = highway)

# 6. ----
summary(mod2)
coef = coefficients(mod2)
coef[4]-coef[5]

# 7. ----
mod3 = lm(rate ~ len * htype, data = highway)
summary(mod3)

# 13. ----
p = 0.25
q = 1-p
o = q/(1-q)

# 14. ----
logodds = 0.5878-0.8109
odds = exp(logodds)
p = odds/(odds+1)

# 15. ----
mod4 = glm(MADE ~ DISTANCE, data = kd, family = "binomial")
summary(mod4)
(exp(coefficients(mod4)[2])-1)*100

# 16. ----
empirical.logit.plot(mod4)

# 17. ----
# When odds=1, p=0.5. Also, log(odds)=log(1)=0.
# 0 = 0.970938 - 0.055960*x
# 0.055960*x = 0.970938
0.970938/0.055960

# 18. ----
predicted = ifelse(predict(mod4, type = "response")>=0.5,1,0)
actual = kd$MADE
sum(predicted == actual)/nrow(kd)

# 19. ----
prop.table(table(predicted, actual), 2)

# 20. ----
mod5 = glm(MADE ~ DISTANCE + as.factor(QUARTER), data = kd, family = "binomial")
summary(mod5)







