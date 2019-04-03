library(okcupiddata)
profiles$male <- ifelse(profiles$sex == "m", 1, 0)
my.profiles <- subset(profiles, height >= 55 & 
                        height <= 80)
model2 <- glm(male ~ height + orientation,
              data=my.profiles, family=binomial)
summary(model2)
exp(-46.08)

empirical.logit.plot(model2)

predicted.male <- (predict(model2, type="response") >= 0.5)
actual.male <- (my.profiles$male == 1)
sum(predicted.male == actual.male) / nrow(my.profiles)

model3 <- glm(male ~ height * orientation, data=my.profiles,
              family=binomial)
summary(model3)
