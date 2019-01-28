addhealth <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/addhealth.csv")
addhealth$agew1
hist(addhealth$agew1, col="green")

hist(addhealth$h4to34)
age <- addhealth$h4to34
age[age >= 96] <- NA 
hist(age)

num.drinks <- addhealth$h4to36
num.drinks[num.drinks >= 96] <- NA
hist(num.drinks)

model <- lm(num.drinks ~ age)
summary(model)
predict(model, list(age=21))
