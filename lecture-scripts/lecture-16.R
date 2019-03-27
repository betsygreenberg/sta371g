install.packages("okcupiddata")
library(okcupiddata)
nrow(profiles)
table(profiles$sex)
35829/59946
35829/(35829+24117)

profiles$male <- ifelse(profiles$sex == "m", 1, 0)
my.profiles <- subset(profiles, height >= 55 & 
                        height <= 80)
nrow(my.profiles)

model <- glm(male ~ height, data=my.profiles, family=binomial)
summary(model)
-44.45 + .66*69
predict(model, list(height=69))
