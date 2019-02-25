counties <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/counties.csv")
counties$PhysiciansPer10000 <- counties$Physicians / counties$Population * 10000
my.counties <- subset(counties, Population > 10000)

install.packages("leaps")
library(leaps)
plot(regsubsets(PhysiciansPer10000 ~ LandArea + PctRural + MedianIncome +
             Population + PctUnder18 + PctOver65 + PctPoverty +
             PctUninsured + PctSomeCollege + PctUnemployed,
           data=my.counties), scale="adjr2")

summary(lm(PhysiciansPer10000 ~ PctRural + PctOver65 + PctSomeCollege,
           data=my.counties))

test.cases <- sample(1:168, 50)
training.cases <- setdiff(1:168, test.cases)
training.set <- my.counties[training.cases,]
test.set <- my.counties[test.cases,]
model <- lm(PhysiciansPer10000 ~ PctRural + PctOver65 + PctSomeCollege,
   data=training.set)
predicted.y <- predict(model, test.set)
cor(test.set$PhysiciansPer10000, predicted.y)^2
