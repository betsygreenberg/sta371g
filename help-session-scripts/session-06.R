manager <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/manager.csv")

#get a general sense of the data (plot manager rating against salary)
plot(manager$Salary, manager$MngrRating)

#remove outliers and clean data
manager <- manager[-c(154),]
manager <- subset(manager, Salary > 0 & Salary < 200)
plot(manager$Salary, manager$MngrRating)


#making a model with only salary as the predictor
names(manager)
model1 <- lm(MngrRating ~ Salary, data = manager)
summary(model1)

#making a model with salary and origin as predictors
model2 <- lm(MngrRating ~ Salary + Origin, data = manager)
summary(model2)

model.internal <- lm(MngrRating ~ Salary, data = manager[manager$Origin == "Internal",])
model.external <- lm(MngrRating ~ Salary, data = manager[manager$Origin == "External",])

plot(manager$Salary, manager$MngrRating)
abline(model.internal, col = "blue")
abline(model.external, col = "red")

#above shows that there might be an interaction between origin and salary

model3 <- lm(MngrRating ~ Salary*Origin, data = manager)
summary(model3)
anova(model3)

#This section is not proper R code. 
#interpretations of interaction coefficients
#When origin == "internal"
MngrRating = -1.92370 + 0.10525(Salary) + 1.30858(OriginInternal) - 0.01298(Salary:OriginInternal)
MngrRating = -1.92370 + 0.10525(Salary) + 1.30858*1 - 0.01298(Salary)
MngrRating = -0.61512 + 0.09227(Salary)

#When the new hire is hired internally, an increase in $1000 in salary
#is associated with a -0.01298 + 0.10525 = 0.09227 increase in manager rating.

#the other interpretation
#For a certain salary, someone who is hired internally is associated with a difference(could be positive
#or negative depending on value of salary) rating
#of -0.01298(Salary) + 1.30858 compared to someone who is hired externally.
