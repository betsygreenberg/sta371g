manager <-read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/manager.csv", na.strings="")

boxplot(manager$YearsExp)
hist(manager$YearsExp)
subset(manager, YearsExp > 80)
manager$YearsExp[manager$YearsExp == 99] <- NA

mclean <- subset(manager, Salary > 0 & Salary < 200)


nrow(subset(manager, Salary >= 50 & Salary <= 60))
