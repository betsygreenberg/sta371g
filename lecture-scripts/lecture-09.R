counties <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/counties.csv")
popmodel <- lm(Physicians ~ Population, data=counties)
plot(popmodel)
counties$PhysiciansPer10000 <- counties$Physicians / counties$Population * 10000
my.counties <- subset(counties, Population > 10000)
names(counties)

summary(lm(PhysiciansPer10000 ~ LandArea + PctRural + MedianIncome + Population +
     PctUnder18 + PctOver65 + PctPoverty + PctUninsured + PctSomeCollege +
     PctUnemployed, data=my.counties))
summary(lm(PhysiciansPer10000 ~ PctRural + MedianIncome + Population +
             PctUnder18 + PctOver65 + PctPoverty + PctUninsured + PctSomeCollege +
             PctUnemployed, data=my.counties))
summary(lm(PhysiciansPer10000 ~ PctRural +
            PctOver65 + PctSomeCollege
             , data=my.counties))
