#Dummy Variables recap 
#Basically categorical variables 
#number of predictor variables = n-1, where n = number of levels 
#the level that is not included as one of the predictor variables is the reference level

#read the data
diamonds <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/diamonds_reduced.csv")
names(diamonds)

#get a general sense of the data (plot price against carat size)
plot(diamonds$Carat.Size, diamonds$Price)

#taking out the outlier
diamonds <- diamonds[diamonds$Carat.Size <= 3,]
plot(diamonds$Carat.Size, diamonds$Price)

#making a model with carat size as the only predictor
model1 <- lm(Price ~ Carat.Size, data = diamonds)
summary(model1)

#making a model with carat size and cut as predictors
model2 <- lm(Price ~ Carat.Size + Cut, data = diamonds)
summary(model2)

#we want to see if carat size and cut have an interaction with each other 
#ie does the slope of carat size depend on cut? 

#eg pulled from slides 
#Price = 13599 + 111 Living.Area + 5567 FireplaceYes 
#FireplaceYes variable is either 1 when there is a fireplace and 0 when there
#is no fireplace --> only the intercept is affected

#slopes of levels of categorical variable of Cut
model.excellent <- lm(Price ~ Carat.Size, data = diamonds[diamonds$Cut == "Excellent",])
model.verygood <- lm(Price ~ Carat.Size, data = diamonds[diamonds$Cut == "Very Good",])
model.good<- lm(Price ~ Carat.Size, data = diamonds[diamonds$Cut == "Good",])
model.ideal <- lm(Price ~ Carat.Size, data = diamonds[diamonds$Cut == "Ideal",])

plot(diamonds$Carat.Size, diamonds$Price)
abline(model.excellent, col = "blue")
abline(model.verygood, col = "red")
abline(model.good, col = "green")
abline(model.ideal, col = "orange") 

#above shows that there might be an interaction between carat size and cut 

model3 <- lm(Price ~ Carat.Size * Cut, data = diamonds)
anova(model3) #check overall significance of interaction term
summary(model3) #check individual significance 


#THIS SECTION IS NOT PROPER R CODE. I just wanted to type out the regression equation for you to see.
#When Cut == "Good" 
Price = -1579.1 + β1 (Carat.Size) + β2 (CutGood) + β3 (CutIdeal) + β4 (CutVeryGood) + 
  β5 (Carat.Size : CutGood) + β6 (Carat.Size : CutIdeal) + β7 (Carat.Size : CutVeryGood) 

Price = -1579.1 + β1 (Carat.Size) + β2 (CutGood) + β5 (Carat.Size: CutGood)
Price = -1579.1 + 6396.4 (Carat.Size) - 456.0*1 - 262.9(Carat.Size)
Price = -2035.1 + 6133.5(Carat.Size)  

#Of a diamond of cut "Good", an increase in one unit of carat size is associated 
#with a -262.9 + 6396.4 increase in price. --> 6133.5

#The other interpretation
#A diamond of cut "Good" is worth $262.9(Carat.Size) + 456.0 less than a diamond of cut "Excellent", for 
#a certain carat size. 





