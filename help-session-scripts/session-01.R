Motorcycles <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/Motorcycles.csv")

names(Motorcycles)
Model <- Motorcycles$Model
MSRP <- Motorcycles$MSRP
Bore <- Motorcycles$Bore
Displacement <- Motorcycles$Displacement
Clearance <- Motorcycles$Clearance
EngineStrokes <- Motorcycles$EngineStrokes
Wheelbase <- Motorcycles$Wheelbase


#creating a histogram
hist(Clearance, col="pink")

#creating a scatterplot
plot(MSRP ~ Clearance, pch = 15, col = "orange")

#calculating correlation
cor(MSRP,Clearance)

#creating model 
model1 <- lm(MSRP ~ Clearance)
summary(model1)

#visualizing regression
plot(MSRP ~ Clearance)
abline(model1) 

#predictions
predict(model1, list(Clearance = 10.00))

#making confidence intervals 
predict(model1, list(Clearance = 10.00), interval = "confidence", level = 0.99)

#making prediction intervals
predict(model1, list(Clearance = 10.00), interval = "prediction", level = 0.99)

#checking regression assumptions
#linearity
plot(model1)

#independence

#normality of residuals 
qqnorm(resid(model1))
hist(resid(model1))

#equal variance (homoscedasticity)



