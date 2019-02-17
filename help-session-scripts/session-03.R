#Autocorrelation

#Autocorrelation - Tests for independence of the residuals
  #In simple terms, residuals are not influenced by surrounding residuals
    #Positive Autocorrelation: Errors are followed by errors of the same sign
    #Negative Autocorrelation: Errors are followed by errors of the opposite sign
#Durbin-Watson Statistic tests for autocorrelation
#DW Statistic will always be between 0 and 4
  #DW Table will give critical values needed to evaluate DW Statistic: will be between 0 and 2
    #0 ~ L = Evidence of Positive Autocorrelation
    #L ~ H = Test is inconclusive
    #H ~ 2 = No Evidence of Autocorrelation
    #2 ~ (4 - H) = No Evidence of Autocorrelation
    #(4 - H) ~ (4 - L) = Test is inconclusive
    #(4 - L) ~ 4 = Evidence of Negative Autocorrelation

Motorcycles <- read_excel("Desktop/Motorcycles.xlsx")

Wheelbase <- Motorcycles$Wheelbase
MSRP <- Motorcycles$MSRP
plot(MSRP ~ Wheelbase)

model1 <- lm(MSRP ~ Wheelbase)
abline(model1)

#Residuals show increasing trend on right side: test for autocorrelation

install.packages("lmtest")
library(lmtest)
dwtest(model1)
#93 Observations, 1 Independent Variable
#dL = 1.63, dH = 1.68
#At 1.0792, evidence of positive autocorrelation exists

#Transformations

#Transformations can fit non-linear data into a linear regression

plot(model1)
#Equal Spread is violated, R^2 is 0.66; Transform MSRP to log(MSRP)
model2 <- lm(log(MSRP) ~ Wheelbase)
plot(model2)

#Equal Spread is better; R^2 is 0.78
#Can do other transformations: Square Roots, Reciprocals, etc.
