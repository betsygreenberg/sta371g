
# Q2
model1 = lm(MathSAT ~ GPA, data = students)
summary(model1)

# Q3
model2 = lm(GPA ~ Height + MathSAT, data = students)
predict(model2, list(Height=60, MathSAT=700))

# Q5
# Closest to the mean x.
mean(students$GPA)
plot(students$GPA, students$MathSAT)

# Q7
plot(model2)

# Q8
summary(model2)

# Q10
predict(model2, list(Height=65, MathSAT=600), interval = "prediction", level=0.95)
confint(model2)
# Q11
# Not appropriate to interpret, since it is not significant.

# Q12
mean(students$MathSAT)
# Sinead:
790 - mean(students$MathSAT)
2.12 - predict(model2, list(Height=65, MathSAT=790))
# Tanaya:
230 - mean(students$MathSAT)
1.97 - predict(model2, list(Height=65, MathSAT=230))

# Q13
# All except "B has a higher R-squared".

# Q15
model3 = lm(Anamoly ~ Year, data = climate)
acf(resid(model3))

# Q16
acf(resid(model3))

# Q19
# Prediction intervals are larger!

# Q22
y = exp(0.23 + 0.8 * log(3))
print(y)



