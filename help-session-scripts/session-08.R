# how much does the model predict accurately? - compare to most common
# confusion table
# check linearity and independence

setwd("~/Desktop/STA 371G/HelpSession08")

# Importing the data set...
rawdata = read.csv("titanic.csv")
# Let's look at the data.
head(rawdata)

# A logistic regression is necessary. Let's build our model. 
model = glm(survived ~ age + fare, data = rawdata, family = "binomial")

# How does our summary look like?
summary(model)
# How do we interpret the coefficients?
# For each unit increase in fare, controlling for age, the estimated logodds
# of survival will increase by 0.013445.
# This does not mean much.
# We can also say that controlling for age, with each unit increase in fare,
# the odds for surviving will increase MULTIPLICATIVELY by exp(0.013445).

# We can estimate either logodds, or probabilities!
estimated.logodds = predict(model)
estimated.probabilities = predict(model, type = "response")

# Predictions work in a similar way as with linear regression!
# Let's make some predictions for a 25-year-old person that paid 150.
logodds = predict(model, list(age=25, fare=150))
probability = predict(model, list(age=25, fare=150), type = "response")

# Now, we assess our model.
# One way to do this is to see how many correct predictions we get.
predictions = ifelse(estimated.probabilities >= 0.5, 1, 0)
check = predictions==rawdata$survived
correct = sum(check)/nrow(rawdata)
print(correct)
# Our model predicts correctly 64% of the time!
# Now, we take a look at the most common occurence.
sur = sum(rawdata$survived)/nrow(rawdata)
print(sur)
# The most common occurence is death, or when survival equals 0.
# If we used this to predict, we would have the following accuracy.
common.accuracy = 1 - sur
print(common.accuracy)
# Our model gives us accurate predictions 64% of the time,
# whereas if we used the most common occurence, we would have correct
# estimates 59% of the time.

# Let's check the confusion matrix.
# We obtain the count of cases for each possibile prediction scenario.
table(predictions, rawdata$survived)
# We can also get the proportions of time for each case.
prop.table(table(predictions, rawdata$survived))
# If we have conditional probabilities, i.e. we know that a person
# actually did or did not survive, we can obtain the following.
prop.table(table(predictions, rawdata$survived), 2)

# Finally, let us check linearity.
# We use the empirical.logit.plot() function found on GitHub.
# We need to import the function.
source("empirical-logit-plot.R")
empirical.logit.plot(model)
