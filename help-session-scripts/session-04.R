# First, set the working directory.
setwd("~/Desktop/STA 371G/HelpSession04")
# Then, we import the data.
original.data = read.csv("housing.csv", header = T)

# We will estimate sale price from lot and garage areas.
original.data = original.data[,c("Lot.Area", "Garage.Area", "SalePrice")]
# First we want to make sure that there is a linear trend.

# Let's plot against lot area.
plot(original.data$Lot.Area, original.data$SalePrice)
# There seems to be a linear trend in values close to 0.
# There are only a few outlier points.
# Let's get rid of those points and plot again.
new.data = original.data[-which(original.data$Lot.Area >= 50000),]
# We also want to omit any missing datapoints.
new.data = na.omit(new.data)
plot(new.data$Lot.Area, new.data$SalePrice)
# Okay, now we see the linear trend.
# However, we see heteroscedasticity.
# We address that with a transformation of Y.
# Usually, we use 1/Y, log(Y) or sqrt(Y).
plot(new.data$Lot.Area, 1/(new.data$SalePrice))
plot(new.data$Lot.Area, log(new.data$SalePrice))
plot(new.data$Lot.Area, sqrt(new.data$SalePrice))
# The first plot has outliers.
# The third plot still exhibits unequal variance.
# Let's go with the log transform.

# Now let's look at the plot against garage area.
plot(new.data$Garage.Area, new.data$SalePrice)
# We see the linear trend.
# Again, we see an unequal variance. We can try to fix it.
plot(new.data$Garage.Area, 1/new.data$SalePrice)
plot(new.data$Garage.Area, log(new.data$SalePrice))
plot(new.data$Garage.Area, sqrt(new.data$SalePrice))
# Be mindful of the fact that you have to choose the same
# transform for Y in both cases, since we are trying to
# have a multiple regression model.
# The log transform looks much better in this case as well.

# Finally, the decision is to use a log transform on SalePrice.
new.data$Log.SalePrice = log(new.data$SalePrice)

# We first build the model.
initial.model = lm(Log.SalePrice ~ Garage.Area + Lot.Area, 
                   data = new.data)
# Next, we check the model and the assumptions.
summary(initial.model)
plot(initial.model)
# The same points seem to cause the distortion in model.
# Let's try to remove them (182, 1554, 1558).
# First, we get the residuals.
res = resid(initial.model)
# Now we can remove the points.
final.data = new.data[-which(res <= -1.5),]
final.model = lm(Log.SalePrice ~ Garage.Area + Lot.Area, 
            data = final.data)
summary(final.model)
plot(final.model)

# In general, you should be very careful when removing points!
# Always ask yourself: can I justify removing these points?
# Can they be considered special cases?
# Compare models with and without these points.

# We see that the R squared does not increase by much,
# and it is still below 50%.
# Next step would be to include more predictors!

# Let's get an estimate for the sale price for the values below.
# Lot.Area = 15000, Garage.Area = 500
est = predict(final.model, list(Lot.Area=15000, Garage.Area=500))
# Be careful, because this command above will provide you a log
# of the sale price, and not the sale price itself.
# We have to calculate back the sale price, as shown below.
price = exp(est)

# Below, we calculate the prediction and confidence intervals.
pred.interval = predict(final.model, 
                        list(Lot.Area=15000, Garage.Area=500), 
                        interval = "prediction")
conf.interval = predict(final.model, 
                        list(Lot.Area=15000, Garage.Area=500), 
                        interval = "confidence")

# Finally, if we want the confidence interval for the model parameters,
# we use the command below.
confint(final.model, level = 0.95)


