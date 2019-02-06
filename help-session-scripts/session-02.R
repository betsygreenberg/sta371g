
# All information about R is contained on the course page on GitHub.

# Setting Working Directory
# You can set it manually in the right of the RStudio window,
# or you can use the command below.
setwd("~/Desktop/STA 371G/R Help Sessions")

# For the homework, you need to download the file from MyStatLab!

# Importing Data (Excel)
library(readxl)
raw_data = read_excel("Motorcycles.xlsx", col_names = TRUE)

# Now the we have the data, we can build our model.
# Say we want to predict price (MSRP) from the wheelbase.

# First, we plot the data.
plot(raw_data$Wheelbase, raw_data$MSRP)

# Let's say that we think the data is lienar.
# We decide to run a linear regression.
model = lm(MSRP ~ Wheelbase, data = raw_data)

# We can view the direct output of our model.
summary(model)
# Take note of slope, intercept, p-values, R-squared!
# With the slope and intercept, we can write the regression equation.

# We need to check assumptions!!!

# Independence
# This can only be checked by evaluating the situation.

# Linearity
# We have checked this before building our model.
# By observing the plot of the data, we said they could be linear.
# In reality, this data could use a transformation to make it linear.
# You will go over that later in class!

# Normality
hist(resid(model))
qqnorm(resid(model))
# The assumption seems to be satisfied!

# Equal Spread Assumption
residuals = resid(model)
estimated = predict(model)
plot(estimated, residuals)
# This seems to be violated!

# We want to predict MSRP when we have wheelbase equal to 45.5.
predict(model, list(Wheelbase=45.5))

# Now, we want the confidence interval for the same value.
predict(model, list(Wheelbase=45.5), interval = "confidence")
# The default level of intervals is 95%!

# Next, we find the prediction interval.
predict(model, list(Wheelbase=45.5), interval = "prediction")
