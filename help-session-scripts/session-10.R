# Investing in Portfolio ----

rm(list = ls())
results = replicate(20000, {
  value = 10000
  for (year in 1:30) {
    value = value * (1 + rnorm(1, mean = 0.8, sd = 0.10))
  }
  value
})

hist(results)
mean(results)

# Investing in Portfolio with 2 Securities ----

rm(list = ls())
results = replicate(20000, {
  value = 10000
  for (year in 1:30) {
    value = value * (1 + 0.55*rnorm(1, mean = 0.8, sd = 0.10) 
                     + 0.45*rnorm(1, mean = 0.3, sd = 0.25))
  }
  value
})

hist(results)
mean(results)

# Investing in Portfolio with 2 Securities (Different Weights) ----

rm(list = ls())
results = replicate(20000, {
  value = 10000
  for (year in 1:30) {
    value = value * (1 + 0.85*rnorm(1, mean = 0.8, sd = 0.10) 
                     + 0.15*rnorm(1, mean = 0.3, sd = 0.25))
  }
  value
})

hist(results)
mean(results)

# Rolling a Fair Die ----

rm(list = ls())
results = replicate(20000, {
  n = 0
  i = runif(1, min = 0, max = 6)
  if (0 <= i & i < 1) {n = 1}
  if (1 <= i & i < 2) {n = 2}
  if (2 <= i & i < 3) {n = 3}
  if (3 <= i & i < 4) {n = 4}
  if (4 <= i & i < 5) {n = 5}
  if (5 <= i & i < 6) {n = 6}
  n
})

mean(results)

# Rolling a Biased Die ----

rm(list = ls())
pmf = c(0.15, 0.45, 0.05, 0.15, 0.10, 0.10)
cdf = cumsum(pmf)
results = replicate(20000, {
  n = 0
  i = runif(1, min = 0, max = 1)
  if (i < cdf[1]) {n = 1}
  if (cdf[1] <= i & i < cdf[2]) {n = 2}
  if (cdf[2] <= i & i < cdf[3]) {n = 3}
  if (cdf[3] <= i & i < cdf[4]) {n = 4}
  if (cdf[4] <= i & i < cdf[5]) {n = 5}
  if (cdf[5] <= i) {n = 6}
  n
})

calculated.mean = pmf %*% c(1:6)
empirical.mean = mean(results)

# Estimating PI ----

rm(list = ls())
library(plotrix)
# Area of Circle = pi * r^2
# Area of Square = 4 * r^2
# Ratio = 0.25 * pi

npoints = 1000
x = runif(npoints, min = -1, max = 1)
y = runif(npoints, min = -1, max = 1)
pts = cbind(x=x, y=y)

# Equation of the circle: x^2 + y^2 = 1

inside = x^2 + y^2 < 1

pi.estimate = 4*sum(inside)/npoints

pts.in = pts[which(inside==1),]
pts.out = pts[which(inside==0),]

plot(pts.in, col = "red", asp=1)
points(pts.out, col = "blue", asp=1)
draw.circle(0, 0, 1)

