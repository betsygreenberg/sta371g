replicate(15, {
  flips <- sample(c(0, 1), 10, replace=T)
  return(sum(flips))
})

replicate(100, {
  flips <- sample(c(0, 1), 10, replace=T)
  return(sum(flips))
})

results <- replicate(100000, {
  flips <- sample(c(0, 1), 10, replace=T)
  sum(flips)
})
hist(results, breaks=10, col='orange')

grades <- replicate(100000, {
  midterm1 <- 75
  midterm2 <- rnorm(1, mean=80, sd=5)
  final.exam <- rnorm(1, mean=90, sd=5)
  return(.25*midterm1 + 0.25*midterm2 + 0.5*final.exam)
})
hist(grades, col="orange")

runs <- replicate(100000, {
  midterm1 <- 75
  midterm2 <- rnorm(1, mean=80, sd=5)
  final.exam <- rnorm(1, mean=90, sd=5)
  return(0.25*midterm1 + 0.25*midterm2 + 0.5*final.exam >= 90)
})
sum(runs) / 100000