# Two ways to do the same thing!

results <- replicate(10000, {
  if (runif(1) < 0.45) {
    # strike oil! yay
    price <- rnorm(1, mean=45, sd=8)
    demand <- rgamma(1, shape=3, rate=1/40000)
    supply <- rgamma(1, shape=3, rate=1/40000)
    revenue <- price * min(supply, demand) - 1000000
  } else {
    # do not strike oil! boo
    revenue <- -1000000
  }
  return(revenue)
})
mean(results)

results <- replicate(10000, {
  if (runif(1) < 0.45) {
    # strike oil! yay
    price <- rnorm(1, mean=45, sd=8)
    demand <- rgamma(1, shape=3, rate=1/40000)
    supply <- rgamma(1, shape=3, rate=1/40000)
    if (supply < demand) {
      revenue <- price * supply - 1000000  
    } else {
      revenue <- price * demand - 1000000  
    }
  } else {
    # do not strike oil! boo
    revenue <- -1000000
  }
  return(revenue)
})