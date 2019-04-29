results <- replicate(10000, {
  bank <- 100
  bet <- 1
  
  for (hand in 1:20) {
    if (runif(1) < 0.4) {
      bank <- bank + bet
    } else {
      bank <- bank - bet
    }
  }
  
  return(bank)
})
hist(results)
mean(results)
sd(results)
quantile(results)
table(results)
1263/(1263+8737)
sum(results)

results <- replicate(10000, {
  bank <- 100
  bet <- 1
  
  for (hand in 1:20) {
    if (runif(1) < 0.4) {
      bank <- bank + bet
      bet <- 1
    } else {
      bank <- bank - bet
      bet <- bet * 2
      if (bet > 1000) {
        bet <- 1000
      }
    }
  }

  return(bank)
})
hist(results)
quantile(results)

hist(rgamma(10000, shape=5, rate=1/10))

results <- replicate(10000, {
  if (runif(1) < 0.45) {
    # strike oil! yay
    price <- rnorm(1, mean=45, sd=8)
    demand <- rgamma(1, shape=3, rate=1/40000)
    revenue <- price * demand - 1000000
  } else {
    # do not strike oil! boo
    revenue <- -1000000
  }
  return(revenue)
})