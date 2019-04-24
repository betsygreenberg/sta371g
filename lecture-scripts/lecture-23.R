results <- replicate(10000, {
  account.value <- 0
  for (n in 1:30) {
    account.value <- account.value + 10000
    this.years.return <- rnorm(1, mean=.12, sd=.18)
    account.value <- account.value + (account.value * this.years.return)
  }
  for (n in 1:20) {
    account.value <- account.value - 100000
    if (account.value <= 0) {
      account.value <- 0
    } else {
      this.years.return <- rnorm(1, mean=.12, sd=.18)
      account.value <- account.value + (account.value * this.years.return)
    }
  }
  return(account.value)
})
hist(results)
mean(results)
quantile(results, probs=c(.05, .25, .5, .75, .95))

############################
total <- 0
for (k in 1:100) {
  total <- total + k^2
  total
}
print(total)
