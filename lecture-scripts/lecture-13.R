nba <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/nba.csv")
model <- lm(PTS ~ N3PA + PCT3P, data=nba)
summary(model)

model2 <- lm(PTS ~ N3PA * PCT3P, data=nba)
summary(model2)
predict(model2, list(N3PA=30, PCT3P=35.4))
