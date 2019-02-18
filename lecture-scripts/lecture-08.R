colleges <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/colleges.csv")
my.sample <- subset(colleges, !is.na(Average.combined.SAT) & Graduation.rate <= 100)
model <- lm(Graduation.rate ~ Average.combined.SAT + In.state.tuition, data=my.sample)
summary(model)
plot(predict(model), residuals(model))
plot(model)

model2 <- lm(Graduation.rate ~ In.state.tuition + Out.of.state.tuition +
               Full.time.students + Part.time.students, data=my.sample)
plot(model2)

summary(lm(Graduation.rate ~ In.state.tuition, data=colleges))
summary(lm(Graduation.rate ~ Average.combined.SAT + In.state.tuition, data=colleges))

model3 <- lm(Graduation.rate ~ Acceptance.rate, data=my.sample)
summary(model3)

model4 <- lm(Graduation.rate ~ Acceptance.rate + Average.combined.SAT, data=my.sample)
summary(model4)
