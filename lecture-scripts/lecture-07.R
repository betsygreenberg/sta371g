colleges <- read.csv("https://raw.githubusercontent.com/brianlukoff/sta371g/master/data/colleges.csv")
hist(colleges$Graduation.rate)
summary(colleges$Graduation.rate)
my.sample <- subset(colleges, !is.na(Average.combined.SAT) & Graduation.rate <= 100)

names(my.sample)
summary(lm(Graduation.rate ~ Average.combined.SAT, data=my.sample))
summary(lm(Graduation.rate ~ Acceptance.rate, data=my.sample))
summary(lm(Graduation.rate ~ Pct.students.top.10.HS, data=my.sample))
summary(lm(Graduation.rate ~ In.state.tuition, data=my.sample))

model <- lm(Graduation.rate ~ Average.combined.SAT + In.state.tuition, data=my.sample)
summary(model)

predict(model, list(Average.combined.SAT=1000, In.state.tuition=20000))

subset(my.sample, Name == "University of Texas at Austin")
predict(model)
