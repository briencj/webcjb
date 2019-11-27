load("Rocket.dat.rda")
attach(Rocket.dat)
plot(Thrust, Temp)
summary(Rocket.dat)
MnThrust <- mean(Thrust)
MnThrust
