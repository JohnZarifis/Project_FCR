library("ggplot2")
library("gam")
library("mgcv")

fcr <- read.csv("fcr1b.csv", sep=";")
fcr <- fcr[fcr$Factor>0 & fcr$Temp<=27& fcr$Temp>=11,]
# View(fcr)

data.fcr <- data.frame("AvWeightCat" = factor(fcr$category), "Temp"=fcr$Temp, "AvWeight"=fcr$AvWeight, "FCR"=fcr$Factor)
nr <- nrow(data.fcr)

rate <- 0.4 
ids <- sort(sample(1:nr, round(nr*rate, 1)))

data.fcr.tr <- data.fcr[ids,]
data.fcr.ts <- data.fcr[-ids,]
pred.data <- data.frame('Temp'=data.fcr.ts$Temp, 'AvWeight'=data.fcr.ts$AvWeight)

gam.m1 <- gam(formula=FCR~s(Temp)+s(AvWeight), data=data.fcr.tr)
pred.gam.m1 <- predict(gam.m1, pred.data)
rmse.gam.m1 <- sqrt( sum( (data.fcr.ts$FCR - pred.gam.m1)^2 )/nrow(data.fcr.ts)  )

gam.m2 <- gam(formula=FCR~s(Temp)+s(AvWeight),data=data.fcr.tr, method="REML")
pred.gam.m2 <- predict(gam.m2, pred.data)
rmse.gam.m2 <- sqrt( sum( (data.fcr.ts$FCR - pred.gam.m2)^2 )/nrow(data.fcr.ts)  )

gam.m3 <- gam(formula=FCR~te(Temp, AvWeight),data=data.fcr.tr)
pred.gam.m3 <- predict(gam.m3, pred.data)
rmse.gam.m3 <- sqrt( sum( (data.fcr.ts$FCR - pred.gam.m3)^2 )/nrow(data.fcr.ts)  )

anova(gam.m1,gam.m2,gam.m3)

