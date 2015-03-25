library("ggplot2")

fcr <- read.csv("fcr1b.csv", sep=";")
fcr <- fcr[fcr$Factor>0 & fcr$Temp<=27& fcr$Temp>=11,]
# View(fcr)

data.fcr <- data.frame("AvWeightCat" = factor(fcr$category), "Temp"=fcr$Temp, "AvWeight"=fcr$AvWeight, "FCR"=fcr$Factor)
nr <- nrow(data.fcr)

rate <- 0.4 
ids <- sort(sample(1:nr, round(nr*rate, 1)))

data.fcr.tr <- data.fcr[ids,]
data.fcr.ts <- data.fcr[-ids,]

# gerasimos
# john
