# solution i found  to see how it works
library("rgl")
library("lattice")
library("ggplot2")
my_surface <- function(f, n=10, ...) { 
  ranges <- rgl:::.getRanges()
  x <- seq(ranges$xlim[1], ranges$xlim[2], length=n)
  y <- seq(ranges$ylim[1], ranges$ylim[2], length=n)
  z <- outer(x,y,f)
  surface3d(x, y, z, ...)
}

library(rgl)
f <- function(x1, x2)
  sin(x1) * x2 + x1 * x2
n <- 200
x1 <- 4*runif(n)
x2 <- 4*runif(n)
y <- f(x1, x2) + rnorm(n, sd=0.3)
plot3d(x1,x2,y, type="p", col="red", xlab="X1", ylab="X2", zlab="Y", site=5, lwd=15)
my_surface(f, alpha=.2 )


plot3d(data.fcr$Temp,data.fcr$FCR,data.fcr$AvWeight,type="p", col="red", xlab="Temp", ylab="FCR", zlab="Av. Weigth" )


# second approach
nhanes_body <- read.csv("~/Project_FCR/nhanes_body.txt")
View(nhanes_body)
plot(nhanes_body$age, nhanes_body$height, xlab = 'Age', ylab = 'Height', main = 'Height vs
Age')
boxplot(nhanes_body$height ~ nhanes_body$age, xlab = 'Age', ylab = 'Height', main = 'Height vs
Age')

ggplot(data = nhanes_body, aes(factor(age),height))+ geom_boxplot()

#
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


fcr.fit <- loess(data.fcr.tr$FCR ~ data.fcr.tr$Temp * data.fcr.tr$AvWeight, span = 1,
                    family = 'gaussian')

Temp.vals <- seq(from = 11, to = 27, by = 1)
AvWeight.vals <- seq(from = 1, to = 1000, by = 20)
AvWeight.vals <-c(0.75,2.5,6,11,16,26.5,47.5,70,90,125,175,225,275,325,425)
predicted.FCR <- predict(fcr.fit, newdata = expand.grid(AvWeight = AvWeight.vals,Temp = Temp.vals ))
predFCR = data.frame(predicted.FCR)
#plot
persp( AvWeight.vals,Temp.vals, predicted.FCR, theta = 40, xlab =
        'Temp', ylab = 'AvWeight', zlab = 'FCR')
#plot
plot3d(AvWeight.vals,Temp.vals,predicted.FCR,type="p", col="red", xlab="Temp", ylab="FCR", zlab="Av. Weigth" )
surface3d(AvWeight.vals,Temp.vals,predicted.FCR)






