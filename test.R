# x, y: the x and y coordinates of the hull points
# n: the number of points in the curve.
bezierCurve <- function(x, y, n=10)
{
  outx <- NULL
  outy <- NULL
  
  i <- 1
  for (t in seq(0, 1, length.out=n))
  {
    b <- bez(x, y, t)
    outx[i] <- b$x
    outy[i] <- b$y
    
    i <- i+1
  }
  
  return (list(x=outx, y=outy))
}

bez <- function(x, y, t)
{
  outx <- 0
  outy <- 0
  n <- length(x)-1
  for (i in 0:n)
  {
    outx <- outx + choose(n, i)*((1-t)^(n-i))*t^i*x[i+1]
    outy <- outy + choose(n, i)*((1-t)^(n-i))*t^i*y[i+1]
  }
  
  return (list(x=outx, y=outy))
}

# Example usage

fcr <- read.csv("fcr1.csv", sep=";")
fcr <- fcr[fcr$Factor>0 & fcr$Temp<=27& fcr$Temp>=11,]
data.fcr <- data.frame("AvWeightCat" = factor(fcr$category), "Temp"=fcr$Temp, "FCR"=fcr$Factor)

ds200_250 <- data.fcr[data.fcr$AvWeightCat == '200--250', ]

x <- ds200_250$Temp
y<-ds200_250$FCR
plot(x, y, "o", pch=20)
points(bezierCurve(x,y,20), type="l", col="red")


#-------------------------------------------------
# fit.lo <- loess(y~x,span=0.25)
# > plot(x, y, "o", pch=20)
# > lines(fitted(fit.lo) ~ x, col = 'darkorange1', lwd = 2)
# > fit.lo1 <- loess(y~x,span=0.5)
# > fit.lo2 <- loess(y~x,span=0.75)
# > lines(fitted(fit.lo1) ~ x, col = 'red', lwd = 2)
# > lines(fitted(fit.lo2) ~ x, col = 'blue', lwd = 2)



