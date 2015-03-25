library("ggplot2")
library("RColorBrewer")
# libraries to fit the FCR curves
library("splines")


#-------------------------------------------------
Best.Fitting.Curve <- function( x, y, rate, category )
{
  ids <- sort(sample(1:length(x), round(length(x)*rate, 1)))
  
  # Training Set
  x.tr <- x[ids]
  y.tr <- y[ids]
  # Testing Set
  x.ts <- x[-ids]
  y.ts <- y[-ids]
  
  spars <- seq(0, 1.5, by = 0.01)
  rmse <- rep(0, length(spars))
  
  for (i in 1:length(spars)){
    # using smooth splines
    fit.spl <- smooth.spline(x.tr, y.tr, spar = spars[i], cv = FALSE, all.knots=TRUE)
    # predict values onto testing set
    y.pred <- predict(fit.spl, x.ts)
    rmse[i] <- sqrt(sum((y.pred$y - y.ts)^2)/length(y.ts)) 
  }
  
  imag.name <- paste( paste('AvEndWeight_Category_',category), 'png', sep='.')
  png(imag.name, width=800, height=640, units="px")
  plot(spars, rmse, 'l', xlab = 'spar', ylab = 'Cross Validation Residual Sum of Squares' , 
        main = paste('CV RSS vs Spar for Weight Category', category) )
  dev.off()
  
  # find the minimum rmse -> best spar value -> best model 
  id <- which.min(rmse)
  sp <- spars[id]
  best.rmse <- rmse[id]
  best.fit.spl <- smooth.spline(x.tr, y.tr, spar = sp, cv = FALSE, all.knots=TRUE)
   
  # plot best model with training and testing set
  data.tr <- data.frame(cbind(x.tr, y.tr))
  data.ts <- data.frame(cbind(x.ts, y.ts))
  data.pred <- data.frame(predict(best.fit.spl,x))
  
   imag.name1 <- paste( paste('Best.Model.Smooth.Splines_',category), 'png', sep='.')
   plot.title <- paste('Category:', category, '|| RMSE:', best.rmse, sep=" ") 
   png(imag.name1, width=800, height=640, units="px")
   p <-ggplot() + 
         geom_point(data=data.tr, aes(x=x.tr, y=y.tr),size=3, col='blue' ) + 
         geom_point(data = data.ts, aes(x=x.ts, y=y.ts),size=3, col='red') + 
         geom_line(data =data.pred ,aes(x,y), color="darkorange1") + 
         xlab("Temperature") + ylab("FCR") + ggtitle(plot.title) 
  
  
   data2.labels <- data.frame(
       xaxis = c(25, 25), 
       yaxis1 = c(max(y)-0.2, max(y)-0.1), 
       yaxis2 = c(max(y)+0.2, max(y)+0.1), 
       label = c("Blue: Training points", "Red: Testing points")
   )
  
   if (max(y) >= 2)
   {  
     p <- p + geom_text(data = data2.labels, aes(x = xaxis, y = yaxis1, label = label))
   }else{
     p <- p + geom_text(data = data2.labels, aes(x = xaxis, y = yaxis2, label = label))
   }
   print(p)
   dev.off()
  
   return( list(best.model = best.fit.spl, RMSE = best.rmse))
}


#----------------------------------
fcr <- read.csv("fcr1.csv", sep=";")
fcr <- fcr[fcr$Factor>0 & fcr$Temp<=27& fcr$Temp>=11,]
# View(fcr)

data.fcr <- data.frame("AvWeightCat" = factor(fcr$category), "Temp"=fcr$Temp, "FCR"=fcr$Factor)

colourCount = length(unique(data.fcr$AvWeightCat)) + 5
#getPalette = colorRampPalette(brewer.pal(8, "RdBu"))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

png("FCR_plot.png", width=800, height=640, units="px")
p <- ggplot(data.fcr,aes(x=Temp, y=FCR, colour=AvWeightCat)) + geom_point(size=3) + geom_line()
p <- p +  scale_fill_manual(values = getPalette(colourCount)) + theme(legend.position="right") + 
          guides(fill=guide_legend(nrow=2))
print(p)
dev.off()

print(p)

#---------------------------------------------------------
# curve fitting with regression models

categories <- unique(data.fcr$AvWeightCat)
nc <- length(categories)

Temps <- seq(10, 30, by = 1)
FCR.Table <- as.data.frame(matrix(nrow=0, ncol=length(Temps))) 
colnames(FCR.Table) <- as.character(Temps)

RMSE.per.Category <- as.data.frame(matrix(nrow=nc, ncol=1))

for (i in 1:nc)
{
  ds <- data.fcr[data.fcr$AvWeightCat == categories[i], ]
  x <- ds$Temp
  y <-ds$FCR
  rate <- 0.5
  
  res <- Best.Fitting.Curve( x, y, rate, categories[i] )
  
  RMSE.per.Category[nrow(RMSE.per.Category)+1,] <- res$RMSE
  
  # Predict the FCR value using the Temperature. Complete the FCR table
  pred.Temp <- predict( res$best.model, Temps)
  FCR.Table[nrow(FCR.Table)+1,] <- pred.Temp$y 
  
}  

rownames(FCR.Table) <- categories
write.csv2(round(FCR.Table,3), file='FCR.Table.csv',row.names=TRUE )
