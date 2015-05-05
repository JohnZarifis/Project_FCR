library("ggplot2")
library("gam")
library("mgcv")
library("rgl")
library("readxl")

#fcr <- read.csv("fcr1b.csv", sep=";")
#fcr <- fcr[fcr$Factor>0 & fcr$Temp<=27& fcr$Temp>=11,]
#View(fcr)

fcr <- read_excel('FCR_Template.xlsx',sheet=1,col_names=TRUE, na='na')
categories <- read_excel('FCR_Template.xlsx',sheet=2,col_names=TRUE, na='na')
#View(categories)
#str(categories)

data.fcr <- data.frame("AvWeightCat" = factor(fcr$category), "Temp"=fcr$Temp, "AvWeight"=fcr$AvWeight, "FCR"=fcr$FCR)
nr <- nrow(data.fcr)

#---------------------------------------------------------------------------------
#                  Search for the best model
#---------------------------------------------------------------------------------
#
rate <- 1
ids <- sort(sample(1:nr, round(nr*rate, 1)))

data.fcr.tr <- data.fcr[ids,]
data.fcr.ts <- data.fcr[-ids,]
nr.ts <- nrow(data.fcr.ts)
pred.data <- data.frame('Temp'=data.fcr.ts$Temp, 'AvWeight'=data.fcr.ts$AvWeight)

# model that Temp predictor is linear
gam.m1 <- gam(formula = FCR ~ Temp + s(AvWeight, bs="cr"), data=data.fcr.tr)
summary.gam.m1 <-summary(gam.m1)
pred.gam.m1 <- predict(gam.m1, pred.data)
rmse.gam.m1 <- sqrt( mean( (data.fcr.ts$FCR - pred.gam.m1)^2 ) )

# model that AvWeight predictor is linear
gam.m2 <- gam(formula= FCR ~ s(Temp, bs="cr") + AvWeight, data=data.fcr.tr)
summary.gam.m2 <-summary(gam.m2)
pred.gam.m2 <- predict(gam.m2, pred.data)
rmse.gam.m2 <- sqrt( mean( (data.fcr.ts$FCR - pred.gam.m2)^2 ) )

# model that both predictors are non-linear
gam.m3 <- gam(formula=FCR ~ s(Temp, bs="cr") + s(AvWeight, bs="cr"), family=gaussian(link=identity), data=data.fcr.tr)
summary.gam.m3 <-summary(gam.m3)
pred.gam.m3 <- predict(gam.m3, pred.data)
rmse.gam.m3 <- sqrt( mean( (data.fcr.ts$FCR - pred.gam.m3)^2 ) )

# model that both predictors are non-linear and interaction between them
# gam.m4 <- gam(formula = FCR ~ s(Temp) + s(AvWeight) + s(AvWeight,Temp), data=data.fcr.tr)  
# summary.gam.m4 <-summary(gam.m4)
# pred.gam.m4 <- predict(gam.m4, pred.data)
# rmse.gam.m4 <- sqrt( mean( (data.fcr.ts$FCR - pred.gam.m4)^2 ) )


# Comparing the models
res.cor.gam <- as.data.frame(matrix(nrow=0, ncol=5))
colnames(res.cor.gam) <- c("Adjusted.R.Squared", "Deviance.Explained", "AIC", "GCV", "RMSE")

models<-list()
if (gam.m1$converged){
  res.cor.gam[nrow(res.cor.gam)+1,] <- c( round(summary.gam.m1$r.sq,3), paste(round(summary.gam.m1$dev.expl*100,3),'%',sep=''), round(gam.m1$aic,3), round(gam.m1$gcv.ubre,3), round(rmse.gam.m1,3) )
  models <- list(gam.m1)
}
if (gam.m2$converged){
  res.cor.gam[nrow(res.cor.gam)+1,] <- c( round(summary.gam.m2$r.sq,3), paste(round(summary.gam.m2$dev.expl*100,3),'%',sep=''), round(gam.m2$aic,3), round(gam.m2$gcv.ubre,3), round(rmse.gam.m2,3) )
  models[[length(models)+1]] <- gam.m2
}
if (gam.m3$converged){
  res.cor.gam[nrow(res.cor.gam)+1,] <- c( round(summary.gam.m3$r.sq,3), paste(round(summary.gam.m3$dev.expl*100,3),'%',sep=''), round(gam.m3$aic,3), round(gam.m3$gcv.ubre,3), round(rmse.gam.m3,3) )
  models[[length(models)+1]] <- gam.m3
}
# if (gam.m4$converged){
#   res.cor.gam[nrow(res.cor.gam)+1,] <- c( round(summary.gam.m4$r.sq,3), paste(round(summary.gam.m4$dev.expl*100,3),'%',sep=''), round(gam.m4$aic,3), round(gam.m4$gcv.ubre,3), round(rmse.gam.m4,3) )
#   models[[length(models)+1]] <- gam.m4  
# }

row.names(res.cor.gam) <- c("gam.m1", "gam.m2", "gam.m3") #, "gam.m4")
print(res.cor.gam)

anv.gam.mods <- anova(gam.m1,gam.m2,gam.m3,test="F") #gam.m4,
print(anv.gam.mods)

#----------------------------------------------------------------------------------------------------------------

# find best model comparing Adjusted R-Squared (max), AIC (min) and RMSE (min)
pos = c( which.max(res.cor.gam$Adjusted.R.Squared), which.min(res.cor.gam$AIC), which.min(res.cor.gam$RMSE) )

if ( length(unique(pos)) == 1 )
{
  best.mod <- models[[max(pos)]]
}

# plot best model

# Get the matrix of s() functions
terms <- predict(best.mod, type="terms")

# Bind in terms with FCR and convert it to data frame
tframe <- cbind(FCR = data.fcr.tr$FCR, as.data.frame(terms))
colnames(tframe) <- gsub('[()]', '', colnames(tframe))

# Bind in the input variables 
pframe <- cbind(tframe, data.fcr.tr[,c("Temp","AvWeight")])

p1 <- ggplot(pframe, aes(x=Temp)) + geom_point(aes(y=scale(sTemp, scale=F))) + geom_smooth( aes(y=scale(FCR, scale=F)))
p2 <- ggplot(pframe, aes(x=AvWeight)) + geom_point(aes(y=scale(sAvWeight, scale=F))) + geom_smooth( aes(y=scale(FCR, scale=F)))

png("BestGamModel.png")
par(mfrow=c(1,2))
print(p1)
print(p2)
dev.off()

png("BestGamModel.Surface.png",width=600,height=400,units="px")
vis.gam(best.mod, view=c("Temp","AvWeight"),plot.type="persp",color="topo", theta=45)
dev.off()
png("BestGamModel.Contour.png",width=600,height=400,units="px")
vis.gam(best.mod, view=c("Temp","AvWeight"),plot.type="contour",color="topo")
dev.off()

#------------------------------------------------------------------------------------
# Predict new FCR values in range of Temperature and AvWeight values

Temp.vals <- seq(from=11, to=27, by=1)
#AvWeight.vals <- unique(c(data.fcr.tr$AvWeight,data.fcr.ts$AvWeight))
#AvWeight.vals <- seq(from=0, to=max(data.fcr$AvWeight), by=50)

#AvWeight.vals <- c( 2.25, 6, 14, 35, 75)
AvWeight.vals <- categories$AvgCategory

predicted.FCR <- predict(best.mod, newdata = expand.grid(Temp = Temp.vals, AvWeight = AvWeight.vals))

mat<-matrix(predicted.FCR,nrow =length(Temp.vals), ncol= length(AvWeight.vals))
mat <- t(mat)
colnames(mat)<-paste("Temp", Temp.vals,sep=" ")
#row.names(mat)<-paste("AvWeight", AvWeight.vals,sep=" ")
row.names(mat)<-categories$category
predFCR = as.data.frame(mat)
View(predFCR)
write.csv2(round(predFCR,3), file='FCR.Table.Surf.csv',row.names=TRUE )

#------------------------------------------------------------------------------------
# plot Surface of predFCR
library("lattice")

# persp(x=AvWeight.vals, y=Temp.vals, z=as.matrix(predFCR), phi = 45, theta = 45, 
#      xlab="Av Weight", ylab="Temperature", zlab="FCR", main = "Surface FCR prediction")

#----------------- second way
grid <- expand.grid(Temp = Temp.vals, AvWeight = AvWeight.vals)
grid[["Predict.FCR"]] <- predict(best.mod, newdata = grid)

png("Predict.BestGamModel.Surface.png")
wireframe(Predict.FCR ~ Temp * AvWeight, grid, outer = TRUE, shade = TRUE, 
          xlab="Temperature", ylab="Av Weight",zlab = "Predict FCR", zoom=1,
          drape = TRUE, colorkey = TRUE)
dev.off()
