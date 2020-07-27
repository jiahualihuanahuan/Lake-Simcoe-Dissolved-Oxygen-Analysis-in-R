# Before modelling, load all data required
# Load data
#setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Data Analysis/Data/VWHDO/csv files")
setwd("C:/Users/jiahu_000/Dropbox/Jiahua's MSc thesis/Data Analysis/Data/VWHDO/csv files")

VWHDO <- read.csv("Multiple linear regression model data.csv", header=T)

colnames(VWHDO)
rownames(VWHDO)
DM.f <- factor(VWHDO$DM)
VWHDO <- cbind(VWHDO,DM.f)
summary(VWHDO)
colnames(VWHDO)

#-------------------
# 1 Model VWHDO min
library(packfor)
# forward.sel(VWHDO[,11], cbind(VWHDO.log[,-c(1:14,23:43,54,60, 61:77)],VWHDO[,-c(1:11,13:14,60, 61:77)]), nperm=9999, alpha=0.05)

forward.sel(VWHDO[,11], VWHDO[,-c(2,3,7,8,11,60)], nperm=9999, alpha=0.05)

# model 1:VWHDOmin ~ DM + VWHT
VWHDOmin.lm <- lm(VWHDO$MVWHDO ~ VWHDO$DM.f + VWHDO$VWHT)
summary(VWHDOmin.lm)

#-------------------
2 Model depletion rate
colnames(VWHDO)
# forward.sel(VWHDO[,3], VWHDO[,-c(1:11,13,60:77)], nperm=9999, alpha=0.05)

# forward.sel(VWHDO[,3], VWHDO[,c(24,52,54,64:67)], nperm=9999, alpha=0.05)


#  model VWHDO depletion rate including annual TP loading
forward.sel(VWHDO[11:32,3], VWHDO[11:32,-c(1:11,13,61:72)], nperm=9999, alpha=0.05)

r.lm <- lm(VWHDO[11:32,3] ~ VWHDO[11:32,]$NH4 + VWHDO[11:32,]$Annual.TP.loading)
summary(r.lm)

# plot(VWHDO[11:32,1],VWHDO[11:32,]$NH4)
# plot(VWHDO[11:32,1],VWHDO[11:32,]$Annual.TP.loading)
# colnames(VWHDO)

#-------
if we take staggered effect of TP loading into account

# Load data
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Data Analysis/Data/VWHDO/csv files")
VWHDO <- read.csv("Multiple linear regression model data.csv", header=T)

# set working directory: phytoplankton annual
# setwd("C:/Users/jiahu_000/Dropbox/Jiahua's MSc thesis/Data Analysis/Data/VWHDO/csv files")


VWHDO <- read.csv("Multiple linear regression model data staggered.csv", header=T)

#  model VWHDO depletion rate including annual TP loading
colnames(VWHDO)
forward.sel(VWHDO[12:33,3], VWHDO[12:33,-c(1:11,13,23:45,49:51, 61:72)], nperm=9999, alpha=0.1)





#-------------------
# 3. model VWHDO depletion period (L)
colnames(VWHDO)
forward.sel(VWHDO[,14], VWHDO[,-c(1:14,16:22,54:60)], nperm=9999, alpha=0.05)

# for the DO paper
l.lm <- lm(VWHDO[,14] ~ VWHDO$Air9 + VWHDO$Air6)
summary(l.lm)


# correct but with spurious correlation
l.lm <- lm(VWHDO[,14] ~ VWHDO$Duration.of.thermal.stratification + VWHDO$timing.of.MVWHDO.occur.in.fall)
summary(l.lm)



plot(1980:2012, VWHDO$Summer.Air.June.Sep)
SA.l <- lm(VWHDO$Summer.Air.June.Sep ~ VWHDO$year)
summary(SA.l)
abline()

plot(1980:2012, VWHDO$Air9)
A9.l <- lm(VWHDO$Air9 ~ VWHDO$year)
summary(A9.l)

plot(1980:2012, VWHDO$Air6)
A6.l <- lm(VWHDO$Air6 ~ VWHDO$year)
summary(A6.l)

#-------------------
# 3.333333333 Model depletion period (L)
# beofre model L, calculate summer air temperature in degree days
# Load data
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 1 VWHDO analysis/csv files")
Sum.Air <- read.csv("Summer Air Temperature Shanty Bay.csv", header=T)

air.temp <- matrix(nrow=33, ncol=1)
for (i in 1:33){
	air.temp[i,] <- sum(Sum.Air[which(Sum.Air[,2]==1979+i),][,5], rm.na=T)
}

write.csv(air.temp, "my.file.csv")

# 1 Model Depletion period
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 1 VWHDO analysis/csv files")
MLR <- read.csv("Multiple Linear Regression data.csv", header=T)
colnames(MLR)

DP <- lm(MLR$L~MLR$Degree.Day)
summary(DP)

# 2 Model Initial VWHDO
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 1 VWHDO analysis/csv files")
MLR <- read.csv("Multiple Linear Regression data.csv", header=T)
colnames(MLR)

IVWHDO <- lm(MLR$Ini.VWHDO ~ MLR$TP + MLR$ZM + MLR$IS800)
summary(IVWHDO)

# 3 Piecewise regression using "segmented" package
library(segmented)
# 3.1 model minimum VWHDO
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 1 VWHDO analysis/csv files")
MLR <- read.csv("Multiple Linear Regression data.csv", header=T)
colnames(MLR)

x <- MLR$Year
y <- MLR$MVWHDO

lin.mod <- lm(y ~ x)
segmented.mod <- segmented(lin.mod, seg.Z = ~ x, psi=1990)
summary(segmented.mod)
plot(x,y,pch=16, ylim=c(0,5))
plot(segmented.mod, add=T)

# 3.2 model period of time (days) between ice-out and onset of thermal stratification
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 1 VWHDO analysis/csv files")
MLR <- read.csv("Multiple Linear Regression data.csv", header=T)
colnames(MLR)
x <- MLR$Year
y <- MLR$IS800

lin.mod <- lm(y ~ x)
segmented.mod <- segmented(lin.mod, seg.Z = ~ x, psi=1990)
summary(segmented.mod)
plot(x,y,pch=16, ylim=c(40,90))
plot(segmented.mod, add=T)

# 4 Piecewise regression using Iterative searching: brute force iterative approaches--estimate the breakpoint statistically
# 4.1 model minimum VWHDO
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 1 VWHDO analysis/csv files")
MLR <- read.csv("Multiple Linear Regression data.csv", header=T)
colnames(MLR)

x <- MLR$Year
y <- MLR$MVWHDO

breaks <- 1980:2012
mse <- numeric(length(breaks))
for (i in 1:length(breaks)){
	piecewise1 <- lm(y ~ x*(x<breaks[i]) + x* (x>=breaks[i]))
	mse[i] <- summary(piecewise1)[6]
}

mse <- as.numeric(mse)

plot(breaks, mse, type="l")

# pick the point with the lowest error (mes)
breaks[which(mse==min(mse))]

# Run final model with testimated breakpoint

piecewise2 <- lm(y ~ (x*(x<breaks[which(mse==min(mse))])) + (x* (x>breaks[which(mse==min(mse))])))
summary(piecewise2)
str(piecewise2)

# Plot piecewise regression
plot(x, y, ylim=c(1,6), pch=16, main="VWHDO minimum concentration in Lake Simcoe K42 from 1980 to 2012", xlab="Year", ylab="VWHDOmin concentration (mg/L)", cex=1.5, cex.main=1.6, cex.axis=1.2, cex.lab=1.5)
curve((piecewise2$coefficients[1]+piecewise2$coefficients[3]) + (piecewise2$coefficients[2]+piecewise2$coefficients[5])*x, add=T, from=1, to=breaks[which(mse==min(mse))])
curve((piecewise2$coefficients[1]+piecewise2$coefficients[4]) + piecewise2$coefficients[2]*x, add=T, from=breaks[which(mse==min(mse))], to=max(x))
abline(v=breaks[which(mse==min(mse))], lty=3)

# 4.2 model period of time (days) between ice-out and onset of thermal stratification
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 1 VWHDO analysis/csv files")
MLR <- read.csv("Multiple Linear Regression data.csv", header=T)
colnames(MLR)
x <- MLR$Year
y <- MLR$IS800

breaks <- x[which(x >=1990 & x <=2000)]
mse <- numeric(length(breaks))
for (i in 1:length(breaks)){
	piecewise1 <- lm(y ~ x*(x<breaks[i]) + x* (x>=breaks[i]))
	mse[i] <- summary(piecewise1)[6]
}

mse <- as.numeric(mse)

plot(breaks, mse, type="l")

# pick the point with the lowest error (mes)
breaks[which(mse==min(mse))]

# Run final model with testimated breakpoint

piecewise2 <- lm(y ~ (x*(x<breaks[which(mse==min(mse))])) + (x* (x>breaks[which(mse==min(mse))])))
summary(piecewise2)
str(piecewise2)

# Plot piecewise regression
plot(x, y, ylim=c(40,90), pch=16, main="# of days between ice-out and onset thermal stratification", xlab="Year", ylab="Days", cex=1.5, cex.main=1.6, cex.axis=1.2, cex.lab=1.5)
curve((piecewise2$coefficients[1]+piecewise2$coefficients[3]) + (piecewise2$coefficients[2]+piecewise2$coefficients[5])*x, add=T, from=1, to=breaks[which(mse==min(mse))])
curve((piecewise2$coefficients[1]+piecewise2$coefficients[4]) + piecewise2$coefficients[2]*x, add=T, from=breaks[which(mse==min(mse))], to=max(x))
abline(v=breaks[which(mse==min(mse))], lty=3)

#----------------
# 4.1 Model spring VWHDO (@ when S=800) 
colnames(VWHDO)
forward.sel(VWHDO[,4], VWHDO[,c(7,19)], nperm=9999, alpha=0.05)


VWHDOS800.lm <- lm(VWHDO$Initial.VWHDO.when.S.800 ~ VWHDO$Spring.VWHDO.at.160.julian.day + VWHDO$onset.of.thermal.stratification)
summary(VWHDOS800.lm)


# 4.2. model spring VWHDO (day of the year 160) 
colnames(VWHDO)
forward.sel(VWHDO[,7], VWHDO[,-c(1:14,19:21,24:26,45,55,60)], nperm=9999, alpha=0.05)
VWHDO160.lm <- lm(VWHDO[,7] ~ DM + TP + ice.off.date..day.of.the.year., data=VWHDO)
summary(VWHDO160.lm)

# 4.2. model spring VWHDO (day of the year 160) 
forward.sel(VWHDO[,7], VWHDO[,-c(1:10,12:13,20,21,39:43,60,61:78)], nperm=9999, alpha=0.05)

# 4.3. model spring VWHDO (day of the year 180)
forward.sel(VWHDO[,8], VWHDO[,-c(1:10,12:13,20,60)], nperm=9999, alpha=0.05)

# 4.4. model spring VWHDO (60 days after ice-out)
forward.sel(VWHDO[,9], VWHDO[,-c(1:10,60)], nperm=9999, alpha=0.05)