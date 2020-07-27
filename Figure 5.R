# 1 Plot Depletion period
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Paper work/!Thesis/Part 1 VWHDO analysis/csv files")
MLR <- read.csv("Multiple Linear Regression data.csv", header=T)
colnames(MLR)

# Plot 
plot(MLR$Year, MLR$IS800, xlab="Year", ylab="peiord between ice-out and S=800 (days)",  type="p", xlim=c(1980,2012), ylim=c(40,90), cex=1.5, cex.main=1.6, cex.axis=1.2, cex.lab=1.5)

# Plot two periods MVWHDO
VWHDO.analysis.pre <- VWHDO.analysis[which(VWHDO.analysis$year < 1997),]
VWHDO.analysis.post <- VWHDO.analysis[which(VWHDO.analysis$year >1996),]
plot(VWHDO.analysis.pre$year, VWHDO.analysis.pre $observed.MVWHDO, xlab="Year", ylab="VWHDOmin concentration (mg/L)",  type="p", xlim=c(1980,2012), ylim=c(1,6), cex.main=1.6, cex.axis=1.2, cex.lab=1.5) # main="VWHDO minimum observed",
points(VWHDO.analysis.post$year, VWHDO.analysis.post$observed.MVWHDO, col="red")
MVWHDO.post.lm <- lm(VWHDO.analysis.post$observed.MVWHDO ~ VWHDO.analysis.post$year)
summary(MVWHDO.post.lm)
# finite length of line using segment() function
n <- length(VWHDO.analysis.post$year)
segments(VWHDO.analysis.post$year[1],fitted(MVWHDO.post.lm)[1],VWHDO.analysis.post$year[n], fitted(MVWHDO.post.lm)[n], col="red")

x <- 1980:2012
y <- MLR$IS800

breaks <- 1997:2000
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

# Plot piecewise regression
plot(x, y, ylim=c(40,100), pch=16, main="peiord between ice-out and S=800", xlab="Year", ylab="peiord between ice-out and S=800 (days)", cex=1.5, cex.main=1.6, cex.axis=1.2, cex.lab=1.5)
curve((piecewise2$coefficients[1]+piecewise2$coefficients[3]) + (piecewise2$coefficients[2]+piecewise2$coefficients[5])*x, add=T, from=1, to=breaks[which(mse==min(mse))])
curve((piecewise2$coefficients[1]+piecewise2$coefficients[4]) + piecewise2$coefficients[2]*x, add=T, from=breaks[which(mse==min(mse))], to=max(x))
abline(v=breaks[which(mse==min(mse))], lty=3)