# Figure 5
# Compare different criteria for hypolimnion with VWHDO analysis
par(mfrow=c(2,2))
# Load data
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Data Analysis/Data/VWHDO/csv files")
VWHDO <- read.csv("Table 5 different hypolimnion analysis.csv", header=T)
rownames(VWHDO)
colnames(VWHDO)

mean(VWHDO[c(1:17),2])
mean(VWHDO[c(18:33),2])


# Plot 1 minimum DO
plot(VWHDO[,1], VWHDO[,2], type="l",lty=1, ylim=c(0,7), xlab="Year", ylab="ice-free minimum VWHDO (mg/L)", cex.main=1.6, cex.axis=1.2, cex.lab=1.5,)
lines(VWHDO[,1], VWHDO[,3], type="l",lty=2)
lines(VWHDO[,1], VWHDO[,4], type="l",lty=3)
lines(VWHDO[,1], VWHDO[,5], type="l",lty=4)
lines(VWHDO[,1], VWHDO[,6], type="l",lty=6)

legend("topright", c("18m", "20m", "25m", "30m", "35m"), lty=c(1,2,3,4,6), cex=1)


# Plot 2 DR
plot(VWHDO[,1], VWHDO[,12], type="l",lty=1, ylim=c(0.04,0.12), xlab="Year", ylab="VWHDO depletion rate (mg/L day)", cex.main=1.6, cex.axis=1.2, cex.lab=1.5,)
lines(VWHDO[,1], VWHDO[,13], type="l",lty=2)
lines(VWHDO[,1], VWHDO[,14], type="l",lty=3)
lines(VWHDO[,1], VWHDO[,15], type="l",lty=4)
lines(VWHDO[,1], VWHDO[,16], type="l",lty=6)

legend("topright", c("18m", "20m", "25m", "30m", "35m"), lty=c(1,2,3,4,6), cex=1)

# Spring/initial DO
plot(VWHDO[,1], VWHDO[,7], type="l",lty=1, ylim=c(7,17), xlab="Year", ylab="Initial VWHDO (mg/L)", cex.main=1.6, cex.axis=1.2, cex.lab=1.5,)
lines(VWHDO[,1], VWHDO[,8], type="l",lty=2)
lines(VWHDO[,1], VWHDO[,9], type="l",lty=3)
lines(VWHDO[,1], VWHDO[,10], type="l",lty=4)
lines(VWHDO[,1], VWHDO[,11], type="l",lty=6)

legend("topright", c("18m", "20m", "25m", "30m", "35m"), lty=c(1,2,3,4,6), cex=1)

# DO depletion period
# Spring/initial DO
plot(VWHDO[,1], VWHDO[,17], type="l",lty=1, ylim=c(70,160), xlab="Year", ylab="Depletion period (days)", cex.main=1.6, cex.axis=1.2, cex.lab=1.5,)
lines(VWHDO[,1], VWHDO[,18], type="l",lty=2)
lines(VWHDO[,1], VWHDO[,19], type="l",lty=3)
lines(VWHDO[,1], VWHDO[,20], type="l",lty=4)
lines(VWHDO[,1], VWHDO[,21], type="l",lty=6)

legend("topright", c("18m", "20m", "25m", "30m", "35m"), lty=c(1,2,3,4,6), cex=1)







