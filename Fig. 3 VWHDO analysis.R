# VWHDO analysis
setwd("/Users/jiahuali1991/Dropbox/R/data/Lake Simcoe data/VWHDO")
VWHDO.analysis <- read.csv("VWHDO.analysis.csv",header=T)
head(VWHDO.analysis, 1)
summary(VWHDO.analysis)
colnames(VWHDO.analysis)

par(mfrow=c(1,2))

# Divided plots into 6 
par(mfrow=c(2,3))
#------------------------------------------------------
Figure 3A
# Plot all MVWHDO
VWHDO.analysis <- read.csv("VWHDO.analysis.csv",header=T)
plot(VWHDO.analysis$year, VWHDO.analysis$observed.MVWHDO, xlab="Year", ylab="VWHDOmin concentration (mg/L)",  type="p", xlim=c(1980,2012), ylim=c(1,6), cex=1.5, cex.main=1.6, cex.axis=1.2, cex.lab=1.5)

# calculate mean and standard deviation of minimum VWHDO during two peirod
mean(VWHDO.analysis$observed.MVWHDO[1:16])
mean(VWHDO.analysis$observed.MVWHDO[17:33])
t.test(VWHDO.analysis$observed.MVWHDO[1:16],VWHDO.analysis$observed.MVWHDO[17:33], paired=F)


# Plot two periods MVWHDO
VWHDO.analysis.pre <- VWHDO.analysis[which(VWHDO.analysis$year < 1996),]
VWHDO.analysis.post <- VWHDO.analysis[which(VWHDO.analysis$year >1995),]
plot(VWHDO.analysis.pre$year, VWHDO.analysis.pre $observed.MVWHDO, xlab="Year", ylab="VWHDOmin concentration (mg/L)",  type="p", xlim=c(1980,2012), ylim=c(1,6), cex.main=1.6, cex.axis=1.2, cex.lab=1.5) # main="VWHDO minimum observed",
points(VWHDO.analysis.post$year, VWHDO.analysis.post$observed.MVWHDO, col="red")
MVWHDO.post.lm <- lm(VWHDO.analysis.post$observed.MVWHDO ~ VWHDO.analysis.post$year)
summary(MVWHDO.post.lm)
# finite length of line using segment() function
n <- length(VWHDO.analysis.post$year)
segments(VWHDO.analysis.post$year[1],fitted(MVWHDO.post.lm)[1],VWHDO.analysis.post$year[n], fitted(MVWHDO.post.lm)[n], col="red")
# text(2012, 5.9, labels="A", cex=1.5)
Kendall(VWHDO.analysis.post $year, VWHDO.analysis.post $observed.MVWHDO)
#------------------------------------------------------------------
# Figure 3B depletion rate
setwd("/Users/jiahuali1991/Dropbox/R/data/Lake Simcoe data")
DR <- read.csv("DR and DR adjusted for each year.csv",header=T)
colnames(DR)
plot(DR[,1], DR[,3], xlab="Year", ylab="VWHDO depletion rate (mg/L day)",  type="l", cex.main=1.6, cex.axis=1.2, cex.lab=1.5, ylim=c(0.05, 0.14)) # main="VWHDO depletion rate",
lines(DR[,1], DR[,4], col="red")
legend("topleft", c("DR observed", "DR adjusted for temperature"),  col=c("black","red"), cex=1, lty=1)
# text(2012, 0.137, labels="B", cex=1.5)

DR.lm <- lm(DR[,3] ~ DR[,1])
summary(DR.lm)
#------------------------------------------------------------------
Figure 3C Spring VWHDO
colnames(VWHDO.analysis)
# Spring VWHDO
# Plot VWHDO on Julian day 160 
plot(VWHDO.analysis$year, VWHDO.analysis$VWHDO.concentration.at.Julian.day.160..Spring.HDO., xlab="Year", ylab="VWHDO(mg/L)", type="p", col="red", ylim=c(8, 17), cex.main=1.6, cex.axis=1.2, cex.lab=1.5) # , main="Spring VWHDO"
VWHDO.VWHDO_on_Julian_day_150.lm <- lm(VWHDO.analysis$VWHDO.concentration.at.Julian.day.160..Spring.HDO. ~ VWHDO.analysis$year)
abline(VWHDO.VWHDO_on_Julian_day_150.lm, col="red")
# finite length of line using segment() function
n <- length(VWHDO.analysis$year)
segments(VWHDO.analysis$year[1],fitted(VWHDO.VWHDO_on_Julian_day_150.lm)[1],VWHDO.analysis$year[n], fitted(VWHDO.VWHDO_on_Julian_day_150.lm)[n], col="red")
summary(VWHDO.VWHDO_on_Julian_day_150.lm)

library(Kendall)
Kendall(VWHDO.analysis$year, VWHDO.analysis$VWHDO.concentration.at.Julian.day.160..Spring.HDO.) # tau = 0.44, 2-sided pvalue =0.00043321

points(VWHDO.analysis$year, VWHDO.analysis$VWHDO.concentration.at.60.days.after.ice.out, xlab="Year", ylab="VWHDO(mg/L)", main="VWHDO on June 1st 1980-2012 K42", type="p", col="blue")
VWHDO.60.lm <- lm(VWHDO.analysis$VWHDO.concentration.at.60.days.after.ice.out ~ VWHDO.analysis$year)
abline(VWHDO.60.lm, col="blue")
# finite length of line using segment() function
n <- length(VWHDO.analysis$year)
segments(VWHDO.analysis$year[1],fitted(VWHDO.60.lm)[1],VWHDO.analysis$year[n], fitted(VWHDO.60.lm)[n], col="blue")
summary(VWHDO.60.lm)

Kendall(VWHDO.analysis$year, VWHDO.analysis$VWHDO.concentration.at.60.days.after.ice.out) # tau = tau = 0.531, 2-sided pvalue =2.8968e-05

legend("topleft", c("VWHDO concentration at Julian day 160", "VWHDO concentration at 60 days after ice-out"), lty=99,col=c("red","blue"), pch=1)
# text(2012, 16.5, labels="C", cex=1.5)


#----------------------------------------------------------------------------
Figure 3 D
# Plot Initial L vs. year
setwd("/Users/jiahuali1991/Dropbox/R/data/Lake Simcoe data/VWHDO")
VWHDO.analysis <- read.csv("VWHDO.analysis.csv",header=T)
colnames(VWHDO.analysis)

plot(VWHDO.analysis$year, VWHDO.analysis$VWHDO.concentration.at.onset.of.thermal.stratification, xlab="Year", ylab="Initial VWHDO (mg/L)", type="p", cex.main=1.6, cex.axis=1.2, cex.lab=1.5) # , main="Initial VWHDO"
VWHDO.analysis.lm <- lm(VWHDO.analysis$VWHDO.concentration.at.onset.of.thermal.stratification ~ VWHDO.analysis$year)
abline(VWHDO.analysis.lm)
summary(VWHDO.analysis.lm)
# text(2012, 15.8, labels="D", cex=1.5)
Kendall(VWHDO.analysis$year, VWHDO.analysis$VWHDO.concentration.at.onset.of.thermal.stratification)
#----------------------------------------------------------------------------
Figure 3 E
# Plot depletion period vs. Year
setwd("/Users/jiahuali1991/Dropbox/R/data/Lake Simcoe data/VWHDO")
VWHDO.analysis <- read.csv("VWHDO.analysis.csv",header=T)
colnames(VWHDO.analysis)

plot(VWHDO.analysis$year, VWHDO.analysis$number.of.days.between.onset.thermal.stratification.and.MVWHDO, xlab="Year", ylab="L (days)", type="p",   cex.main=1.6, cex.axis=1.2, cex.lab=1.5) # , main="L"
VWHDO.analysis.lm <- lm(VWHDO.analysis$number.of.days.between.onset.thermal.stratification.and.MVWHDO ~ VWHDO.analysis$year)
abline(VWHDO.analysis.lm)
# text(2012, 135, labels="E", cex=1.5)
summary(VWHDO.analysis.lm)
Kendall(VWHDO.analysis$year, VWHDO.analysis$number.of.days.between.onset.thermal.stratification.and.MVWHDO)
#----------------------------------------------------------------


# Figure 3F
# Minimum of predicted and observed ice-free period minimum VWHDO after 1996
VWHDO.analysis <- read.csv("VWHDO.analysis.csv",header=T)

# Compare predicted and observed ice-free period minimum VWHDO
plot(VWHDO.analysis$observed.MVWHDO, VWHDO.analysis$predicted.MVWHDO.based.on.MVWHDO.date, xlab="Observed VWHDOmin (mg/L)", ylab="Predicted VWHDOmin (mg/L)", type="p", cex.main=1.6, cex.axis=1.2, cex.lab=1.5) # , main="Observed vs. Predicted VWHDOmin"
MWHDO.lm <- lm(VWHDO.analysis$predicted_MVWHDO ~ VWHDO.analysis$observed_MVWHDO)
abline(MWHDO.lm)
abline(0,1)
text(5, 4, labels="1:1 line", cex=1.5, col="red")
# text(5.9, 5.5, labels="F", cex=1.5)

#------------------------------------

#下面这些为了改图

# Divided plots into 6 
par(mfrow=c(2,3))
#------------------------------------------------------
Figure 3A
# Plot all MVWHDO
VWHDO.analysis <- read.csv("VWHDO.analysis.csv",header=T)

# calculate mean and standard deviation of minimum VWHDO during two peirod
mean(VWHDO.analysis$observed.MVWHDO[1:17])
mean(VWHDO.analysis$observed.MVWHDO[18:33])
t.test(VWHDO.analysis$observed.MVWHDO[1:17],VWHDO.analysis$observed.MVWHDO[18:33], paired=F)


# Plot two periods MVWHDO
VWHDO.analysis.pre <- VWHDO.analysis[which(VWHDO.analysis$year < 1997),]
VWHDO.analysis.post <- VWHDO.analysis[which(VWHDO.analysis$year >1996),]
plot(VWHDO.analysis.pre$year, VWHDO.analysis.pre $observed.MVWHDO, xlab="Year", ylab="VWHDOmin concentration (mg/L)",  type="p", xlim=c(1980,2012), ylim=c(1,6), cex.axis=1.5, cex.lab=1.5) # main="VWHDO minimum observed",
points(VWHDO.analysis.post$year, VWHDO.analysis.post$observed.MVWHDO, col="red")
MVWHDO.post.lm <- lm(VWHDO.analysis.post$observed.MVWHDO ~ VWHDO.analysis.post$year)
summary(MVWHDO.post.lm)
# finite length of line using segment() function
n <- length(VWHDO.analysis.post$year)
segments(VWHDO.analysis.post$year[1],fitted(MVWHDO.post.lm)[1],VWHDO.analysis.post$year[n], fitted(MVWHDO.post.lm)[n], col="red")
text(2012, 5.9, labels="A", cex=1.5)
Kendall(VWHDO.analysis.post $year, VWHDO.analysis.post $observed.MVWHDO)
#------------------------------------------------------------------
# Figure 3B depletion rate
setwd("/Users/jiahuali1991/Dropbox/R/data/Lake Simcoe data")
DR <- read.csv("DR and DR adjusted for each year.csv",header=T)
colnames(DR)
plot(DR[,1], DR[,3], xlab="Year", ylab="VWHDO depletion rate (mg/L day)",  type="l", cex.axis=1.5, cex.lab=1.5, ylim=c(0.05, 0.14)) # main="VWHDO depletion rate",
lines(DR[,1], DR[,4], col="red")
legend("topleft", c("DR observed", "DR adjusted for hypolimnetic water temperature rate at 10˚C"),  col=c("black","red"), cex=1, lty=1)
text(2012, 0.137, labels="B", cex=1.5)

DR.lm <- lm(DR[,3] ~ DR[,1])
summary(DR.lm)
#------------------------------------------------------------------
Figure 3C Spring VWHDO
colnames(VWHDO.analysis)
# Spring VWHDO
# Plot VWHDO on Julian day 160 
plot(VWHDO.analysis$year, VWHDO.analysis$VWHDO.concentration.at.Julian.day.160..Spring.HDO., xlab="Year", ylab="VWHDO(mg/L)", type="p", col="red", ylim=c(8, 17), cex.axis=1.5, cex.lab=1.5) # , main="Spring VWHDO"
VWHDO.VWHDO_on_Julian_day_150.lm <- lm(VWHDO.analysis$VWHDO.concentration.at.Julian.day.160..Spring.HDO. ~ VWHDO.analysis$year)
abline(VWHDO.VWHDO_on_Julian_day_150.lm, col="red")
# finite length of line using segment() function
n <- length(VWHDO.analysis$year)
segments(VWHDO.analysis$year[1],fitted(VWHDO.VWHDO_on_Julian_day_150.lm)[1],VWHDO.analysis$year[n], fitted(VWHDO.VWHDO_on_Julian_day_150.lm)[n], col="red")
summary(VWHDO.VWHDO_on_Julian_day_150.lm)

library(Kendall)
Kendall(VWHDO.analysis$year, VWHDO.analysis$VWHDO.concentration.at.Julian.day.160..Spring.HDO.) # tau = 0.44, 2-sided pvalue =0.00043321

points(VWHDO.analysis$year, VWHDO.analysis$VWHDO.concentration.at.60.days.after.ice.out, xlab="Year", ylab="VWHDO(mg/L)", main="VWHDO on June 1st 1980-2012 K42", type="p", col="blue")
VWHDO.60.lm <- lm(VWHDO.analysis$VWHDO.concentration.at.60.days.after.ice.out ~ VWHDO.analysis$year)
abline(VWHDO.60.lm, col="blue")
# finite length of line using segment() function
n <- length(VWHDO.analysis$year)
segments(VWHDO.analysis$year[1],fitted(VWHDO.60.lm)[1],VWHDO.analysis$year[n], fitted(VWHDO.60.lm)[n], col="blue")
summary(VWHDO.60.lm)

Kendall(VWHDO.analysis$year, VWHDO.analysis$VWHDO.concentration.at.60.days.after.ice.out) # tau = tau = 0.531, 2-sided pvalue =2.8968e-05

legend("topleft", c("VWHDO concentration at Julian day 160", "VWHDO concentration at 60 days after ice-out"), lty=99,col=c("red","blue"), pch=1)
text(2012, 16.8, labels="C", cex=1.5)


#----------------------------------------------------------------------------
Figure 3 D
# Plot Initial L vs. year
setwd("/Users/jiahuali1991/Dropbox/R/data/Lake Simcoe data/VWHDO")
VWHDO.analysis <- read.csv("VWHDO.analysis.csv",header=T)
colnames(VWHDO.analysis)

plot(VWHDO.analysis$year, VWHDO.analysis$VWHDO.concentration.at.onset.of.thermal.stratification, xlab="Year", ylab="Initial VWHDO (mg/L)", type="p", cex.axis=1.5, cex.lab=1.5) # , main="Initial VWHDO"
VWHDO.analysis.lm <- lm(VWHDO.analysis$VWHDO.concentration.at.onset.of.thermal.stratification ~ VWHDO.analysis$year)
abline(VWHDO.analysis.lm)
summary(VWHDO.analysis.lm)
text(2012, 15.8, labels="D", cex=1.5)
Kendall(VWHDO.analysis$year, VWHDO.analysis$VWHDO.concentration.at.onset.of.thermal.stratification)
#----------------------------------------------------------------------------
Figure 3 E
# Plot depletion period vs. Year
setwd("/Users/jiahuali1991/Dropbox/R/data/Lake Simcoe data/VWHDO")
VWHDO.analysis <- read.csv("VWHDO.analysis.csv",header=T)
colnames(VWHDO.analysis)

plot(VWHDO.analysis$year, VWHDO.analysis$number.of.days.between.onset.thermal.stratification.and.MVWHDO, xlab="Year", ylab="L (days)", type="p",   cex.main=1.6, cex.axis=1.2, cex.lab=1.5) # , main="L"
VWHDO.analysis.lm <- lm(VWHDO.analysis$number.of.days.between.onset.thermal.stratification.and.MVWHDO ~ VWHDO.analysis$year)
abline(VWHDO.analysis.lm)
text(2012, 135, labels="E", cex=1.5)
summary(VWHDO.analysis.lm)
Kendall(VWHDO.analysis$year, VWHDO.analysis$number.of.days.between.onset.thermal.stratification.and.MVWHDO)
#----------------------------------------------------------------


# Figure 3F
# Minimum of predicted and observed ice-free period minimum VWHDO after 1996
VWHDO.analysis <- read.csv("VWHDO.analysis.csv",header=T)

# Compare predicted and observed ice-free period minimum VWHDO
plot(VWHDO.analysis$observed.MVWHDO, VWHDO.analysis$predicted.MVWHDO.based.on.MVWHDO.date, xlab="Observed VWHDOmin (mg/L)", ylab="Predicted VWHDOmin (mg/L)", type="p", cex.axis=1.5, cex.lab=1.5) # , main="Observed vs. Predicted VWHDOmin"
MWHDO.lm <- lm(VWHDO.analysis$predicted_MVWHDO ~ VWHDO.analysis$observed_MVWHDO)
abline(MWHDO.lm)
abline(0,1)
text(5, 4, labels="1:1 line", cex=1.5, col="red")
text(5.9, 5.5, labels="F", cex=1.5)