setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Data Analysis/Data/Temperature profile and Stability/csv files")
ther.stra <-read.csv("Schmit's Stability Analysis.csv",header=T)
head(ther.stra)
colnames(ther.stra)

# Mann-Kendall trend test
library("Kendall")
Kendall(ther.stra$Year, ther.stra$Onset)
Kendall(ther.stra$Year, ther.stra$Overturn)
Kendall(ther.stra$Year, ther.stra$Duration)
Kendall(ther.stra$Year, ther.stra$S.in)
Kendall(ther.stra$Year, ther.stra$S.de) # non-significant
Kendall(ther.stra$Year, ther.stra$Max.S) # non-significant
Kendall(ther.stra$Year, ther.stra$Average.S)

# fit in linear regression
onset.lm <- lm(ther.stra$Onset ~ ther.stra$Year)
summary(onset.lm)
overturn.lm <- lm(ther.stra$Overturn ~ ther.stra$Year)
summary(overturn.lm)
duration.lm <- lm(ther.stra$Duration ~ ther.stra$Year)
summary(duration.lm)
increase_rate_150.lm <- lm(ther.stra$S.in ~ ther.stra$Year)
summary(increase_rate_150.lm)
decrease_rate.lm <- lm(ther.stra$S.de ~ ther.stra$Year)
summary(decrease_rate.lm)
average.lm <- lm(ther.stra$Average.S ~ ther.stra$Year)
summary(average.lm)
max.lm <- lm(ther.stra$Max.S ~ ther.stra$Year)
summary(max.lm)
#divide the plot window into 8 frames
par(mfrow=c(2,4))
# Plot data 1980-2012 thermal stratification K42

# 1. Timing of onset of thermal stratficiation
plot(ther.stra$Year, ther.stra$Onset, type="l", xlab="Year", ylab="Day of the Year",  cex.main=1.6, cex.axis=1.2, cex.lab=1.5, ps=1.5)#main="Onset timing",
# finite length of line using segment() function
n <- length(ther.stra$Year)
segments(ther.stra$Year[1],fitted(onset.lm)[1],ther.stra$Year[n], fitted(onset.lm)[n], col="red")
text(1983, 188, labels="A", cex=2)

# 2. Timing of fall overturn
plot(ther.stra$Year, ther.stra$Overturn, type="l", xlab="Year", ylab="Day of the Year",  cex.main=1.6, cex.axis=1.2, cex.lab=1.5, ps=1.5) # main="Fall overturn timing",
# finite length of line using segment() function
n <- length(ther.stra$Year)
segments(ther.stra$Year[1],fitted(overturn.lm)[1],ther.stra$Year[n], fitted(overturn.lm)[n], col="red")
text(1983, 297, labels="B", cex=2)

# 3. Duration of thermal stratification
plot(ther.stra$Year, ther.stra$Duration, type="l", xlab="Year", ylab="Days",  cex.main=1.6, cex.axis=1.2, cex.lab=1.5, ps=1.5) # main="Duration",
# finite length of line using segment() function
n <- length(ther.stra$Year)
segments(ther.stra$Year[1],fitted(duration.lm)[1],ther.stra$Year[n], fitted(duration.lm)[n], col="red")
text(1983, 141, labels="C", cex=2)

# 4. Average Schmidt's stability during thermal stratification
plot(ther.stra$Year, ther.stra$Average.S, type="l", xlab="Year", ylab="Schmidt's stability index (g cm cm-2)",  cex.main=1.6, cex.axis=1.2, cex.lab=1.5, ps=1.5) # main="Mean S during stratified period",
# finite length of line using segment() function
n <- length(ther.stra$Year)
segments(ther.stra$Year[1],fitted(average.lm)[1],ther.stra$Year[n], fitted(average.lm)[n], col="red")
text(1983, 1810, labels="D", cex=2)

# 5. Instantaneous rate S increase on 150 day of the year
plot(ther.stra$Year, ther.stra$S.in, type="l", xlab="Year", ylab="Rate of S increase (g cm cm-2 day-1)",  cex.main=1.6, cex.axis=1.2, cex.lab=1.5, ps=1.5) # main="Rate of S increase",
# finite length of line using segment() function
n <- length(ther.stra$Year)
segments(ther.stra$Year[1],fitted(increase_rate_150.lm)[1],ther.stra$Year[n], fitted(increase_rate_150.lm)[n], col="red")
text(1983, 48, labels="E", cex=2)

# 6. Rate of S decrease in Lake Simcoe K42
plot(ther.stra$Year, ther.stra$S.de, type="l", xlab="Year", ylab="rate of S decrease (g cm cm-2 day-1)", cex.main=1.6, cex.axis=1.2, cex.lab=1.5, ps=1.5) # , main="Rate of S decrease"
# finite length of line using segment() function
n <- length(ther.stra$Year)
segments(ther.stra$Year[1],fitted(decrease_rate.lm)[1],ther.stra$Year[n], fitted(decrease_rate.lm)[n], col="red")
text(1983, 80, labels="F", cex=2)

# S max during the year in Lake Simcoe K42
plot(ther.stra$Year, ther.stra$Max.S, type="l", xlab="Year", ylab="Schmidt's stability index (g cm cm-2)", cex.main=1.6, cex.axis=1.2, cex.lab=1.5, ps=1.5) # , main="Maximum of S"
# finite length of line using segment() function
n <- length(ther.stra$Year)
segments(ther.stra$Year[1],fitted(max.lm)[1],ther.stra$Year[n], fitted(max.lm)[n], col="red")
text(1983, 2600, labels="G", cex=2)
#------------------------------------

#下面这些为了改图
#divide the plot window into 6 frames
par(mfrow=c(2,3))
# 1. Timing of onset of thermal stratficiation
plot(ther.stra$Year, ther.stra$Onset, type="l", xlab="Year", ylab="Day of the Year", cex.axis=1.5, cex.lab=1.5, ps=1.5)#main="Onset timing",
# finite length of line using segment() function
n <- length(ther.stra$Year)
segments(ther.stra$Year[1],fitted(onset.lm)[1],ther.stra$Year[n], fitted(onset.lm)[n], col="red")
text(1983, 188, labels="A", cex=2)

# 2. Timing of fall overturn
plot(ther.stra$Year, ther.stra$Overturn, type="l", xlab="Year", ylab="Day of the Year", cex.axis=1.5, cex.lab=1.5, ps=1.5) # main="Fall overturn timing",
# finite length of line using segment() function
n <- length(ther.stra$Year)
segments(ther.stra$Year[1],fitted(overturn.lm)[1],ther.stra$Year[n], fitted(overturn.lm)[n], col="red")
text(1983, 297, labels="B", cex=2)

# 3. Duration of thermal stratification
plot(ther.stra$Year, ther.stra$Duration, type="l", xlab="Year", ylab="Days", cex.axis=1.5, cex.lab=1.5, ps=1.5) # main="Duration",
# finite length of line using segment() function
n <- length(ther.stra$Year)
segments(ther.stra$Year[1],fitted(duration.lm)[1],ther.stra$Year[n], fitted(duration.lm)[n], col="red")
text(1983, 141, labels="C", cex=2)

# 4. Average Schmidt's stability during thermal stratification
plot(ther.stra$Year, ther.stra$Average.S, type="l", xlab="Year", ylab="Schmidt's Stability Index (g cm cm-2)", cex.axis=1.5, cex.lab=1.5, ps=1.5) # main="Mean S during stratified period",
# finite length of line using segment() function
n <- length(ther.stra$Year)
segments(ther.stra$Year[1],fitted(average.lm)[1],ther.stra$Year[n], fitted(average.lm)[n], col="red")
text(1983, 1810, labels="D", cex=2)

# 5. Instantaneous rate S increase on 150 day of the year
plot(ther.stra$Year, ther.stra$S.in, type="l", xlab="Year", ylab="Rate of S Increase (g cm cm-2 day-1)", cex.axis=1.5, cex.lab=1.5, ps=1.5) # main="Rate of S increase",
# finite length of line using segment() function
n <- length(ther.stra$Year)
segments(ther.stra$Year[1],fitted(increase_rate_150.lm)[1],ther.stra$Year[n], fitted(increase_rate_150.lm)[n], col="red")
text(1983, 48, labels="E", cex=2)

# S max during the year in Lake Simcoe K42
plot(ther.stra$Year, ther.stra$Max.S, type="l", xlab="Year", ylab="Schmidt's Stability Index (g cm cm-2)", cex.main=1.6, cex.axis=1.5, cex.lab=1.5, ps=1.5) # , main="Maximum of S"
# finite length of line using segment() function
n <- length(ther.stra$Year)
segments(ther.stra$Year[1],fitted(max.lm)[1],ther.stra$Year[n], fitted(max.lm)[n], col="red")
text(1983, 2600, labels="F", cex=2)
