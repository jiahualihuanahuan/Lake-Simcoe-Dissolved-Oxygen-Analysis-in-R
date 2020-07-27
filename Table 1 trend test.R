# VWHDO paper
# Table 1, trend test, Mann-Kendall


# before doing trend test, load all data required
# Load data
setwd("D:/Jiahua's MSc thesis/Publications/DO PAPER/csv")
VWHDO <- read.csv("Table 1.csv", header=T)
colnames(VWHDO)
rownames(VWHDO)
summary(VWHDO)
VWHDO[,1]
#-------------------
# Laod Mann-Kendall trend test package
library("Kendall")
library("trend")
#-------------------
# 1 trend test for VWHDO minimum
VWHDOmin.lm <- lm(VWHDO[,12] ~ VWHDO[,1])
summary(VWHDOmin.lm)
plot(VWHDO[,c(1,12)], xlab="year")
abline(VWHDOmin.lm, col="red")
tau <- Kendall(VWHDO[,1], VWHDO[,12])[1]
p <- Kendall(VWHDO[,1], VWHDO[,12])[2]

VWHDOmin.ts <- ts(VWHDO[,12],start=1980, end=2012, frequency=1)
sens <- sens.slope(VWHDOmin.ts)

sens
tau
p
#-------------------
# 2 trend test for Depletion Rate
VWHDOmin.lm <- lm(VWHDO[,15] ~ VWHDO[,1])
summary(VWHDOmin.lm)
plot(VWHDO[,c(1,15)], xlab="year")
abline(VWHDOmin.lm, col="red")
tau <- Kendall(VWHDO[,1], VWHDO[,15])[1]
p <- Kendall(VWHDO[,1], VWHDO[,15])[2]

VWHDOmin.ts <- ts(VWHDO[,15],start=1980, end=2012, frequency=1)
sens <- sens.slope(VWHDOmin.ts)

sens
tau
p
#-------------------
# 3 trend test for Depletion Rate Adjusted
VWHDOmin.lm <- lm(VWHDO[,17] ~ VWHDO[,1])
summary(VWHDOmin.lm)
plot(VWHDO[,c(1,17)], xlab="year")
abline(VWHDOmin.lm, col="red")
tau <- Kendall(VWHDO[,1], VWHDO[,17])[1]
p <- Kendall(VWHDO[,1], VWHDO[,17])[2]

VWHDOmin.ts <- ts(VWHDO[,17],start=1980, end=2012, frequency=1)
sens <- sens.slope(VWHDOmin.ts)

sens
tau
p







