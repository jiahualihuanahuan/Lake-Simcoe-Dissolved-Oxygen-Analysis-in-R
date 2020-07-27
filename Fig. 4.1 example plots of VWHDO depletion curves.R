# give the size of the outer margins some space for  text
par(oma=c(2,2,0,0))
#divide the plot window into 36 frames, 6 per row
par(mfrow=c(3,3))
par(mar=c(0,0,0,0))
axis(side=1, )

# 1980
VWHDO.1980 <- VWHDO[which(VWHDO$year==1980),]
VWHDO.1980
plot(VWHDO.1980$day, VWHDO.1980$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.1980.de <- VWHDO.1980[-(9:11),]
VWHDO.1980.lm <- lm(VWHDO.1980.de$VWHDO ~ VWHDO.1980.de$day)
VWHDO.1980.lm
# finite length of line using segment() function
n <- length(VWHDO.1980.de$day)
segments(VWHDO.1980.de$day[1],fitted(VWHDO.1980.lm)[1],VWHDO.1980.de$day[n], fitted(VWHDO.1980.lm)[n], col="red")
summary(VWHDO.1980.lm)
axis(2,at=c(0,5,10,15),tck=0.01, cex.axis=1.5, cex.lab=1.5)
text(250, 15, labels="1980")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)

# 1984
VWHDO.1984 <- VWHDO[which(VWHDO$year==1984),]
VWHDO.1984
plot(VWHDO.1984$day, VWHDO.1984$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.1984.de <- VWHDO.1984[-(20:25),]
VWHDO.1984.de <- VWHDO.1984.de[-(1:2),]
VWHDO.1984.lm <- lm(VWHDO.1984.de$VWHDO ~ VWHDO.1984.de$day)
VWHDO.1984.lm
# finite length of line using segment() function
n <- length(VWHDO.1984.de$day)
segments(VWHDO.1984.de$day[1],fitted(VWHDO.1984.lm)[1],VWHDO.1984.de$day[n], fitted(VWHDO.1984.lm)[n], col="red")
summary(VWHDO.1984.lm)
text(250, 15, labels="1984")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)

# 1989

VWHDO.1989 <- VWHDO[which(VWHDO$year==1989),]
VWHDO.1989
plot(VWHDO.1989$day, VWHDO.1989$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.1989.de <- VWHDO.1989[-(20:21),]
VWHDO.1989.de <- VWHDO.1989.de[,]
VWHDO.1989.lm <- lm(VWHDO.1989.de$VWHDO ~ VWHDO.1989.de$day)
VWHDO.1989.lm
# finite length of line using segment() function
n <- length(VWHDO.1989.de$day)
segments(VWHDO.1989.de$day[1],fitted(VWHDO.1989.lm)[1],VWHDO.1989.de$day[n], fitted(VWHDO.1989.lm)[n], col="red")
summary(VWHDO.1989.lm)
text(250, 15, labels="1989")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)


# 1993

VWHDO.1993 <- VWHDO[which(VWHDO$year==1993),]
VWHDO.1993
plot(VWHDO.1993$day, VWHDO.1993$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.1993.de <- VWHDO.1993[-(10:11),]
VWHDO.1993.de <- VWHDO.1993.de[-(1),]
VWHDO.1993.lm <- lm(VWHDO.1993.de$VWHDO ~ VWHDO.1993.de$day)
VWHDO.1993.lm
# finite length of line using segment() function
n <- length(VWHDO.1993.de$day)
segments(VWHDO.1993.de$day[1],fitted(VWHDO.1993.lm)[1],VWHDO.1993.de$day[n], fitted(VWHDO.1993.lm)[n], col="red")
summary(VWHDO.1993.lm)
axis(2,at=c(0,5,10,15),tck=0.01, cex.axis=1.5, cex.lab=1.5)
text(250, 15, labels="1993")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)

# 1996

VWHDO.1996 <- VWHDO[which(VWHDO$year==1996),]
VWHDO.1996
plot(VWHDO.1996$day, VWHDO.1996$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.1996.de <- VWHDO.1996[-(10),]
VWHDO.1996.lm <- lm(VWHDO.1996.de$VWHDO ~ VWHDO.1996.de$day)
VWHDO.1996.lm
# finite length of line using segment() function
n <- length(VWHDO.1996.de$day)
segments(VWHDO.1996.de$day[1],fitted(VWHDO.1996.lm)[1],VWHDO.1996.de$day[n], fitted(VWHDO.1996.lm)[n], col="red")
summary(VWHDO.1996.lm)
text(250, 15, labels="1996")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)

# 2002

VWHDO.2002 <- VWHDO[which(VWHDO$year==2002),]
VWHDO.2002
plot(VWHDO.2002$day, VWHDO.2002$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.2002.de <- VWHDO.2002[-(11),]
VWHDO.2002.lm <- lm(VWHDO.2002.de$VWHDO ~ VWHDO.2002.de$day)
VWHDO.2002.lm
# finite length of line using segment() function
n <- length(VWHDO.2002.de$day)
segments(VWHDO.2002.de$day[1],fitted(VWHDO.2002.lm)[1],VWHDO.2002.de$day[n], fitted(VWHDO.2002.lm)[n], col="red")
summary(VWHDO.2002.lm)
text(250, 15, labels="2002")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)


# 2006

VWHDO.2006 <- VWHDO[which(VWHDO$year==2006),]
VWHDO.2006
plot(VWHDO.2006$day, VWHDO.2006$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.2006.de <- VWHDO.2006[-(9:10),]
VWHDO.2006.lm <- lm(VWHDO.2006.de$VWHDO ~ VWHDO.2006.de$day)
VWHDO.2006.lm
# finite length of line using segment() function
n <- length(VWHDO.2006.de$day)
segments(VWHDO.2006.de$day[1],fitted(VWHDO.2006.lm)[1],VWHDO.2006.de$day[n], fitted(VWHDO.2006.lm)[n], col="red")
summary(VWHDO.2006.lm)
text(250, 15, labels="2006")
axis(2,at=c(0,5,10,15),tck=0.01, cex.axis=1.5, cex.lab=1.5)
axis(1,at=c(100, 200, 300),tck=0.01, cex.axis=1.5, cex.lab=1.5)
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)


# 2008

VWHDO.2008 <- VWHDO[which(VWHDO$year==2008),]
VWHDO.2008
plot(VWHDO.2008$day, VWHDO.2008$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.2008.de <- VWHDO.2008[-(12:14),]
VWHDO.2008.de <- VWHDO.2008.de[-(1),]
VWHDO.2008.lm <- lm(VWHDO.2008.de$VWHDO ~ VWHDO.2008.de$day)
VWHDO.2008.lm
# finite length of line using segment() function
n <- length(VWHDO.2008.de$day)
segments(VWHDO.2008.de$day[1],fitted(VWHDO.2008.lm)[1],VWHDO.2008.de$day[n], fitted(VWHDO.2008.lm)[n], col="red")
summary(VWHDO.2008.lm)
axis(1,at=c(100, 200, 300),tck=0.01, cex.axis=1.5, cex.lab=1.5)
text(250, 15, labels="2008")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)


# 2012
VWHDO.2012 <- VWHDO[which(VWHDO$year==2012),]
VWHDO.2012
plot(VWHDO.2012$day, VWHDO.2012$VWHDO, type="p", xlab="Day of the Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.2012.de <- VWHDO.2012[-(12:14),]
VWHDO.2012.de <- VWHDO.2012.de[,]
VWHDO.2012.lm <- lm(VWHDO.2012.de$VWHDO ~ VWHDO.2012.de$day)
VWHDO.2012.lm
# finite length of line using segment() function
n <- length(VWHDO.2012.de$day)
segments(VWHDO.2012.de$day[1],fitted(VWHDO.2012.lm)[1],VWHDO.2012.de$day[n], fitted(VWHDO.2012.lm)[n], col="red")
summary(VWHDO.2012.lm)
axis(1,at=c(100, 200, 300),tck=0.01, cex.axis=1.5, cex.lab=1.5)
text(250, 15, labels="2012")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)












