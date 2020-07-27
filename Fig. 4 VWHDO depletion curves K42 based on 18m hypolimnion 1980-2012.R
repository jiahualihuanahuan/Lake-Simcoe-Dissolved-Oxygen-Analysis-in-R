# VWHDO data analysis in plots
# Check the VWHDO data
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Data Analysis/Data/VWHDO/csv files")
VWHDO <- read.csv("LS.K42.VWHDO.data.csv",header=T)
head(VWHDO)
colnames(VWHDO)
summary(VWHDO)
# Check the highest VWHDO and the distribution of VWHDO
hist(VWHDO$VWHDO, xlab="VWHDO mg/L", ylab="Frequency", main="Histogram of VWHDO")
# Plot all VWHDO vs Julian day to see the general pattern of the VWHDO depletion
plot(VWHDO$day, VWHDO$VWHDO, xlab="Day of the year", ylab="VWHDO (mg/L)", main="general pattern of VWHDO vs. Julian day")

# give the size of the outer margins some space for  text
par(oma=c(2,2,0,0))
#divide the plot window into 36 frames, 6 per row
par(mfrow=c(5,7))
par(mfrow=c(3,3))
par(mar=c(0,0,0,0))
axis(side=1, )

# VWHDO analysis K42 1980-2012

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
axis(2,at=c(0,5,10,15),tck=0.01)
text(250, 15, labels="1980")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)

# 1981
VWHDO.1981 <- VWHDO[which(VWHDO$year==1981),]
VWHDO.1981
plot(VWHDO.1981$day, VWHDO.1981$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)",xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.1981.de <- VWHDO.1981[-c(8:10),]
VWHDO.1981.lm <- lm(VWHDO.1981.de$VWHDO ~ VWHDO.1981.de$day)
VWHDO.1981.lm
# finite length of line using segment() function
n <- length(VWHDO.1981.de$day)
segments(VWHDO.1981.de$day[1],fitted(VWHDO.1981.lm)[1],VWHDO.1981.de$day[n], fitted(VWHDO.1981.lm)[n], col="red")
summary(VWHDO.1981.lm)
text(250, 15, labels="1981")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)

# 1982
VWHDO.1982 <- VWHDO[which(VWHDO$year==1982),]
VWHDO.1982
plot(VWHDO.1982$day, VWHDO.1982$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.1982.de <- VWHDO.1982[-(11:13),]
VWHDO.1982.de <- VWHDO.1982.de[,]
VWHDO.1982.lm <- lm(VWHDO.1982.de$VWHDO ~ VWHDO.1982.de$day)
VWHDO.1982.lm
# finite length of line using segment() function
n <- length(VWHDO.1982.de$day)
segments(VWHDO.1982.de$day[1],fitted(VWHDO.1982.lm)[1],VWHDO.1982.de$day[n], fitted(VWHDO.1982.lm)[n], col="red")
summary(VWHDO.1982.lm)
text(250, 15, labels="1982")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)

# 1983
VWHDO.1983 <- VWHDO[which(VWHDO$year==1983),]
VWHDO.1983
plot(VWHDO.1983$day, VWHDO.1983$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.1983.de <- VWHDO.1983[-(9:11),]
VWHDO.1983.de <- VWHDO.1983.de[-1,]
VWHDO.1983.lm <- lm(VWHDO.1983.de$VWHDO ~ VWHDO.1983.de$day)
VWHDO.1983.lm
# finite length of line using segment() function
n <- length(VWHDO.1983.de$day)
segments(VWHDO.1983.de$day[1],fitted(VWHDO.1983.lm)[1],VWHDO.1983.de$day[n], fitted(VWHDO.1983.lm)[n], col="red")
summary(VWHDO.1983.lm)
text(250, 15, labels="1983")
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

# 1985

VWHDO.1985 <- VWHDO[which(VWHDO$year==1985),]
VWHDO.1985
plot(VWHDO.1985$day, VWHDO.1985$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.1985.de <- VWHDO.1985[-(15:19),]
VWHDO.1985.de <- VWHDO.1985.de[-(1:2),]
VWHDO.1985.lm <- lm(VWHDO.1985.de$VWHDO ~ VWHDO.1985.de$day)
VWHDO.1985.lm
# finite length of line using segment() function
n <- length(VWHDO.1985.de$day)
segments(VWHDO.1985.de$day[1],fitted(VWHDO.1985.lm)[1],VWHDO.1985.de$day[n], fitted(VWHDO.1985.lm)[n], col="red")
summary(VWHDO.1985.lm)

text(250, 15, labels="1985")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)

# 1986

VWHDO.1986 <- VWHDO[which(VWHDO$year==1986),]
VWHDO.1986
plot(VWHDO.1986$day, VWHDO.1986$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.1986.de <- VWHDO.1986[-c(16),]
VWHDO.1986.de <- VWHDO.1986.de[-c(1,2),]
VWHDO.1986.lm <- lm(VWHDO.1986.de$VWHDO ~ VWHDO.1986.de$day)
VWHDO.1986.lm
# finite length of line using segment() function
n <- length(VWHDO.1986.de$day)
segments(VWHDO.1986.de$day[1],fitted(VWHDO.1986.lm)[1],VWHDO.1986.de$day[n], fitted(VWHDO.1986.lm)[n], col="red")
summary(VWHDO.1986.lm)
text(250, 15, labels="1986")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)

# 1987

VWHDO.1987 <- VWHDO[which(VWHDO$year==1987),]
VWHDO.1987
plot(VWHDO.1987$day, VWHDO.1987$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.1987.de <- VWHDO.1987[-c(17:19),]
VWHDO.1987.de <- VWHDO.1987.de[-(1:3),]
VWHDO.1987.lm <- lm(VWHDO.1987.de$VWHDO ~ VWHDO.1987.de$day)
VWHDO.1987.lm
# finite length of line using segment() function
n <- length(VWHDO.1987.de$day)
segments(VWHDO.1987.de$day[1],fitted(VWHDO.1987.lm)[1],VWHDO.1987.de$day[n], fitted(VWHDO.1987.lm)[n], col="red")
summary(VWHDO.1987.lm)
text(250, 15, labels="1987")
axis(2,at=c(0,5,10,15),tck=0.01)
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)

# 1988

VWHDO.1988 <- VWHDO[which(VWHDO$year==1988),]
VWHDO.1988
plot(VWHDO.1988$day, VWHDO.1988$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.1988.de <- VWHDO.1988[-c(2,17),]
VWHDO.1988.lm <- lm(VWHDO.1988.de$VWHDO ~ VWHDO.1988.de$day)
VWHDO.1988.lm
# finite length of line using segment() function
n <- length(VWHDO.1988.de$day)
segments(VWHDO.1988.de$day[1],fitted(VWHDO.1988.lm)[1],VWHDO.1988.de$day[n], fitted(VWHDO.1988.lm)[n], col="red")
summary(VWHDO.1988.lm)
text(250, 15, labels="1988")
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


# 1990

VWHDO.1990 <- VWHDO[which(VWHDO$year==1990),]
VWHDO.1990
plot(VWHDO.1990$day, VWHDO.1990$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.1990.de <- VWHDO.1990[-(17:20),]
VWHDO.1990.lm <- lm(VWHDO.1990.de$VWHDO ~ VWHDO.1990.de$day)
VWHDO.1990.lm
# finite length of line using segment() function
n <- length(VWHDO.1990.de$day)
segments(VWHDO.1990.de$day[1],fitted(VWHDO.1990.lm)[1],VWHDO.1990.de$day[n], fitted(VWHDO.1990.lm)[n], col="red")
summary(VWHDO.1990.lm)
text(250, 15, labels="1990")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)

# 1991

VWHDO.1991 <- VWHDO[which(VWHDO$year==1991),]
VWHDO.1991
plot(VWHDO.1991$day, VWHDO.1991$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.1991.de <- VWHDO.1991[-c(13:14),]
VWHDO.1991.de <- VWHDO.1991.de[-(1:3),]
VWHDO.1991.lm <- lm(VWHDO.1991.de$VWHDO ~ VWHDO.1991.de$day)
VWHDO.1991.lm
# finite length of line using segment() function
n <- length(VWHDO.1991.de$day)
segments(VWHDO.1991.de$day[1],fitted(VWHDO.1991.lm)[1],VWHDO.1991.de$day[n], fitted(VWHDO.1991.lm)[n], col="red")
summary(VWHDO.1991.lm)
text(250, 15, labels="1991")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)

# 1992

VWHDO.1992 <- VWHDO[which(VWHDO$year==1992),]
VWHDO.1992
plot(VWHDO.1992$day, VWHDO.1992$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.1992.de <- VWHDO.1992[-(10:11),]
VWHDO.1992.de <- VWHDO.1992.de[-(1),]
VWHDO.1992.lm <- lm(VWHDO.1992.de$VWHDO ~ VWHDO.1992.de$day)
VWHDO.1992.lm
# finite length of line using segment() function
n <- length(VWHDO.1992.de$day)
segments(VWHDO.1992.de$day[1],fitted(VWHDO.1992.lm)[1],VWHDO.1992.de$day[n], fitted(VWHDO.1992.lm)[n], col="red")
summary(VWHDO.1992.lm)
text(250, 15, labels="1992")
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
text(250, 15, labels="1993")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)


# 1994

VWHDO.1994 <- VWHDO[which(VWHDO$year==1994),]
VWHDO.1994
plot(VWHDO.1994$day, VWHDO.1994$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.1994.de <- VWHDO.1994[-(12:13),]
VWHDO.1994.de <- VWHDO.1994.de[-c(3),]
VWHDO.1994.lm <- lm(VWHDO.1994.de$VWHDO ~ VWHDO.1994.de$day)
VWHDO.1994.lm
# finite length of line using segment() function
n <- length(VWHDO.1994.de$day)
segments(VWHDO.1994.de$day[1],fitted(VWHDO.1994.lm)[1],VWHDO.1994.de$day[n], fitted(VWHDO.1994.lm)[n], col="red")
summary(VWHDO.1994.lm)
text(250, 15, labels="1994")
axis(2,at=c(0,5,10,15),tck=0.01)
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)

# 1995

VWHDO.1995 <- VWHDO[which(VWHDO$year==1995),]
VWHDO.1995
plot(VWHDO.1995$day, VWHDO.1995$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.1995.de <- VWHDO.1995[-(9:10),]
VWHDO.1995.lm <- lm(VWHDO.1995.de$VWHDO ~ VWHDO.1995.de$day)
VWHDO.1995.lm
# finite length of line using segment() function
n <- length(VWHDO.1995.de$day)
segments(VWHDO.1995.de$day[1],fitted(VWHDO.1995.lm)[1],VWHDO.1995.de$day[n], fitted(VWHDO.1995.lm)[n], col="red")
summary(VWHDO.1995.lm)
text(250, 15, labels="1995")
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

# 1997

VWHDO.1997 <- VWHDO[which(VWHDO$year==1997),]
VWHDO.1997
plot(VWHDO.1997$day, VWHDO.1997$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.1997.de <- VWHDO.1997[-(10:11),]
VWHDO.1997.de <- VWHDO.1997.de[-(1),]
VWHDO.1997.lm <- lm(VWHDO.1997.de$VWHDO ~ VWHDO.1997.de$day)
VWHDO.1997.lm
# finite length of line using segment() function
n <- length(VWHDO.1997.de$day)
segments(VWHDO.1997.de$day[1],fitted(VWHDO.1997.lm)[1],VWHDO.1997.de$day[n], fitted(VWHDO.1997.lm)[n], col="red")
summary(VWHDO.1997.lm)
text(250, 15, labels="1997")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)

# 1998

VWHDO.1998 <- VWHDO[which(VWHDO$year==1998),]
VWHDO.1998
plot(VWHDO.1998$day, VWHDO.1998$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')

VWHDO.1998.de <- VWHDO.1998[-c(9:10),]
VWHDO.1998.lm <- lm(VWHDO.1998.de$VWHDO ~ VWHDO.1998.de$day)
VWHDO.1998.lm
# finite length of line using segment() function
n <- length(VWHDO.1998.de$day)
segments(VWHDO.1998.de$day[1],fitted(VWHDO.1998.lm)[1],VWHDO.1998.de$day[n], fitted(VWHDO.1998.lm)[n], col="red")
summary(VWHDO.1998.lm)
text(250, 15, labels="1998")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)

# 1999

VWHDO.1999 <- VWHDO[which(VWHDO$year==1999),]
VWHDO.1999
plot(VWHDO.1999$day, VWHDO.1999$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.1999.de <- VWHDO.1999[-(11),]
VWHDO.1999.de <- VWHDO.1999.de[-(1:2),]
VWHDO.1999.lm <- lm(VWHDO.1999.de$VWHDO ~ VWHDO.1999.de$day)
VWHDO.1999.lm
# finite length of line using segment() function
n <- length(VWHDO.1999.de$day)
segments(VWHDO.1999.de$day[1],fitted(VWHDO.1999.lm)[1],VWHDO.1999.de$day[n], fitted(VWHDO.1999.lm)[n], col="red")
summary(VWHDO.1999.lm)
text(250, 15, labels="1999")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)


# 2000

VWHDO.2000 <- VWHDO[which(VWHDO$year==2000),]
VWHDO.2000
plot(VWHDO.2000$day, VWHDO.2000$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.2000.de <- VWHDO.2000[-(9:11),]
VWHDO.2000.lm <- lm(VWHDO.2000.de$VWHDO ~ VWHDO.2000.de$day)
VWHDO.2000.lm
# finite length of line using segment() function
n <- length(VWHDO.2000.de$day)
segments(VWHDO.2000.de$day[1],fitted(VWHDO.2000.lm)[1],VWHDO.2000.de$day[n], fitted(VWHDO.2000.lm)[n], col="red")
summary(VWHDO.2000.lm)
text(250, 15, labels="2000")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)

# 2001

VWHDO.2001 <- VWHDO[which(VWHDO$year==2001),]
VWHDO.2001
plot(VWHDO.2001$day, VWHDO.2001$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.2001.de <- VWHDO.2001[-(9:11),]
VWHDO.2001.lm <- lm(VWHDO.2001.de$VWHDO ~ VWHDO.2001.de$day)
VWHDO.2001.lm
# finite length of line using segment() function
n <- length(VWHDO.2001.de$day)
segments(VWHDO.2001.de$day[1],fitted(VWHDO.2001.lm)[1],VWHDO.2001.de$day[n], fitted(VWHDO.2001.lm)[n], col="red")
summary(VWHDO.2001.lm)
text(250, 15, labels="2001")
axis(2,at=c(0,5,10,15),tck=0.01)
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



# 2003

VWHDO.2003 <- VWHDO[which(VWHDO$year==2003),]
VWHDO.2003
plot(VWHDO.2003$day, VWHDO.2003$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.2003.de <- VWHDO.2003[-(9:10),]
VWHDO.2003.de <- VWHDO.2003.de[-(4:5),]
VWHDO.2003.lm <- lm(VWHDO.2003.de$VWHDO ~ VWHDO.2003.de$day)
VWHDO.2003.lm
# finite length of line using segment() function
n <- length(VWHDO.2003.de$day)
segments(VWHDO.2003.de$day[1],fitted(VWHDO.2003.lm)[1],VWHDO.2003.de$day[n], fitted(VWHDO.2003.lm)[n], col="red")
summary(VWHDO.2003.lm)
text(250, 15, labels="2003")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)

# 2004

VWHDO.2004 <- VWHDO[which(VWHDO$year==2004),]
VWHDO.2004
plot(VWHDO.2004$day, VWHDO.2004$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.2004.de <- VWHDO.2004[-(11:12),]
VWHDO.2004.de <- VWHDO.2004.de[-(1:2),]
VWHDO.2004.lm <- lm(VWHDO.2004.de$VWHDO ~ VWHDO.2004.de$day)
VWHDO.2004.lm
# finite length of line using segment() function
n <- length(VWHDO.2004.de$day)
segments(VWHDO.2004.de$day[1],fitted(VWHDO.2004.lm)[1],VWHDO.2004.de$day[n], fitted(VWHDO.2004.lm)[n], col="red")
summary(VWHDO.2004.lm)
text(250, 15, labels="2004")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)

# 2005

VWHDO.2005 <- VWHDO[which(VWHDO$year==2005),]
VWHDO.2005
plot(VWHDO.2005$day, VWHDO.2005$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.2005.de <- VWHDO.2005[-(11),]
VWHDO.2005.de <- VWHDO.2005.de[-(1:2),]
VWHDO.2005.lm <- lm(VWHDO.2005.de$VWHDO ~ VWHDO.2005.de$day)
VWHDO.2005.lm
# finite length of line using segment() function
n <- length(VWHDO.2005.de$day)
segments(VWHDO.2005.de$day[1],fitted(VWHDO.2005.lm)[1],VWHDO.2005.de$day[n], fitted(VWHDO.2005.lm)[n], col="red")
summary(VWHDO.2005.lm)
text(250, 15, labels="2005")
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
axis(1,at=c(100, 200, 300),tck=0.01)
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)


# 2007

VWHDO.2007 <- VWHDO[which(VWHDO$year==2007),]
VWHDO.2007
plot(VWHDO.2007$day, VWHDO.2007$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.2007.de <- VWHDO.2007[-(10),]
VWHDO.2007.lm <- lm(VWHDO.2007.de$VWHDO ~ VWHDO.2007.de$day)
VWHDO.2007.lm
# finite length of line using segment() function
n <- length(VWHDO.2007.de$day)
segments(VWHDO.2007.de$day[1],fitted(VWHDO.2007.lm)[1],VWHDO.2007.de$day[n], fitted(VWHDO.2007.lm)[n], col="red")
summary(VWHDO.2007.lm)
axis(1,at=c(100, 200, 300),tck=0.01)
text(250, 15, labels="2007")
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
axis(1,at=c(100, 200, 300),tck=0.01)
text(250, 15, labels="2008")
axis(2,at=c(0,5,10,15),tck=0.01)
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)


# 2009
VWHDO.2009 <- VWHDO[which(VWHDO$year==2009),]
VWHDO.2009
plot(VWHDO.2009$day, VWHDO.2009$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.2009.de <- VWHDO.2009[-(12:15),]
VWHDO.2009.de <- VWHDO.2009.de[,]
VWHDO.2009.lm <- lm(VWHDO.2009.de$VWHDO ~ VWHDO.2009.de$day)
VWHDO.2009.lm
# finite length of line using segment() function
n <- length(VWHDO.2009.de$day)
segments(VWHDO.2009.de$day[1],fitted(VWHDO.2009.lm)[1],VWHDO.2009.de$day[n], fitted(VWHDO.2009.lm)[n], col="red")
summary(VWHDO.2009.lm)
axis(1,at=c(100, 200, 300),tck=0.01)
text(250, 15, labels="2009")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)


# 2010

VWHDO.2010 <- VWHDO[which(VWHDO$year==2010),]
VWHDO.2010
plot(VWHDO.2010$day, VWHDO.2010$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.2010.de <- VWHDO.2010[-(13:15),]
VWHDO.2010.de <- VWHDO.2010.de[-(1:2),]
VWHDO.2010.lm <- lm(VWHDO.2010.de$VWHDO ~ VWHDO.2010.de$day)
VWHDO.2010.lm
# finite length of line using segment() function
n <- length(VWHDO.2010.de$day)
segments(VWHDO.2010.de$day[1],fitted(VWHDO.2010.lm)[1],VWHDO.2010.de$day[n], fitted(VWHDO.2010.lm)[n], col="red")
summary(VWHDO.2010.lm)
axis(1,at=c(100, 200, 300),tck=0.01)
text(250, 15, labels="2010")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)


# 2011

VWHDO.2011 <- VWHDO[which(VWHDO$year==2011),]
VWHDO.2011
plot(VWHDO.2011$day, VWHDO.2011$VWHDO, type="p", xlab="Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), xaxt='n', yaxt='n')
VWHDO.2011.de <- VWHDO.2011[-(13:15),]
VWHDO.2011.de <- VWHDO.2011.de[,]
VWHDO.2011.lm <- lm(VWHDO.2011.de$VWHDO ~ VWHDO.2011.de$day)
VWHDO.2011.lm
# finite length of line using segment() function
n <- length(VWHDO.2011.de$day)
segments(VWHDO.2011.de$day[1],fitted(VWHDO.2011.lm)[1],VWHDO.2011.de$day[n], fitted(VWHDO.2011.lm)[n], col="red")
summary(VWHDO.2011.lm)
axis(1,at=c(100, 200, 300),tck=0.01)
text(250, 15, labels="2011")
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
axis(1,at=c(100, 200, 300),tck=0.01)
text(250, 15, labels="2012")
grid(4, 4, col="lightgray", lty= "dotted", lwd=par("lwd"), equilogs=T)


# 2012 example

VWHDO.2012 <- VWHDO[which(VWHDO$year==2012),]
VWHDO.2012
plot(VWHDO.2012$day, VWHDO.2012$VWHDO, type="p", xlab="Day of the Year", ylab="VWHDO (mg/L)", xlim=c(50,350), ylim=c(0,16), cex.lab=1.5, cex=1.5)
VWHDO.2012.de <- VWHDO.2012[-(13:14),]
VWHDO.2012.de <- VWHDO.2012.de[,]
VWHDO.2012.lm <- lm(VWHDO.2012.de$VWHDO ~ VWHDO.2012.de$day)
VWHDO.2012.lm

