# Calculate annual mean air temperature, summer air temperature (June-Sep) and individual months
setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Data Analysis/Data/Temperature profile and Stability/csv files")
# Load temperature profiel data
air <- read.csv("Shanty.Bay.air.temp.daily.csv",header=T)
colnames(air)
air$date <- as.Date (air$date, "%Y-%m-%d")
plot(air, type="l", xlab="Year", ylab="Air Temperature (˚C)")

# annual mean
air.temp <- c()
for (i in 1:33){
	air.temp[i] <- mean(air[which(as.numeric(format(air$date, "%Y"))==i+1979),2])
}
write.csv(air.temp, "my.file.csv")

# summer mean
air.summer <- c()
for (i in 1:33){
	air.summer[i] <- mean(air[which(as.numeric(format(air$date, "%Y"))==i+1979),][which(((as.numeric(format(air[which(as.numeric(format(air$date, "%Y"))==i+1979),]$date, "%m"))>=6) & (as.numeric(format(air[which(as.numeric(format(air$date, "%Y"))==i+1979),]$date, "%m"))<=9))),][,2]) # change the criteria to change the month
}
air.summer
write.csv(air.summer, "my.file.csv")


# individual months
air.month <- matrix(ncol=12, nrow=33)
for (i in 1:33){
	for (k in 1:12){
		air.month[i,k] <- mean(air[which(as.numeric(format(air$date, "%Y"))==i+1979 & as.numeric(format(air$date, "%m"))==k),2]) # change the criteria to change the month
		}
}
air.month
write.csv(air.month, "my.file.csv")

#----------------------------------------
# 1. VWHDO calculation based on thermocline depth
# set directory
setwd("/Users/jiahuali1991/Dropbox/R/data/Lake Simcoe data")
# load bathemetric data
bthA <- read.csv("LS.bthA.csv",header=T)

# load thermocline data
thermo.depth <-read.csv("LS.K42.thermo.depth.csv",header=T)
# load oxygen profile data
oxy <- read.csv("LS.K42.oxy.profile.csv",header=T)

VWHDO <- c()
for (i in 1:439) {
	 t.d <- round(thermo.depth$thermocline[i], digits=0)
	 VWHDO[i] <- sum(bthA[(t.d:42),13]*oxy[(t.d:42),i])/sum(bthA[(t.d:42),13])
}
# Write CSV in R
write.csv(VWHDO, file = "my.file.csv")
#-------------------------------------
# 1
# set directory
setwd("/Users/jiahuali1991/Dropbox/R/data/Lake Simcoe data")
# load bathemetric data
bthA <- read.csv("LS.bthA.csv",header=T)

# load thermocline data
thermo.depth <-read.csv("LS.K42.thermo.depth.csv",header=T)
# load oxygen profile data
oxy <- read.csv("LS.K42.oxy.profile.csv",header=T)

VWHDO <- c()
for (i in 1:439) {
	 VWHDO[i] <- sum(bthA[(18:42),13]*oxy[(18:42),i])/sum(bthA[(18:42),13])
}
# Write CSV in R
write.csv(VWHDO, file = "my.file.csv")

#-------------------------------------
# 3. VWEDO calculation based on 18m depth (epilimnion)
# set directory
setwd("/Users/jiahuali1991/Dropbox/R/data/Lake Simcoe data")
# load bathemetric data
bthA <- read.csv("LS.bthA.csv",header=T)

# load thermocline data
thermo.depth <-read.csv("LS.K42.thermo.depth.csv",header=T)
# load oxygen profile data
oxy <- read.csv("LS.K42.oxy.profile.csv",header=T)

VWHDO <- c()
for (i in 1:439) {
	 	 VWHDO[i] <- sum(bthA[(1:17),13]*oxy[(1:17),i])/sum(bthA[(1:17),13])
}
# Write CSV in R
write.csv(VWHDO, file = "my.file.csv")

#-------------------------------------
# 4. VWET calculation based on 18m depth (epilimnion)
# set directory
setwd("/Users/jiahuali1991/Dropbox/R/data/Lake Simcoe data")
# load bathemetric data
bthA <- read.csv("LS.bthA.csv",header=T)

# load thermocline data
thermo.depth <-read.csv("LS.K42.thermo.depth.csv",header=T)
# load oxygen profile data
temp <- read.csv("LS.K42.temp.profile.csv",header=T)

VWHDO <- c()
for (i in 1:439) {
	 	 VWHDO[i] <- sum(bthA[(1:17),13]* temp[(1:17),i])/sum(bthA[(1:17),13])
}
# Write CSV in R
write.csv(VWHDO, file = "my.file.csv")
#-------------------------------------
# 5. VWHT calculation based on 18m depth (hypolimnion)
# set directory
setwd("/Users/jiahuali1991/Dropbox/R/data/Lake Simcoe data")
# load bathemetric data
bthA <- read.csv("LS.bthA.csv",header=T)

# load thermocline data
thermo.depth <-read.csv("LS.K42.thermo.depth.csv",header=T)
# load oxygen profile data
temp <- read.csv("LS.K42.temp.profile.csv",header=T)

VWHDO <- c()
for (i in 1:439) {
	 	 VWHDO[i] <- sum(bthA[(18:42),13]* temp[(18:42),i])/sum(bthA[(18:42),13])
}
# Write CSV in R
write.csv(VWHDO, file = "my.file.csv")

#-------------------------------------
# Calculate max VWHT and when it occurs
VWHT <- read.csv("LS.Kempenfelt.VWHT.csv",header=T)
head(VWHT)
VWHT <- VWHT[,c(2,3,4)]

VWHT.i.max <- c()
for (i in 1:33){
	VWHT.i <- VWHT[which(VWHT$year==i+1979),]
	VWHT.i.max[i] <- max(VWHT.i$VWHT)
	}
write.csv(VWHT.i.max, "my.file.csv")

VWHT.i.max <- c()
day <- c()
for (i in 1:33){
	VWHT.i <- VWHT[which(VWHT$year==i+1979),]
	VWHT.i.sort <- VWHT.i[order(-VWHT.i$VWHT),]
	VWHT.i.max <- VWHT.i.sort[1,2]
	day[i] <- VWHT.i.max
	}
write.csv(day, "my.file.csv")

#-------------------------------------
# Calculate mean VWHT during July 1 - Sep 30 (day 182-day 273)
# set directory
setwd("/Users/jiahuali1991/Dropbox/R/data/Lake Simcoe data")
# load bathemetric data
VWHT <- read.csv("LS.Kempenfelt.VWHT.csv",header=T)
head(VWHT)

temp.182 <- c()
temp.273 <- c()
for (i in 1:33) {
	VWHT.i <- VWHT[which(VWHT$year==i+1979),]
	n <- length(VWHT.i$year)
	for (k in 1:(n-1)){
		if((182 %in% round(VWHT.i$day[k]):round(VWHT.i$day[k+1]))==TRUE){
			VWHT.lm <- lm(VWHT.i[c(k,k+1),4] ~ VWHT.i[c(k,k+1),3])
			temp.182[i] <- 182* VWHT.lm$coefficients[2] + VWHT.lm$coefficients[1]
		} else {}
		if ((273 %in% round(VWHT.i$day[k]):round(VWHT.i$day[k+1]))==TRUE){
			VWHT.lm <- lm(VWHT.i[c(k,k+1),4] ~ VWHT.i[c(k,k+1),3])
			temp.273[i] <- 273*VWHT.lm$coefficients[2] + VWHT.lm$coefficients[1]
		}else {}
		
		
	}
	
}

write.csv(temp.182, "182.csv")
write.csv(temp.273, "273.csv")

# only want the 182 to 273 day

temp.182.273 <- matrix(nrow=25, ncol=33)
for (i in 1:33) {
	VWHT.i <- VWHT[which(VWHT$year==i+1979),]
	n <- length(VWHT.i$year)
	for (k in 1:(n-1)){
		if(VWHT.i$day[k]<273 & VWHT.i$day[k]>182){
			temp.182.273[k,i] <- VWHT.i[k,3]			
		} else {}
				
		
	}
	
}
write.csv(temp.182.273, "182.273.day.csv")


temp.182.273 <- matrix(nrow=25, ncol=33)
for (i in 1:33) {
	VWHT.i <- VWHT[which(VWHT$year==i+1979),]
	n <- length(VWHT.i$year)
	for (k in 1:(n-1)){
		if(VWHT.i$day[k]<273 & VWHT.i$day[k]>182){
			temp.182.273[k,i] <- VWHT.i[k,4]			
		} else {}
				
		
	}
	
}
write.csv(temp.182.273, "182.273.temp.csv")

# Calculate mean VWHT between July 1 and Sep 30
VWHT <- read.csv("VWHT.182.273.csv", header=T)
head(temp)

temp.r <- matrix(nrow=25, ncol=33)
day.r <- matrix(nrow=25, ncol=33)
interval <- c()
area <- matrix(nrow=25, ncol=33)
mean.VWHT.192.273 <- c()

for (i in 1:33){
	VWHT.i <- VWHT[which(VWHT$year==i+1979),]
	n <- length(VWHT.i$year)
	for (k in 1:(n-1)){
		interval[k] <- VWHT.i[(k+1),2]-VWHT.i[k,2]
		area[k,i] <- ((VWHT.i[k,3]+ VWHT.i[(k+1),3])/2)*interval[k]
	}
	mean.VWHT.192.273[i] <- sum(area[,i],na.rm=T)/(VWHT.i[n,2]-VWHT.i[1,2])
}
plot((1980:2012), mean.VWHT.192.273)
write.csv(mean.VWHT.192.273, "mean.182.273.temp.csv")



#---------------------------------------------------
# Calculate mean VWET during summer (JULY to SEP) 

# set directory
setwd("/Users/jiahuali1991/Dropbox/R/data/Lake Simcoe data")
# load bathemetric data
VWHT <- read.csv("LS.K42.VWHDO.VWHT.thermocline.csv",header=T)
head(VWHT)

temp.182 <- c()
temp.273 <- c()
for (i in 1:33) {
	VWHT.i <- VWHT[which(VWHT$year==i+1979),]
	n <- length(VWHT.i$year)
	for (k in 1:(n-1)){
		if((182 %in% round(VWHT.i$day[k]):round(VWHT.i$day[k+1]))==TRUE){
			VWHT.lm <- lm(VWHT.i[c(k,k+1),8] ~ VWHT.i[c(k,k+1),3])
			temp.182[i] <- 182* VWHT.lm$coefficients[2] + VWHT.lm$coefficients[1]
		} else {}
		if ((273 %in% round(VWHT.i$day[k]):round(VWHT.i$day[k+1]))==TRUE){
			VWHT.lm <- lm(VWHT.i[c(k,k+1),8] ~ VWHT.i[c(k,k+1),3])
			temp.273[i] <- 273*VWHT.lm$coefficients[2] + VWHT.lm$coefficients[1]
		}else {}
		
		
	}
	
}

write.csv(temp.182, "182.csv")
write.csv(temp.273, "273.csv")

# only want the 182 to 273 day

temp.182.273 <- matrix(nrow=25, ncol=33)
for (i in 1:33) {
	VWHT.i <- VWHT[which(VWHT$year==i+1979),]
	n <- length(VWHT.i$year)
	for (k in 1:(n-1)){
		if(VWHT.i$day[k]<273 & VWHT.i$day[k]>182){
			temp.182.273[k,i] <- VWHT.i[k,3]			
		} else {}
				
		
	}
	
}
write.csv(temp.182.273, "182.273.day.csv")


temp.182.273 <- matrix(nrow=25, ncol=33)
for (i in 1:33) {
	VWHT.i <- VWHT[which(VWHT$year==i+1979),]
	n <- length(VWHT.i$year)
	for (k in 1:(n-1)){
		if(VWHT.i$day[k]<273 & VWHT.i$day[k]>182){
			temp.182.273[k,i] <- VWHT.i[k,8]			
		} else {}
				
		
	}
	
}
write.csv(temp.182.273, "182.273.temp.csv")

# Calculate mean VWHT between July 1 and Sep 30
VWHT <- read.csv("182.273.temp.csv", header=T)
day <- read.csv("182.273.day.csv", header=T)

VWHT.r <- matrix(nrow=25, ncol=33)
day.r <- matrix(nrow=25, ncol=33)
interval <- c()
area <- matrix(nrow=25, ncol=33)
mean.VWHT.192.273 <- c()

for (i in 1:33){
	VWHT.i <- VWHT[,i]
	VWHT.r <- VWHT.i[complete.cases(VWHT.i)]
	day.i. <- day[,i]
	day.r <- day.r[complete.cases(day.i)]
	n <- length(VWHT.r)
	for (k in 1:(n-1)){
		interval[k] <- VWHT.r[(k+1)]-VWHT.r[k]
		area[k,i] <- ((VWHT.r[k]+ VWHT.r[(k+1)])/2)*interval[k]
	}
	mean.VWHT.192.273[i] <- sum(area[,i],na.rm=T)/(VWHT.r[n]-VWHT.r[1])
}
plot((1980:2012), mean.VWHT.192.273)
a <- lm((mean.VWHT.192.273) ~ (1980:2012))
abline(a)
write.csv(mean.VWHT.192.273, "mean.182.273.temp.csv")

#------------------------------------------------------
# Plot VWET, boxplot
temp <- read.csv("temp.csv", header=T)
head(temp)
temp1 <- temp$VWET_July_Sep_mean[1:16]
temp2 <- temp$VWET_July_Sep_mean[17:33]
boxplot(temp1,temp2, xlab="1980-1996                                       1997-2012", ylab="temperature (˚C)", main="VWET during summer (July 1st - Sep 30th)")

#------------------------------------------------------------
# # Calculate VWT/heat content of whole lake in K42
setwd("/Users/jiahuali1991/Dropbox/R/data/Lake Simcoe data")
bthA <- read.csv("LS.bthA.csv", header=T)
temp <- read.csv("LS.K42.temp.profile.csv", header=T)
head(bthA)
head(temp)
depth <-1:42
VWT.k <- matrix(nrow=42, ncol=445)
VWT <- c()
for (i in 1:445){
	for (k in 1:42){
		VWT.k[k,i] <- temp[k,i]*bthA[k,13]
	}
	VWT[i] <- sum(VWT.k[,i])/sum(bthA[,13])
	
}
write.csv(VWT, file="VWT.csv")
#------------------------------------------------------------
# # Calculate VWT/heat content of whole lake in K42 during summer (july sep) 

# set directory
setwd("/Users/jiahuali1991/Dropbox/R/data/Lake Simcoe data")
# load bathemetric data
VWHT <- read.csv("VWT.csv",header=T)
head(VWHT)

temp.182 <- c()
temp.273 <- c()
for (i in 1:33) {
	VWHT.i <- VWHT[which(VWHT$year==i+1979),]
	n <- length(VWHT.i$year)
	for (k in 1:(n-1)){
		if((182 %in% round(VWHT.i$day[k]):round(VWHT.i$day[k+1]))==TRUE){
			VWHT.lm <- lm(VWHT.i[c(k,k+1),1] ~ VWHT.i[c(k,k+1),4])
			temp.182[i] <- 182* VWHT.lm$coefficients[2] + VWHT.lm$coefficients[1]
		} else {}
		if ((273 %in% round(VWHT.i$day[k]):round(VWHT.i$day[k+1]))==TRUE){
			VWHT.lm <- lm(VWHT.i[c(k,k+1),1] ~ VWHT.i[c(k,k+1),4])
			temp.273[i] <- 273*VWHT.lm$coefficients[2] + VWHT.lm$coefficients[1]
		}else {}
		
		
	}
	
}

write.csv(temp.182, "182.csv")
write.csv(temp.273, "273.csv")

# only want the 182 to 273 day

temp.182.273 <- matrix(nrow=25, ncol=33)
for (i in 1:33) {
	VWHT.i <- VWHT[which(VWHT$year==i+1979),]
	n <- length(VWHT.i$year)
	for (k in 1:(n-1)){
		if(VWHT.i$day[k]<273 & VWHT.i$day[k]>182){
			temp.182.273[k,i] <- VWHT.i[k,4]			
		} else {}
				
		
	}
	
}
write.csv(temp.182.273, "182.273.day.csv")


temp.182.273 <- matrix(nrow=25, ncol=33)
for (i in 1:33) {
	VWHT.i <- VWHT[which(VWHT$year==i+1979),]
	n <- length(VWHT.i$year)
	for (k in 1:(n-1)){
		if(VWHT.i$day[k]<273 & VWHT.i$day[k]>182){
			temp.182.273[k,i] <- VWHT.i[k,1]			
		} else {}
				
		
	}
	
}
write.csv(temp.182.273, "182.273.temp.csv")

# Calculate mean VWHT between July 1 and Sep 30
VWHT <- read.csv("182.273.temp.csv", header=T)
day <- read.csv("182.273.day.csv", header=T)

VWHT.r <- matrix(nrow=25, ncol=33)
day.r <- matrix(nrow=25, ncol=33)
interval <- c()
area <- matrix(nrow=25, ncol=33)
mean.VWHT.192.273 <- c()

for (i in 1:33){
	VWHT.i <- VWHT[,i]
	VWHT.r <- VWHT.i[complete.cases(VWHT.i)]
	day.i <- day[,i]
	day.r <- day.r[complete.cases(day.i)]
	n <- length(VWHT.r)
	for (k in 1:(n-1)){
		interval[k] <- VWHT.r[(k+1)]-VWHT.r[k]
		area[k,i] <- ((VWHT.r[k]+ VWHT.r[(k+1)])/2)*interval[k]
	}
	mean.VWHT.192.273[i] <- sum(area[,i],na.rm=T)/(VWHT.r[n]-VWHT.r[1])
}
plot((1980:2012), mean.VWHT.192.273)
a <- lm((mean.VWHT.192.273) ~ (1980:2012))
abline(a)
write.csv(mean.VWHT.192.273, "mean.182.273.temp.csv")




