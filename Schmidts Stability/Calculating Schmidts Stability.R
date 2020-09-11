# Here I use package named "rLakeAnalyzer" to calculate Schmidt's stability index
# Load "rLakeAnalyzer" package
library(rLakeAnalyzer)
# Set directory
#setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Data Analysis/Data/Temperature profile and Stability/csv files")
setwd("D:/Dropbox/Jiahua's MSc thesis/Data Analysis/Data/Temperature profile and Stability/csv files")

# Load temperature profiel data
wtr <- read.csv("K42.temp.csv",header=T)
head(wtr)
# Load depth data
depths <- c(1:42)
head(depths)
# Load cross sectional areas in m2 corresponding to bthD depths
# setwd("/Users/jiahuali1991/Dropbox/Jiahua's MSc thesis/Data Analysis/Data/Bathemetric data of Lake Simcoe")
setwd("D:/Dropbox/Jiahua's MSc thesis/Data Analysis/Data/Bathemetric data of Lake Simcoe")

bthA <- read.csv("LS.bthA.csv",header=T)
head(bthA)
bthA.Kem <- bthA$Kempenfelt # The required total area at depth in Kempemfelt Bay
# Load depths(m) which correspond to areal measures in bthA
bthD <- c(1:42)
head(bthD)
# use for loop to calculate 445 schmidt's stability index for Kempenfelt Bay stability during 1980-2012
schmidt <- c()
for (i in 1:445){
  schmidt[i] <- schmidt.stability(wtr[,i], depths, bthA.Kem, bthD)
  #print(schmidt)
}
head(schmidt)
# Write CSV in R
write.csv(schmidt, file = "Schmidts_stability_Kempenfelt.csv")
# See file "LS.Kempenfelt.stability.csv" for Kempenfelt Bay Schmidt's stability index using K42 temperature profile and Kempenfelt Bay bath data
