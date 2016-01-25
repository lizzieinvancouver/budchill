# Calculating chill units

setwd("~/Documents/git/budchill")

# Dynamic Model

library(chillR)
# Luedeling recommends Dynamic Modle (chill portions)

# HF chilling fall 2015
htemp <- read.csv("input/hf001-10-15min-m.csv")

htemp$datetime <- as.POSIXlt(htemp$datetime, format = "%Y-%m-%dT%H:%M")

# select years. There are 44 na's

htemp1 <- htemp[htemp$datetime > "2015-09-30" & htemp$datetime < "2015-12-18" & !is.na(htemp$datetime),]

htemp2 <- htemp[htemp$datetime > "2015-09-30" & htemp$datetime < "2015-12-23" & !is.na(htemp$datetime),]

# aggregate to hourly averages 

ht <- aggregate(airt ~ format(htemp1$datetime, "%Y-%m-%d %H"), mean, data = htemp1)
names(ht)[1] = 'datetime'

ht$Year <- as.numeric(substr(ht$datetime, 1, 4))
ht$JDay <- as.numeric(format(strptime(substr(ht$datetime, 1, 10), "%Y-%m-%d"), "%j"))
ht$Hour <- as.numeric(substr(ht$datetime, 12, 13))
names(ht)[2] = "Temp"

# For later harvest 
ht2 <- aggregate(airt ~ format(htemp2$datetime, "%Y-%m-%d %H"), mean, data = htemp2)
names(ht2)[1] = 'datetime'

ht2$Year <- as.numeric(substr(ht2$datetime, 1, 4))
ht2$JDay <- as.numeric(format(strptime(substr(ht2$datetime, 1, 10), "%Y-%m-%d"), "%j"))
ht2$Hour <- as.numeric(substr(ht2$datetime, 12, 13))
names(ht2)[2] = "Temp"


# Natural chilling

chilling(ht, 305, 60) # 32 chill portions as of Dec 18.
chilling(ht2, 305, 60) # 35 chill portions as of Dec 23.

# Chilling in experimental setting. 
diff(as.POSIXlt(c("2015-12-23", "2016-01-01"), "%Y-%m-%d"))

dayseq = seq(as.numeric(format(as.POSIXlt("2016-01-01", "%Y-%m-%d"), "%j")),
             as.numeric(format(as.POSIXlt("2016-01-31", "%Y-%m-%d"), "%j")))

# First seven days at 4 deg
dayseq = 1:7

chill1 <- data.frame(
  Year = as.numeric(rep("2016", length(dayseq)*24)),
  JDay = as.numeric(rep(dayseq, each = 24)),
  Hour = as.numeric(rep(0:23, each = length(dayseq))),
  Temp = 4
  )

# bug: Jday vs JDay, returns error for "chillout not found"

chilling(chill1, 305, 60)

function
