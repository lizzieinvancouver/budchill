# Calculating chilling portions for new chilling experiment

# 4 chilling levels, including control
# 6 species
# 12 reps / species / level
# 2 time points after control
4 * 6 * 8 * 3 + 4 * 4 * 3


chilltreat = c(1, 2, 4, 8)

timetreat = c(1, 15, 30)

spp = c("acesac", "querub","hamvir","vibcas","faggra", "ilemuc")

chillev = length(chilltreat)
spno = length(spp)
repno = 8
timeno = length(timetreat)
lengthout = chillev*spno*repno*timeno 

time <- gl(timeno, chillev*spno*repno, length = lengthout, labels = timetreat) # two time blocks
sp <- gl(spno, repno*chillev, length = lengthout, labels = spp) # within time block, have species repeat rep * chilllev times
chill <- gl(chillev, repno, length = lengthout, labels = chilltreat) # for each species in each time block, repeat rep number of times
reps <- gl(repno, 1, length = lengthout) # for each treatment combination, give number of reps


design <- data.frame(time, sp, chill, rep=reps)

design[1:50,]

# Now calculating chilling portions

library(chillR)

# chill unit calcs, for each treatment

chillcalc <- function(days, temp){
	Year = 2016
	JDay = as.numeric(gl(days, 24))
	Hour = as.numeric(gl(24, 1, length = days*24))
	Temp = temp
	xx <- data.frame(Year, JDay, Hour, Temp)
	chilling(xx, 1, days)
	}

chillportions <- vector()
numdays <- vector()

chilltreat = seq(1, 10, by = 0.5)
timetreat = seq(10, 40, by = 5)

for(i in chilltreat){
	for(j in timetreat){
	
	chillportions <- c(chillportions, as.numeric(chillcalc(j, i)[6]))
	numdays <- c(numdays, j)
	}
}
	
dat <- data.frame(days = numdays, 
					chill = gl(length(chilltreat), length(timetreat), labels = chilltreat), 
					cp = chillportions)

library(ggplot2)

#qplot(days, cp, color = chill, data = dat)

qplot(chill, cp, color = days, data = dat, 
	ylab = "Chill Portions \n (Dynamic Model)", 
	xlab = "Chilling Treatment (°C)")

### Experimental design

chillportions <- vector()
numdays <- vector()

chilltreat = c(1, 2, 4, 8)
timetreat = c(10, 20)

for(i in chilltreat){
	for(j in timetreat){
	
	chillportions <- c(chillportions, as.numeric(chillcalc(j, i)[6]))
	numdays <- c(numdays, j)
	}
}
	
dat <- data.frame(days = numdays, 
					chill = gl(length(chilltreat), length(timetreat), labels = chilltreat), 
					cp = chillportions)

library(ggplot2)

#qplot(days, cp, color = chill, data = dat)

qplot(chill, cp, color = days, data = dat, 
	ylab = "Chill Portions \n (Dynamic Model)", 
	xlab = "Chilling Treatment (°C)",
	ylim = c(0, 25))
