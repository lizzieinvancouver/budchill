# Budchill analyses
library(scales)
library(chillR)
library(ggplot2)

setwd("~/Documents/git/budchill")


d <- read.csv('data/Budburst Chill Datasheet.csv')

# 
d$Date <- strptime(d$Date, "%Y-%m-%d")
day1 <- as.numeric(format(d$Date, "%j"))

# from min(d$Date). Actual start of experiment was January 1, for time1
d$day1 <- day1 - as.numeric(format(strptime("2015-01-01", "%Y-%m-%d"), "%j")) + 1 

# for time1 and time2
d$day2 <- day1 - as.numeric(format(strptime("2015-01-16", "%Y-%m-%d"), "%j")) + 1 
d$day3 <- day1 - as.numeric(format(strptime("2015-02-01", "%Y-%m-%d"), "%j")) + 1 

d$day <- ifelse(d$time == "time1", d$day1, ifelse(d$time == "time2", d$day2, d$day3))

# Calculate actual chill units
# chill unit calcs, for each treatment

chillcalc <- function(days, temp){
  Year = 2016
  JDay = as.numeric(gl(days, 24))
  Hour = as.numeric(gl(24, 1, length = days*24))
  Temp = temp
  xx <- data.frame(Year, JDay, Hour, Temp)
  chilling(xx, 1, days)
}

chilltreat = c(1, 2, 4, 8)#seq(-3, 10, by = 0.5)
timetreat = c(16, 32)#seq(8, 8*5, by = 8)

chillportions <- vector()
numdays <- vector()
for(i in chilltreat){
  for(j in timetreat){
    
    chillportions <- c(chillportions, as.numeric(chillcalc(j, i)[6]))
    numdays <- c(numdays, j)
  }
}


dat <- data.frame(days = numdays, 
                  temp = gl(length(chilltreat), length(timetreat), labels = chilltreat), 
                  cp = chillportions)

# Natural chilling
natural = 35.68494
storage = 4.558914 # 7 days at 4 deg

dat <- rbind(data.frame(days = rep(0,4), temp = c(1,2,4,8), cp = rep(0,4)), dat)

dat$chillport = dat$cp + natural + storage

dat$time = c(rep("time1",4),rep(c("time2","time3"), 4))
dat$chill = c(c("chill1","chill2","chill4","chill8"), rep(c("chill1","chill2","chill4","chill8"), each=2))
dat$Treat = paste(dat$temp, substr(dat$time, 5,5), sep="_")

dat <- dat[order(dat$time, dat$chill),]

chillcalcs = dat

# order by date

d <- d[order(d$Date, d$id, d$Treat),]

# clean up formats
# make continuous data, now using second value if something is split by slash
d$tleaf <- unlist(lapply(strsplit(as.character(d$Term.lf), "/"), function(x) ifelse(length(x)>1, x[2], x[1])))
d$lleaf <- unlist(lapply(strsplit(as.character(d$Lat.lf), "/"), function(x) ifelse(length(x)>1, x[2], x[1])))

# and use first values if comma. This is a sequential manipulation, after slash data have been modified.
d$tleaf <- unlist(lapply(strsplit(as.character(d$tleaf), ", "), function(x) ifelse(length(x)>1, x[1], x[1])))
d$lleaf <- unlist(lapply(strsplit(as.character(d$lleaf), ", "), function(x) ifelse(length(x)>1, x[1], x[1])))

for(i in c("tleaf","lleaf","Term.fl","Lat.fl")){
  d[,i][d[,i] =="-"] = NA
  d[,i][d[,i] ==""] = NA
  d[,i][d[,i] =="*"] = NA
}

d$lleaf <- sub("\\*", "", d$lleaf) # get rid of one asterix after 4*

# Make into numeric data, needed because it was read in with weird characters and automaticially made into factors
for(i in c("tleaf","lleaf","Term.fl","Lat.fl")){
  d[,i] = as.numeric(as.character(d[,i])) }


# Add chill portion calcs to this data frame
d$chillport = chillcalcs[match(d$Treat, chillcalcs$Treat),"chillport"] 

# Now summarize to days to bud burst and days to leafout

bday <- lday <- fday <- nl <- vector()

for(i in levels(d$id)){ # i=levels(d$id)[602] # for each individual clipping.
  
  dx <- d[d$id == i,]
  
  # 1. for both terminal and lateral buds, what is the max stage within a row. Identify which rows are greater or equal to the specific BBCH stage
  # 2. now for that individual, find the earliest day at which that stage was reached.
  bdax <- which(apply(dx[,c("tleaf","lleaf")], 1, max, na.rm=T) >= 3)
  if(length(bdax) < 1) bdax = NA else bdax = dx[min(bdax),'day']
  
  ldax <- which(apply(dx[,c("tleaf","lleaf")], 1, max, na.rm=T) >= 6)
  if(length(ldax) < 1) {ldax = NA; nl <- c(nl, 0)} else {ldax = dx[min(ldax),'day']; nl <- c(nl, 1)}
  
  fdax <- which(apply(dx[,c("Term.fl","Lat.fl")], 1, max) > 16)
  if(length(fdax) < 1) fdax = NA else fdax = dx[min(fdax),'day']
  
  bday <- c(bday, bdax)
  lday <- c(lday, ldax)
  fday <- c(fday, fdax)	
}

# merging with unique id data

dx <- d[match(levels(d$id), d$id),] # with twig id in same order as the loop above

dx <- dx[,2:ncol(dx)]

dx <- data.frame(dx, lday, fday, bday, nl)

#
aggregate(dx["bday"], dx[c("chill", "time")], FUN=mean, na.rm=T) # Budburst signficantly earlier with more chilling.


aggregate(dx["bday"], dx[c("chill", "time","sp")], FUN=mean, na.rm=T) # Also clear patterns by species



# Save these

write.csv(dx, "input/Budburst Chill By Day.csv", row.names=F)
write.csv(d, "input/Budburst Chill.csv", row.names=F)

save(list = c('d', 'dx','chillcalcs'), file = paste("input/Budburst Chill Data ", Sys.Date(), ".Rdata", sep="")) # save as R formatted data frames for easier use next time.


# For each species, make trace plots


# re-sort to make sure ordered by date correctly
d <- d[order(d$Date, d$id, d$Treat),]

colz <- c("darkorchid","blue3", "cadetblue","coral3")
lcol <- alpha(colz, 0.1)
names(lcol) = levels(d$chill)


# Pages for each species. Colors for chill treatments. Panels for time

pdf(paste("figures/Trace Plots ", Sys.Date(), ".pdf", sep=""), width = 8, height = 4)

par(mfcol=c(1, 3), mar = c(3,3,1,0.5))
for(spx in levels(d$sp)){ # spx = "BETALL"
  
  dxx = d[d$sp == spx,]
  
  counter = 1
  for(i in sort(as.character((unique(dx$time))))){# i = "time1"
    
    dseq = seq(0, max(dx$day))
    plot(dseq, seq(0, 7,length=length(dseq)), type = "n", 
         ylab = "Stage",
         xlab = "")
    if(counter == 1) mtext(spx, line = -2, adj = 0.5)
    legend("topleft",bty="n",i, cex = 0.85, inset = 0)
    xx <- dxx[dxx$time == i,]
    # calculate mean response by date and chill
    xt <- tapply(pmax(xx$tleaf, xx$lleaf,na.rm=T), list(xx$day, xx$chill), mean, na.rm=T)
    
    for(j in unique(xx$ind)){ #j=unique(xx$ind)[1]
      xj <- xx[xx$ind == j,]
      pcol = lcol[xj$chill]
      lines(xj$day, xj$tleaf, col = pcol)
    }
    lines(rownames(xt), xt[,1], col = colz[1], lwd = 2)
    lines(rownames(xt), xt[,2], col = colz[2], lwd = 2)
    lines(rownames(xt), xt[,3], col = colz[3], lwd = 2)
    lines(rownames(xt), xt[,4], col = colz[4], lwd = 2)
  
    
    # add a legend
    if(counter == 3) {    
    legend("topright", bty = "n",
           col = colz,
           lwd = 2,
           legend = c(1, 2, 4, 8),
           title = "°C")
    }
      
    counter = counter + 1
  }
  
}
dev.off()
system(paste("open '", paste("figures/Trace Plots ", Sys.Date(), ".pdf", sep=""), "' -a /Applications/Preview.app", sep=""))


### Also plot by chill portions

ggplot(dx, aes(chillport, bday)) + geom_point(aes(color = sp)) + geom_smooth(method="lm")

ggplot(dx, aes(jitter(chillport, 5), jitter(bday, 5), group = sp)) + geom_point(aes(color = sp)) + 
    xlab("Chill Portions") + ylab("Day of Budburst") + ylim(0, 40)#+ geom_smooth(method="lm", level = 0.5)

dev.print(pdf, "figures/Budburst x Chill Port.pdf", width = 6, height = 5)
system(paste("open 'figures/Budburst x Chill Port.pdf' -a /Applications/Preview.app", sep=""))


ggplot(dx, aes(jitter(chillport, 5), jitter(lday, 5), group = sp)) + geom_point(aes(color = sp)) + 
  xlab("Chill Portions") + ylab("Day of Leafout") + ylim(0, 40)##+ geom_smooth(method="lm", level = 0.5)

dev.print(pdf, "figures/Leafout x Chill Port.pdf", width = 6, height = 5)
system(paste("open 'figures/Leafout x Chill Port.pdf' -a /Applications/Preview.app", sep=""))
