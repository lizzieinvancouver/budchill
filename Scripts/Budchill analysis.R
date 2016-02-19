# Budburst Chilling Experiment 2016 analysis
library(nlme)
library(scales)
library(arm)
library(rstan)
library(sjPlot)

rm(list=ls())

setwd("~/Documents/git/budchill/")

print(toload <- sort(dir("./input")[grep("Budburst Chill Data", dir('./input'))], T)[1])

load(file.path("input", toload))

# Initial analysis: by experimental treatment
# convert chill and time to numerics for ordered analysis

dx$chilltemp = as.numeric(substr(as.character(dx$chill), 6, 6))
dx$timetreat = as.numeric(substr(as.character(dx$time), 5, 5))

m1 <- lmer(bday ~ chilltemp * timetreat + (1|sp), data = dx)
summary(m1)
sjp.lmer(m1, type = 'fe')


########### USE THIS #################
# only run for species which have > 25% budburst

keepsp <- table(dx$nl, dx$sp)[2,] / table(dx$sp) > 0.25 # now three species

m2 <- lmer(bday ~ (chilltemp*timetreat|sp/ind), data = dx[dx$sp %in% names(keepsp)[keepsp==T],] )
summary(m2)
ranef(m2)
sjp.lmer(m2, type = 're')

sjt.lmer(m2)

m2l <- lmer(lday ~ (chilltemp*timetreat|sp), data = dx)
summary(m2l)
sjp.lmer(m2l, type = 're')

# temperature effects non linear?

summary.aov(lm(bday ~ chilltemp * timetreat * sp, data = dx))
plot(bday ~ chilltemp * sp, data = dx)

means <- aggregate(dx$bday, list(chill=dx$chilltemp, time=dx$timetreat, sp=dx$sp), mean, na.rm=T)
ses <- aggregate(dx$bday, list(dx$chilltemp, dx$timetreat, dx$sp), function(x) sd(x,na.rm=T)/sqrt(length(x[!is.na(x)])))
datx <- data.frame(means, se=ses$x)


ggplot(datx, aes(time, x, group = chill)) + geom_line(aes(color=chill), lwd = 2) + facet_grid(.~sp) + ylab('Day of budburst') + xlab('Chilling time')


m3 <- lmer(bday ~ chillport + chilltemp * timetreat + (1|sp), data = dx)
summary(m3)
sjp.lmer(m3, type = 'fe')

# Are chill portions better predictors of budburst than temperature?

m4 <- lmer(bday ~ chillport * chilltemp * timetreat + (1|sp), data = dx)
summary(m4)
sjp.lmer(m3, type = 'fe')

AIC(m1,m3,m4)

# no, calculated chill portions at least by AIC are not as good as chill temp timetreat
m5 <- lmer(bday ~ chillport  + (1|sp/ind), data = dx)
m6 <- lmer(bday ~ chilltemp  * timetreat + (1|sp/ind), data = dx)

AIC(m5, m6)

summary(m5)
summary(m6)

# check for leafout. Now not so different, but not better
m5 <- lmer(lday ~ chillport  + (1|sp/ind), data = dx)
m6 <- lmer(lday ~ chilltemp  * timetreat + (1|sp/ind), data = dx)

AIC(m5, m6)

summary(m5)
summary(m6)

