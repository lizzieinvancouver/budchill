## Started in 2016 by D Flynn ##
## Lizzie started working on it in Jan 2018 ##

# Budburst Chilling Experiment 2016 analysis
rm(list=ls())

# library(nlme) # switching to rstanarm models by Lizzie in Jan 2018
library(scales)
library(arm)
library(rstan)
library(shinystan)
# library(sjPlot)
library(rstanarm)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

runstan = FALSE

# setwd("~/Documents/git/budchill/analyses")
setwd("~/Documents/git/projects/treegarden/budchill/analyses")
source('stan/savestan.R')
print(toload <- sort(dir("./input")[grep("Budburst Chill Data", dir('./input'))], T)[1])

load(file.path("input", toload))

# Initial analysis: by experimental treatment
# convert chill and time to numerics for ordered analysis

dx$chilltemp = as.numeric(substr(as.character(dx$chill), 6, 6))
dx$timetreat = as.numeric(substr(as.character(dx$time), 5, 5))

if(runstan){
m1 <- stan_lmer(bday ~ chilltemp * timetreat + (1|sp), data = dx)
summary(m1)
}

########### USE THIS #################
keepsp <- table(dx$nl, dx$sp)[2,] / table(dx$sp) >= 0.25 # all

if(runstan){
m2 <- stan_lmer(bday ~ (chilltemp*timetreat|sp/ind), data = dx[dx$sp %in% names(keepsp)[keepsp==TRUE],] ) # warnings.
summary(m2)
ranef(m2)

m2l <- stan_lmer(lday ~ (chilltemp*timetreat|sp), data = dx)
summary(m2l)
save(m2l, file="output/m2l.RData")
# load("output/m2l.RData")
}

# temperature effects non linear?
summary.aov(lm(bday ~ chilltemp * timetreat * sp, data = dx))
plot(bday ~ chilltemp * sp, data = dx)

means <- aggregate(dx$bday, list(chill=dx$chilltemp, time=dx$timetreat, sp=dx$sp), mean, na.rm=T)
ses <- aggregate(dx$bday, list(dx$chilltemp, dx$timetreat, dx$sp), function(x) sd(x,na.rm=T)/sqrt(length(x[!is.na(x)])))
datx <- data.frame(means, se=ses$x)

pdf(file="figures/budburst_bytimechill.pdf", width=14, height=6)
ggplot(datx, aes(time, x, group = as.factor(chill), color=chill)) + geom_line(lwd = 2) +
    geom_errorbar(aes(x=time, ymin=x-se, ymax=x+se), width=0) +
    facet_grid(.~sp) + ylab('Day of budburst') +
    xlab('Chilling time') +
    scale_colour_gradient(low="red4", high = "lemonchiffon") # high = "#56B1F7"
dev.off()

# subset down to just 1 and 4 C
datx14 <- subset(datx, chill==1|chill==4)

pdf(file="figures/budburst_bytimechill_1Cand4C.pdf", width=14, height=6)
ggplot(datx14, aes(time, x, group = as.factor(chill), color=chill)) + geom_line(lwd = 2) +
    geom_errorbar(aes(x=time, ymin=x-se, ymax=x+se), width=0) +
    facet_grid(.~sp) + ylab('Day of budburst') +
    xlab('Chilling time') +
    scale_colour_gradient(low="red4", high = "#56B1F7") 
dev.off()

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

######### Stan.

# make dummy vars. Can we do 4 levels of chill treatment without a 'reference' level? More in the spirit of Bayesian..
# now doing with chill1 as reference.
dx$chill1 = ifelse(dx$chill == "chill1", 1, 0) 
dx$chill2 = ifelse(dx$chill == "chill2", 1, 0) 
dx$chill4 = ifelse(dx$chill == "chill4", 1, 0) 
dx$chill8 = ifelse(dx$chill == "chill8", 1, 0) 
dx$time2 = ifelse(dx$time == "time2", 1, 0) 
dx$time3 = ifelse(dx$time == "time3", 1, 0) 

with(dx, table(time2, time3))

with(dx, table(chill1, chill2))
with(dx, table(chill4, chill8))

dxb <- dx[!is.na(dx$bday),] # ignore those which failed to burst bud

datalist.b <- list(lday = dxb$bday, # budburst as respose 
                  sp = as.numeric(dxb$sp), 
                  chill1 = as.numeric(dxb$chill1),
                  chill2 = as.numeric(dxb$chill2),
                  chill4 = as.numeric(dxb$chill4),
                  chill8 = as.numeric(dxb$chill8),
                  time2 = as.numeric(dxb$time2),
                  time3 = as.numeric(dxb$time3),
                   N = nrow(dxb), 
                   n_sp = length(unique(dxb$sp))
)

  doym.b <- stan('stan/chill_time_sp1.stan', 
                 data = datalist.b, iter = 5005, chains = 4,
                 control = list(adapt_delta = 0.9,
                                max_treedepth = 15))

# 266 divergences with all 4 levels, still 128 divergences with 3-level version
  sumerb <- summary(doym.b)$summary
  sumerb[grep("mu_", rownames(sumerb)),]
  
  ssm.b <- as.shinystan(doym.b)
  # launch_shinystan(ssm.b) 
savestan("Chill1 bud")
