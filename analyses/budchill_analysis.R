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

runstan = TRUE

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

########### 1 and 4 C only ###########
dx.14 <- subset(dx, chill=="chill1" | chill=="chill4")

if(runstan){
m1.14 <- stan_lmer(bday ~ (chill*time) +(chill*time|sp), data = dx.14) # warnings.
}
# 1 day advance due to 4 degree temperature (compared to 1 C), 8 and 10 day advance due to longer time ...

#
sumer.m1.14 <- summary(m1.14)
iter.m1.14 <- as.data.frame(m1.14)

# manually to get right order, with intercept
params <- c("chillchill4","timetime2","timetime3",
               "chillchill4:timetime2","chillchill4:timetime3")

col4fig <- c("mean","sd","25%","50%","75%","Rhat")

meanzb.wi <- sumer.m1.14[params,col4fig]

rownames(meanzb.wi) = c("Chilling at 4°C",
                    "16 days additional chilling",
                    "30 days additional chilling",
                    "16 days x chilling 4°",
                    "30 days x chilling 4°C"
                    )

speff.bb <- speff.lo <- vector()

pdf(file.path("figures/m1.14.pdf"), width = 7, height = 6)

par(mfrow=c(1,1), mar = c(2, 10, 2, 1))
# One panel: budburst
plot(seq(-15, #min(meanz[,'mean']*1.1),
         10, #max(meanz[,'mean']*1.1),
         length.out = nrow(meanzb.wi)), 
     seq(1, 5*nrow(meanzb.wi), length.out = nrow(meanzb.wi)),
     type="n",
     xlab = "",
     ylab = "",
     yaxt = "n")

# legend(x =-16.5, y = 2, bty="n", legend = "Budburst", text.font = 2)
# rasterImage(bbpng, -0.25, 1, 0, 4)

axis(2, at = 5*(nrow(meanzb.wi):1), labels = rownames(meanzb.wi), las = 1, cex.axis = 0.8)


# Plot species levels for each predictor
for(i in 1:length(unique(dx.14$sp))){
  b.params <- iter.m1.14[!is.na(match(colnames(iter.m1.14), c(paste("b", "[", params, " sp:",
      unique(dx$sp)[i], "]", sep=""))))]

  main.params <- iter.m1.14[!is.na(match(colnames(iter.m1.14), params))]

  bplusmain <- b.params
  for(c in 1:ncol(main.params)){
      bplusmain[c] <- b.params[c]+main.params[c]
      }

  bplusmain.quant <- sapply(bplusmain, FUN = quantile, probs = c(0.25, 0.50, 0.75))
  
  sp.est <- t(bplusmain.quant)
  
  jt <- jitter(0, factor = 40)

  arrows(sp.est[,"75%"],  jt+(5*(nrow(meanzb.wi):1)-1), sp.est[,"25%"],  jt+(5*(nrow(meanzb.wi):1)-1),
         len = 0, col = alpha("firebrick", 0.2)) 
  
  points(sp.est[,'50%'],
         jt+(5*(nrow(meanzb.wi):1)-1), #[c(3:5,11:12)], # ADJUSTED for just the ranef here
         pch = 16,
         col = alpha("firebrick", 0.5))

  speff.bb = rbind(speff.bb, t(sp.est[,1]))
    }

arrows(meanzb.wi[,"75%"], (5*(nrow(meanzb.wi):1))+0.25, meanzb.wi[,"25%"], (5*(nrow(meanzb.wi):1))+0.25,
       len = 0, col = "black", lwd = 3)

points(meanzb.wi[,'mean'],
       (5*(nrow(meanzb.wi):1))+0.25,
       pch = 16,
       cex = 1,
       col = "midnightblue")
abline(v = 0, lty = 2)
dev.off()

########### USE THIS (Dan's analyses) #################
keepsp <- table(dx$nl, dx$sp)[2,] / table(dx$sp) >= 0.25 # all

if(runstan){
m2 <- stan_lmer(bday ~ (chilltemp*timetreat|sp/ind), data = dx[dx$sp %in% names(keepsp)[keepsp==TRUE],] ) # note that this one does not give overall effects across species!
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
