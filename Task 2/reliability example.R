library(SPREDA)
library(boot)
library(lattice)

source("https://raw.githubusercontent.com/CodeOwl94/ross-reliability/master/ReliabilitySupportFns.R")
exa1 <- read.csv("https://raw.githubusercontent.com/CodeOwl94/ross-reliability/master/EXA1.csv", header=T)
dim(exa1)
stack(table(exa1$fail))

exa1$fail <- ifelse(exa1$fail=="S","T",as.character(exa1$fail))
exa1.dat <- data.frame(time=exa1$time,event=1-as.numeric(as.logical(exa1$fail)))
str(exa1.dat)

stack(table(exa1.dat$event))                       

Plot.Observations(exa1.dat)
title("Figure 1", adj=1)

plot(ecdf(exa1.dat[exa1.dat$event==1,"time"]),main="",xlab="time",
     verticals=T,las=1,adj=0.5) 
abline(v=quantile(exa1.dat[exa1.dat$event==1,"time"],probs=c(0.25,0.5,0.75)),
       col='red',lwd=2,lty=2) # add on quartiles
title("Figure 4: ECDF", adj=1)

# We can adjust these relative frequencies for the censored observations to 
# obtain non-parametric estimates of the probability of failure with time, F(t)

hist(exa1.dat[exa1.dat$event==1,"time"],main="",col="lightgrey",
     xlab="time (non-censored measurements)",las=1,adj=0.5)
title("Figure 5: Histogram of Failures", adj=1)

exa1.hist = hist(exa1.dat$time, plot=FALSE)
attach(exa1.hist)
data.frame(time=mids, n=counts)

detach(exa1.hist) # removes attached exa1.hist elements from the workspace.
rm(exa1.hist) # removes exa1.hist from our workspace, as we are finished with it.

Probability.Plots(exa1.dat)

##############
fit.norm.mle = fitdist(exa1.dat$time, 'weibull', method='mle')
summary(fit.norm.mle)
plot(fit.norm.mle)

fit2 = fitdist(x1, 'lnorm', method='mle')
summary(fit2)
plot(fit2)
