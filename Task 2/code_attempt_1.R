library(fitdistrplus)

time = seq(2000, 6200, by=300)

dccc_num_fails = c(3, 4, 1, 4, 5, 3, 2, 3, 4, 2, 1, 5, 3, 5, 3)
dccc_data = data.frame(time=time, num_of_fails=dccc_num_fails)

spc_num_fails = c(0, 5, 2, 1, 1, 2, 1, 1, 4, 5, 2, 5, 6, 5, 3)
spc_data = data.frame(time=time, num_of_fails=spc_num_fails)

antenna_num_fails = c(0, 0, 0, 0, 1, 1, 1, 1, 3, 2, 2, 1, 2, 3, 3)
antenna_data = data.frame(time=time, num_of_fails=antenna_num_fails)

aird_num_fails = c(0, 0, 1, 1, 1, 2, 1, 1, 3, 2, 2, 3, 4, 2, 2)
aird_data = data.frame(time=time, num_of_fails=aird_num_fails)

iff_num_fails = c(0,0,1,0,1,0,0,1,1,2,1,5,3,5,3)
iff_data = data.frame(time=time, num_of_fails=iff_num_fails)

fail_data = data.frame(time=time, dccc=dccc_num_fails, spc=spc_num_fails,
                       antenna=antenna_num_fails, airdryer=aird_num_fails,
                       iff=iff_num_fails)

descdist(fail_data$dccc, discrete = F)
descdist(fail_data$spc, discrete = F)
descdist(fail_data$antenna, discrete = F)
descdist(fail_data$airdryer, discrete = F)
descdist(fail_data$iff, discrete = F)


#DCCC Data Analysis
# Normal


# Maximum Likelihood
fit.norm.mle = fitdist(fail_data$dccc, 'norm', method='mle')
summary(fit.norm.mle)
plot(fit.norm.mle)

# Matching Moments
fit.norm.mme = fitdist(fail_data$dccc, 'norm', method='mme')
summary(fit.norm.mme)
plot(fit.norm.mme)

# Quantile Matching
#fit.norm.qme = fitdist(fail_data$dccc, 'norm', method='qme')
#summary(fit.norm.qme)

# Maximum Goodness-of-Fit CvM
fit.norm.gme.cvm = mgedist(fail_data$dccc, 'norm', gof='CvM')

# Maximum Goodness-of-Fit KS
fit.norm.gme.ks = mgedist(fail_data$dccc, 'norm', gof='KS')

# Maximum Goodness-of-Fit AD
fit.norm.gme.ad = mgedist(fail_data$dccc, 'norm', gof='AD')


plot(table(fail_data$dccc))





