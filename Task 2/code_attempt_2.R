df_spc = read.csv('E:/2022/Freelance/Jobs/001-25/data-spc.csv', header=T)
df_spc = data.frame(time=df_spc$Time..hours., fails=df_spc$Number.of.failures)

time_rep = rep(df_spc$time, df_spc$fails)
descdist(time_rep)

###################### Weibull ################################################

info_df = data.frame(model=NA, method=NA,Log_likelihood=NA, AIC=NA)

########## MLE ################
fit_wei_mle = fitdist(time_rep, 'weibull', method='mle')
summary(fit_wei_mle)
plot(fit_wei_mle)
info_df[1, ] = c('weibull','MLE',fit_wei_mle$loglik, fit_wei_mle$aic)

########## MME ################
library(actuar)
memp <- function(x, order) mean(x^order)
fit_wei_mme = fitdist(time_rep, 'weibull', method='mme', order=c(1,2), memp=memp)
summary(fit_wei_mme)
plot(fit_wei_mme)
info_df[2, ] = c('weibull','MME',fit_wei_mme$loglik, fit_wei_mme$aic)

########## QME ################
fit_wei_qme = fitdist(time_rep, 'weibull', method='qme', probs=c(0.5,0.5))
summary(fit_wei_qme)
plot(fit_wei_qme)
info_df[3, ] = c('weibull','QME',fit_wei_qme$loglik, fit_wei_qme$aic)

########## MGE ################
fit_wei_mge = fitdist(time_rep, 'weibull', method='mge', gof='KS')
summary(fit_wei_mge)
plot(fit_wei_mge)
info_df[4, ] = c('weibull','MGE KS',fit_wei_mge$loglik, fit_wei_mge$aic)

fit_wei_mge.2 = fitdist(time_rep, 'weibull', method='mge', gof='CvM')
summary(fit_wei_mge.2)
plot(fit_wei_mge.2)
info_df[5, ] = c('weibull','MGE CvM',fit_wei_mge.2$loglik, fit_wei_mge.2$aic)

fit_wei_mge.3 = fitdist(time_rep, 'weibull', method='mge', gof='AD')
summary(fit_wei_mge.3)
plot(fit_wei_mge.3)
info_df[6, ] = c('weibull','MGE AD',fit_wei_mge.3$loglik, fit_wei_mge.3$aic)


###################### Gamma ################################################
# MLE
fit_gam_mle = fitdist(time_rep, 'gamma', method='mle')
summary(fit_gam_mle)
plot(fit_gam_mle)
info_df[7, ] = c('gamma','MLE',fit_gam_mle$loglik, fit_gam_mle$aic)

# MME
fit_gam_mme = fitdist(time_rep, 'gamma', method='mme')
summary(fit_gam_mme)
plot(fit_gam_mme)
info_df[8, ] = c('gamma','MME',fit_gam_mme$loglik, fit_gam_mme$aic)

# QME
fit_gam_qme = fitdist(time_rep, 'gamma', method='qme', probs=c(0.5,0.5))
summary(fit_gam_qme)
plot(fit_gam_qme)
info_df[9, ] = c('gamma','QME',fit_gam_qme$loglik, fit_gam_qme$aic)

# MGE
fit_gam_mge = fitdist(time_rep, 'gamma', method='mge', gof='KS')
summary(fit_gam_mge)
plot(fit_gam_mge)
info_df[10, ] = c('gamma','MGE KS',fit_gam_mge$loglik, fit_gam_mge$aic)

fit_gam_mge.2 = fitdist(time_rep, 'gamma', method='mge', gof='CvM') # Doesn't work
summary(fit_gam_mge.2)
plot(fit_gam_mge.2)
info_df[11, ] = c('gamma','MGE CvM',fit_gam_mge.2$loglik, fit_gam_mge.2$aic)

fit_gam_mge.3 = fitdist(time_rep, 'gamma', method='mge', gof='AD')
summary(fit_gam_mge.3)
plot(fit_gam_mge.3)
info_df[12, ] = c('gamma','MGE AD',fit_gam_mge.3$loglik, fit_gam_mge.3$aic)


###################### lnorm ################################################
# MLE
fit_lnorm_mle = fitdist(time_rep, 'lnorm', method='mle')
summary(fit_lnorm_mle)
plot(fit_lnorm_mle)
info_df[13, ] = c('lnorm','MLE',fit_lnorm_mle$loglik, fit_lnorm_mle$aic)

# MME
fit_lnorm_mme = fitdist(time_rep, 'lnorm', method='mme')
summary(fit_lnorm_mme)
plot(fit_lnorm_mme)
info_df[14, ] = c('lnorm','MME',fit_lnorm_mme$loglik, fit_lnorm_mme$aic)

# QME
fit_lnorm_qme = fitdist(time_rep, 'lnorm', method='qme', probs=c(0.5,0.5))
summary(fit_lnorm_qme)
plot(fit_lnorm_qme)
info_df[15, ] = c('lnorm','QME',fit_lnorm_qme$loglik, fit_lnorm_qme$aic)

# MGE
fit_lnorm_mge = fitdist(time_rep, 'lnorm', method='mge', gof='KS')
summary(fit_lnorm_mge)
plot(fit_lnorm_mge)
info_df[16, ] = c('lnorm','MGE KS',fit_lnorm_mge$loglik, fit_lnorm_mge$aic)

fit_lnorm_mge.2 = fitdist(time_rep, 'lnorm', method='mge', gof='CvM') 
summary(fit_lnorm_mge.2)
plot(fit_lnorm_mge.2)
info_df[17, ] = c('lnorm','MGE CvM',fit_lnorm_mge.2$loglik, fit_lnorm_mge.2$aic)

fit_lnorm_mge.3 = fitdist(time_rep, 'lnorm', method='mge', gof='AD')
summary(fit_lnorm_mge.3)
plot(fit_lnorm_mge.3)
info_df[18, ] = c('lnorm','MGE AD',fit_lnorm_mge.3$loglik, fit_lnorm_mge.3$aic)



###################### Beta ################################################
# In order to use beta distribution we need to normalize values within [0,1]
# By dividing the time values to 10000 we reach this requirement:
time_rep_beta = time_rep/10000
# MLE
fit_beta_mle = fitdist(time_rep_beta, 'beta', method='mle')
summary(fit_beta_mle)
plot(fit_beta_mle)
info_df[19, ] = c('beta','MLE',fit_beta_mle$loglik, fit_beta_mle$aic)

# MME
fit_beta_mme = fitdist(time_rep_beta, 'beta', method='mme')
summary(fit_beta_mme)
plot(fit_beta_mme)
info_df[20, ] = c('beta','MME',fit_beta_mme$loglik, fit_beta_mme$aic)

# QME
fit_beta_qme = fitdist(time_rep_beta, 'beta', method='qme', probs=c(0.5,0.5))
summary(fit_beta_qme)
plot(fit_beta_qme)
info_df[21, ] = c('beta','QME',fit_beta_qme$loglik, fit_beta_qme$aic)

# MGE
fit_beta_mge = fitdist(time_rep_beta, 'beta', method='mge', gof='KS')
summary(fit_beta_mge)
plot(fit_beta_mge)
info_df[22, ] = c('beta','MGE KS',fit_beta_mge$loglik, fit_beta_mge$aic)

fit_beta_mge.2 = fitdist(time_rep_beta, 'beta', method='mge', gof='CvM') 
summary(fit_beta_mge.2)
plot(fit_beta_mge.2)
info_df[23, ] = c('beta','MGE CvM',fit_beta_mge.2$loglik, fit_beta_mge.2$aic)

fit_beta_mge.3 = fitdist(time_rep_beta, 'beta', method='mge', gof='AD')
summary(fit_beta_mge.3)
plot(fit_beta_mge.3)
info_df[24, ] = c('beta','MGE AD',fit_beta_mge.3$loglik, fit_beta_mge.3$aic)

info_df




