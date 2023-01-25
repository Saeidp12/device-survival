# data sets
df_spc <- read.csv('data-spc.csv', header=TRUE)
df_aird <- read.csv('data-airdryer.csv', header=TRUE)
df_iff <- read.csv('datat-iff.csv', header=TRUE)
df_dc <- read.csv('data-dc.csv', header=TRUE)
df_ant <- read.csv('data-antenna.csv', header=TRUE)

# install.packages('dplyr')
library(dplyr)

# Count the number of rows with 0 and 1
table_1 <- df_spc %>% dplyr::count(RESULT)
table_2 <- df_aird %>% dplyr::count(RESULT)
table_3 <- df_iff %>% dplyr::count(RESULT)
table_4 <- df_dc %>% dplyr::count(RESULT)
table_5 <- df_ant %>% dplyr::count(RESULT)

# Finding the %
pct_spc <- (table_1[1,2]/sum(table_1[,2]))*100
pct_aird <- (table_2[1,2]/sum(table_2[,2]))*100
pct_iff <- (table_3[1,2]/sum(table_3[,2]))*100
pct_dc <- (table_4[1,2]/sum(table_4[,2]))*100
pct_ant <- (table_5[1,2]/sum(table_5[,2]))*100

cat("Percent of zeros for SPC: ", pct_spc, "\n")
cat("Percent of zeros for Airdryer: ", pct_aird, "\n")
cat("Percent of zeros for IFF: ", pct_iff, "\n")
cat("Percent of zeros for DC: ", pct_dc, "\n")
cat("Percent of zeros for ANTENNA: ", pct_ant, "\n")

# This filters the rows that have RESULT = 0:
df_spc_0 <- df_spc %>% filter (RESULT == 0)  
df_aird_0 <- df_aird %>% filter (RESULT == 0) 
df_iff_0 <- df_iff %>% filter (RESULT == 0) 
df_dc_0 <- df_dc %>% filter (RESULT == 0) 
df_ant_0 <- df_ant %>% filter (RESULT == 0) 

# install.packages('survival')
library(survival)

#=========================== spc data
cox.fit <- coxph(Surv(Time..hours., RESULT) ~ M + S + C, data=df_spc)
summary(cox.fit)

# You can change the model according to your needs. For example,
# If you want only the variable M to be in the model:
cox.fit <- coxph(Surv(Time..hours., RESULT) ~ M, data=df_spc)
summary(cox.fit)

#=========================== airdryer data
cox.fit <- coxph(Surv(Time..hours., RESULT) ~ M + S + C, data=df_aird)
summary(cox.fit)

#=========================== iff data
cox.fit <- coxph(Surv(Time..hours., RESULT) ~ M + S + C, data=df_iff)
summary(cox.fit)

#=========================== dc data
cox.fit <- coxph(Surv(Time..hours., RESULT) ~ M + S + C, data=df_dc)
summary(cox.fit)

#=========================== antenna data
cox.fit <- coxph(Surv(Time..hours., RESULT) ~ M + S + C, data=df_ant)
summary(cox.fit)


