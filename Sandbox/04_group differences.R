
library(data.table)
library(tidyverse)

# library(data.table) ## Don't forget to load the library
#dat <-  fread('https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv')



# load data ---------------------------------------------------------------


#cpi <-  fread('C:/Users/Ivan/Documents/Projects/Stata2R/cpi ru.csv')
#cpi
#setDT(cpi)
#save(cpi, file = "Sandbox/cpi.Rdata")

load(file = "Sandbox/cpi.Rdata")


#keys <- c('year', 'idind', 'ID_H', 'I1', 'I2', 'J1', 'J10', 'J60', 'age')
#keys


#ru <-  haven::read_dta("C:/Users/Ivan/Documents/Projects/RLMS/USER_RLMS-HSE_IND_1994_2020_v3_eng.dta",
                       #col_select = keys)

#setDT(ru)

#setorder(ru, year,idind) 
#ru[, ]


#save(ru, file = "Sandbox/ru.Rdata")
load(file = "Sandbox/ru.Rdata")


ru[, .(idind, year)]



# rename measures ---------------------------------------------------------


setnames(ru, 'ID_H', 'hhid') 
setnames(ru, 'idind', 'pid') 
setnames(ru, 'I1', 'born') 
setnames(ru, 'I2', 'cob')
setnames(ru, 'J1', 'employed')
setnames(ru, 'J10', 'wages')
setnames(ru, 'J60', 'income')

ru


ru[1:200, .(pid, year, hhid, pid)] 




ru[, year:income] 

ru[, .SD, .SDcols=patterns('*id')]


view(ru)


ru <- ru[(income > 0 & wages > 0 & income < 99999995 & wages < 99999995), ]
ru <- na.omit(ru, cols = c('pid', 'income', 'wages'))



# create new measures -----------------------------------------------------



ru[, income_denom := ifelse(year < 1998, income/1000, income)]

ru[, wage_denom := ifelse(year < 1998, wages/1000, wages)]

###create percentiles

ru[, income_quintile := cut(income_denom, 
                            breaks =quantile(income_denom, probs = 0:5/5, na.rm=TRUE),
                            labels = 1:5, right = FALSE), by=year]



ru[year==2020 & !is.na(income_quintile), .(mean_income=mean(income_denom, na.rm=TRUE),
                                    mean_wages=mean(wage_denom,na.rm=TRUE),
                                    median_income=median(income_denom,na.rm=TRUE),
                                    median_wages=median(wage_denom, na.rm=TRUE)), by = .(income_quintile)]



setorder(ru2, year)
ru2