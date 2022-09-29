
library(data.table)
library(tidyverse)
library(viridis)
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


ru[, income_decile := cut(income_denom, 
                          breaks =quantile(income_denom, probs = 0:10/10, na.rm=TRUE),
                          labels = 1:10, right = FALSE), by=year]


ru[year==2020, .(mean=mean(cpi_income_2020, na.rm=T))]

cpi[1:200,]

setnames(cpi, 'Year', 'year')
cpi <- cpi[!is.na(CPI_2010)]


cpi[, cpi_2010 := CPI_2010/CPI_2010[year==2010]]
cpi[, cpi_2020 := CPI_2010/CPI_2010[year==2020]]


cpi
view (ru)
ru

ru <- ru[cpi, on = .(year = year)]

ru[1:20]



ru[, cpi_income_2010 := income_denom * (cpi_2010)]
ru[, cpi_income_2020 := income_denom * cpi_2020]


# gini coefficient --------------------------------------------------------
??rank

ru <- ru[year==2020]

ru[,
   rank := rank(cpi_income_2020, ties.method="min")]

ru[,
    total :=.N, by=rank]

ru[,
   total_income := sum(cpi_income_2020)]

ru[,
   total_sample := .N]

ru[,
   ]

rank <- ru[,
   .(mean_income= mean(cpi_income_2020, na.rm=T),
     total_income= mean(total_income, na.rm=T),
     total_sample= mean(total_sample, na.rm=T),
     n= .N),
   by=rank]



setorder(rank, rank) 

setDT(rank)


rank[,
     share_inc := mean_income/total_income]
rank[,
     share_sample := n/total_sample]
rank[,
     share_richer := 1- share_sample]


rank






