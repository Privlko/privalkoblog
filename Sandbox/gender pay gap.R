

library(data.table)
library(tidyverse)
library(viridis)
library(gt)

load(file = "C:\\Users\\Ivan\\Documents\\Projects\\Blog\\Sandbox\\ru.Rdata")



ru


# rename measures ---------------------------------------------------------


setnames(ru, 'ID_H', 'hhid') 
setnames(ru, 'idind', 'pid') 
setnames(ru, 'I1', 'born') 
setnames(ru, 'I2', 'cob')
setnames(ru, 'J1', 'employed')
setnames(ru, 'J10', 'wages')
setnames(ru, 'J60', 'income')
setnames(ru, 'H5', 'gender')




# correct for extreme values ----------------------------------------------




ru <-  ru %>% 
  mutate(income = ifelse(income > 99999994, NA, income))

ru[, income_denom := ifelse(year < 1998, income/1000, income)]

ru[, wage_denom := ifelse(year < 1998, wages/1000, wages)]

# sort --------------------------------------------------------------------
setcolorder(ru, c("hhid", "year", "pid"))

setorder(ru, hhid, year, pid)



# equivalence scale B -----------------------------------------------------

ru[, n_within_hhid := rowid(hhid, year)]


ru[, equiv := case_when(n_within_hhid == 1 ~ 1,
                        n_within_hhid > 1 & age > 18 ~ 0.5,
                        n_within_hhid > 1 & age < 18 ~ 0.3)]

ru[,  hh_equiv := sum(equiv) , by = .(hhid, year)]
ru[,  hh_income := sum(income_denom, na.rm=T) , by = .(hhid, year)]
ru[,  hh_equiv_income := hh_income/hh_equiv , by = .(hhid, year)]



ru[, ]



setcolorder(ru, c('hhid','pid', 'year', 'gender', 'age', 'income_denom', 'hh_equiv_income' ))



# bigger files ------------------------------------------------------------

# dat = haven::read_dta("file.dta")
# dat = haven::read_dta("file.dta", col_select=var1:var4)
# setDT(dat) # i.e. Set as a data.table

# haven::write_dta(dat, "file.dta")



ru[, .SD, .SDcols=patterns('hh_*')]


ru
d1 <- ru[, .(mean_income = mean(income_denom, na.rm=T),
       mean_equiv_income = mean(hh_equiv_income, na.rm=T),
       median_income = median(income_denom, na.rm=T),
       median_equiv_income = median(hh_equiv_income, na.rm=T)), by=.(year, gender)] 


as_tibble(d1) %>% 
  arrange(gender, year) %>% 
  ggplot(aes(x= year,
             y=median_equiv_income,
             group=as.factor(gender)))+
  geom_point(aes(colour=as.factor(gender)))+
  geom_line(aes(colour=as.factor(gender)))+
  scale_y_log10(label=scales::comma)


as_tibble(d1) %>% 
  arrange(gender, year) %>% 
  ggplot(aes(x= year,
             y=mean_equiv_income,
             group=as.factor(gender)))+
  geom_point(aes(colour=as.factor(gender)))+
  geom_line(aes(colour=as.factor(gender)))+
  scale_y_log10(label=scales::comma)



