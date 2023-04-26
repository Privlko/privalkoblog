library(data.table)
library(tidyverse)
library(viridis)
# library(data.table) ## Don't forget to load the library
#dat <-  fread('https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv')



# load data ---------------------------------------------------------------

load(file = "Sandbox/cpi.Rdata")


#keys <- c('year', 'idind', 'ID_H', 'I1', 'I2', 'J1', 'J10', 'J60', 'age', 'H5')
#keys


#ru <-  haven::read_dta("C:/Users/Ivan/Documents/Projects/RLMS/USER_RLMS-HSE_IND_1994_2020_v3_eng.dta",
#col_select = keys)

#setDT(ru)

#setorder(ru, year,idind) 
ru[, ]


#save(ru, file = "Sandbox/ru.Rdata")
load(file = "Sandbox/ru.Rdata")


ru[, ]

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



ru[1:12, .(pid, year, hhid, pid)] 
ru[, year:income] 
ru[, .SD, .SDcols=patterns('*id')]




#ru <- na.omit(ru, cols = c('pid', 'income', 'wages'))

ru <-  ru %>% 
  mutate(income = ifelse(income > 99999994, NA, income))



ru[1:100, ]

ru[, income_denom := ifelse(year < 1998, income/1000, income)]

ru[, wage_denom := ifelse(year < 1998, wages/1000, wages)]

# sort --------------------------------------------------------------------
setcolorder(ru, c("hhid", "year", "pid"))

setorder(ru, hhid, year, pid)
ru[1:12, ]




# equivalence scale A -----------------------------------------------------



ru[, n_within_hhid := rowid(hhid, year)]


ru[hhid==10013 & year>2009, ]
ru[, equiv := case_when(n_within_hhid == 1 ~ 1,
                        n_within_hhid > 1 & age > 18 ~ 0.7,
                        n_within_hhid > 1 & age < 18 ~ 0.5)]

ru[1:10,]

ru[,  hh_equiv := sum(equiv) , by = .(hhid, year)]
t1 <- ru[hhid==10013 & year>2009,.(hhid, year, pid, gender, age, income, n_within_hhid, equiv,hh_equiv)]



gt(t1) %>% 
  tab_header(
  title = md("**One household over time**"),
  subtitle = "Following one household 2010-2020") %>% 
  tab_source_note(source_note = "Source: RLMS analysis") %>% 
  tab_footnote(footnote = "The Americas.",
               locations = cells_body(columns = income, rows = 3:4)) %>% 
  tab_spanner(
    label = "Identifiers",
    columns = c(hhid, year, pid)
  ) %>%
  tab_spanner(
    label = "Demographics",
    columns = c(gender, age, income,n_within_hhid)) %>% 
  tab_spanner(label = "Equivalence scale",
    columns = c(equiv, hh_equiv)) %>% 
  fmt_number(
    columns = c(income),
    decimals = 0,
    use_seps = TRUE)



 ru[,  hh_income := sum(income_denom, na.rm=T) , by = .(hhid, year)]
ru[,  hh_equiv_income := hh_income/hh_equiv , by = .(hhid, year)]
ru[1:13,]


ru[hhid==10013 & year>2009,]

setorder(ru, year)


t1 <- ru[, .(mean_income = mean(income_denom, na.rm=T),
       mean_hh_income = mean(hh_income, na.rm=T),
       mean_hh_equiv_income = mean(hh_equiv_income, na.rm=T)), 
   by = year]

t1
p1<- melt(t1, id.vars = c("year"),
             measure.vars = c("mean_income", "mean_hh_income", "mean_hh_equiv_income"))


ggplot(p1,
       aes(y=value,
           x=year,
           group=variable)) +
  geom_point(aes(colour=variable)) +
  geom_line(aes(colour=variable)) +
  scale_y_log10(labels=scales::comma) +
  theme_minimal() +
  labs(title="Trends in three income measures",
       subtitle = "Average individual income, household income, and equivalised household income over time",
       x='Year',
       y='Rubles',
       caption = "Source: RLMS 1991-2020 \nAnalysis: RussianNumbers.substack.com") +
  scale_colour_discrete(name = "Measure", labels = c("Mean income", "Mean HH income", "Mean equivalised \nHH income")) +
  theme(
    plot.title = element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5))




# equivalence scale B -----------------------------------------------------



ru[, n_within_hhid := rowid(hhid, year)]


ru[hhid==10013 & year>2009, ]
ru[, equiv := case_when(n_within_hhid == 1 ~ 1,
                        n_within_hhid > 1 & age > 18 ~ 0.5,
                        n_within_hhid > 1 & age < 18 ~ 0.3)]

ru[1:10,]

ru[,  hh_equiv := sum(equiv) , by = .(hhid, year)]
ru[hhid==10013 & year>2009,]


ru[,  hh_income := sum(income_denom, na.rm=T) , by = .(hhid, year)]
ru[,  hh_equiv_income := hh_income/hh_equiv , by = .(hhid, year)]
ru[1:13,]


ru[hhid==10013 & year>2009,]

setorder(ru, year)


t1 <- ru[, .(mean_income = mean(income_denom, na.rm=T),
             mean_hh_income = mean(hh_income, na.rm=T),
             mean_hh_equiv_income = mean(hh_equiv_income, na.rm=T)), 
         by = year]

t1
p1<- melt(t1, id.vars = c("year"),
          measure.vars = c("mean_income", "mean_hh_income", "mean_hh_equiv_income"))


p_equiv <- ggplot(p1,
       aes(y=value,
           x=year,
           group=variable)) +
  geom_point(aes(colour=variable)) +
  geom_line(aes(colour=variable)) +
  scale_y_log10(labels=scales::comma) +
  theme_minimal() +
  labs(title="Trends in three income measures",
       subtitle = "Average individual income, household income, and equivalised household income over time",
       x='Year',
       y='Rubles',
       caption = "Source: RLMS 1991-2020 \nAnalysis: RussianNumbers.substack.com") +
  scale_colour_discrete(name = "Measure", labels = c("Mean income", "Mean HH income", "Mean equivalised \nHH income")) +
  theme(
    plot.title = element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5))


save(p_equiv, file = "Sandbox/p_equiv")

# compare both scales-----------------------------------------------------------------

ru[, equiv_a := case_when(n_within_hhid == 1 ~ 1,
                        n_within_hhid > 1 & age > 18 ~ 0.7,
                        n_within_hhid > 1 & age < 18 ~ 0.5)]

ru[1:10,]

ru[,  hh_equiv_a := sum(equiv_a) , by = .(hhid, year)]
ru[hhid==10013 & year>2009,]


ru[,  hh_income := sum(income_denom, na.rm=T) , by = .(hhid, year)]
ru[,  hh_equiv_income_a := hh_income/hh_equiv_a , by = .(hhid, year)]
ru[1:13,]


ru[hhid==10013 & year>2009,]

setorder(ru, year)


ru[hhid==10013 & year>2009, ]
ru[, equiv_b := case_when(n_within_hhid == 1 ~ 1,
                        n_within_hhid > 1 & age > 18 ~ 0.5,
                        n_within_hhid > 1 & age < 18 ~ 0.3)]

ru[1:10,]

ru[,  hh_equiv_b := sum(equiv_b) , by = .(hhid, year)]
ru[hhid==10013 & year>2009,]


ru[,  hh_income := sum(income_denom, na.rm=T) , by = .(hhid, year)]
ru[,  hh_equiv_income_b := hh_income/hh_equiv_b , by = .(hhid, year)]
ru[1:13,]


ru[hhid==10013 & year>2009,]

setorder(ru, year)











t2 <- ru[, .(mean_hh_equiv_income_a = mean(hh_equiv_income_a, na.rm=T),
             mean_hh_equiv_income_b = mean(hh_equiv_income_b, na.rm=T)), 
         by = year]

t2
p1<- melt(t2, id.vars = c("year"),
          measure.vars = c("mean_hh_equiv_income_b", "mean_hh_equiv_income_a"))


comp <- ggplot(p1,
       aes(y=value,
           x=year,
           group=variable)) +
  geom_point(aes(colour=variable)) +
  geom_line(aes(colour=variable)) +
  scale_y_log10(labels=scales::comma) +
  theme_minimal() +
  labs(title="Trends in three income measures",
       subtitle = "Average individual income, household income, and equivalised household income over time",
       x='Year',
       y='Rubles',
       caption = "Source: RLMS 1991-2020 \nAnalysis: RussianNumbers.substack.com") +
  scale_colour_discrete(name = "Measure", labels = c("Mean HH Modified OECD Scale \nEquivalised income", "Mean HH OECD Scale \nEquivalised income")) +
  theme(
    plot.title = element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5))


comp
save(comp, file = "Sandbox/comp")



# income poverty ----------------------------------------------------------

ru[, .(pid, year, hhid, pid, )]
ru[1:12, .(pid, year, hhid, pid, hh_equiv_income_a )] 

ru[,  threshold_a := (0.6*median(hh_equiv_income_a, na.rm=T)), by = .(year)]
ru[,  threshold_b := (0.5*median(hh_equiv_income_a, na.rm=T)), by = .(year)]
ru[,  threshold_c := (0.4*median(hh_equiv_income_a, na.rm=T)), by = .(year)]




ru[1:120, .(pid, year, hhid, pid, hh_equiv_income_a , threshold_a,threshold_b,threshold_c)] 


ru[, .(mean(threshold_a),
       mean(threshold_b),
       mean(threshold_c)), by=year]
