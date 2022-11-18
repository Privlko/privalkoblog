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


#keys <- c('year', 'idind', 'ID_H', 'I1', 'I2', 'J1', 'J10', 'J60', 'age', 'H5')
#keys


#ru <-  haven::read_dta("C:/Users/Ivan/Documents/Projects/RLMS/USER_RLMS-HSE_IND_1994_2020_v3_eng.dta",
                       #col_select = keys)

#setDT(ru)

#setorder(ru, year,idind) 
ru[, ]


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
setnames(ru, 'H5', 'gender')

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


ru[, wage_decile := cut(wage_denom, 
                            breaks =quantile(wage_denom, probs = 0:10/10, na.rm=TRUE),
                            labels = 1:10, right = FALSE), by=year]



ru[, median_inc := median(income_denom), by=year] 
ru[, bench := 0.6*median_inc, by=year]
ru[, poverty := ifelse(income_denom < bench, 1, 0)]
ru[, .(income_denom, median_inc, bench, poverty)]


ru


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

# poverty risk over time --------------------------------


setorder(ru, year, poverty)

ru2 <- ru[!is.na(poverty), 
   .(mean_poverty=mean(poverty, na.rm=TRUE)), 
   by = .(year)]

#save(ru2, file = "Sandbox/ru2.Rdata")


ru2

ggplot(ru2, aes(y=mean_poverty,
                x=year))+
  geom_point(aes()) + 
  geom_line(aes()) +
  scale_y_continuous(labels=scales::percent) +
  theme_minimal()+
  labs(title="Trends in  at-risk of poverty rate",
       subtitle = "\nTrend uses 60% of median income benchmark (1994-2020)",
       x='Year',
       y='At-Risk of Poverty Rate %',
       caption = "Source: RLMS 1991-2020 \nAnalysis: Ivan Privalko")  +
  theme(
    plot.title = element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5))




# group differences over time ---------------------------------------------


ru3 <- ru[!is.na(poverty), 
          .(mean_poverty=mean(poverty, na.rm=TRUE)), 
          by = .(year, gender)]

ru3



ggplot(ru3, aes(y=mean_poverty,
                x=year,
                group=gender)) +
  geom_point(aes(colour=as.factor(gender))) + 
  geom_line(aes(colour=as.factor(gender))) +
  scale_y_continuous(labels=scales::percent) +
  theme_minimal() +
  labs(title="Trends in  at-risk of poverty rate for men and women",
       subtitle = "\nTrend uses 60% of median income benchmark (1994-2020)",
       x='Year',
       y='At-Risk of Poverty Rate %',
       caption = "Source: RLMS 1991-2020 \nAnalysis: Ivan Privalko")  +
  theme(
    plot.title = element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5))




# group differences over time ---------------------------------------------


ru3 <- ru[employed==1, 
          .(mean_poverty=mean(poverty, na.rm=TRUE)), 
          by = .(year, gender)]

ru3



ggplot(ru3, aes(y=mean_poverty,
                x=year,
                group=gender)) +
  geom_point(aes(colour=as.factor(gender))) + 
  geom_line(aes(colour=as.factor(gender))) +
  scale_y_continuous(labels=scales::percent) +
  theme_minimal() +
  labs(title="Trends in at-risk of poverty rate for employed men and women",
       subtitle = "\nTrend uses 60% of median income benchmark (1994-2020)",
       x='Year',
       y='At-Risk of Poverty Rate %',
       caption = "Source: RLMS 1991-2020 \nAnalysis: Ivan Privalko")  +
  theme(
    plot.title = element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5))






# compare time points -----------------------------------------------------



percent <- percent %>% 
  pivot_wider(names_from = year, values_from = wages) %>% 
  rename('wages_2020' = `2020`,
         'wages_2012' = `2012`,
         'wages_2007' = `2007`,
         'wages_2000' = `2000`,
         'wages_1995' = `1995`)

percent

percent <- percent %>% 
  mutate(ratio_2020vs2012= income_2020/income_2012,
         ratio_2012vs2007= income_2012/income_2007,
         ratio_2007vs2000= income_2007/income_2000,
         ratio_2000vs1995= income_2000/income_1995) %>%
  select(-contains('income')) %>% 
  pivot_longer(cols = contains('ratio'),
               names_to = 'group',
               values_to = 'ratio') 

percent
save(percent, file = "Sandbox/percent2_w.Rdata")


p2 <-  percent %>% 
  filter(percentile <98 & percentile > 2) %>% 
  ggplot(aes(x=percentile, y=ratio, group=group))+
  geom_point(aes(colour=group))+
  geom_line(aes(colour=group)) +
  scale_x_continuous("percentile") +
  geom_hline(yintercept = 1,
             linetype="dotted") +
  theme_minimal()+
  labs(title="Change in real income for each percentie by time points",
       subtitle = "Adjusting for inflation and accounting for denominational change, \nincome has grown for each percentile. \nWe drop the bottom 2 and top 2 percentiles from our analysis",
       x='Income percentile',
       y='Rubles (2020 Prices CPI adjusted)',
       caption = "Source: RLMS 1991-2020 \nAnalysis: Ivan Privalko")+
   scale_colour_discrete(name = "Time period", labels = c("1995 vs 2000", 
                                                              "2000 vs 2007", 
                                                              "2007 vs 2012",
                                                              "2012 vs 2020")) +
   theme(
    plot.title = element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5))

p2
save(p2, file = "Sandbox/p2_wages")

p4 <- percent %>% 
  filter(percentile <98 & percentile > 2,
         group=='ratio_2020vs2012' | group=='ratio_2012vs2007') %>% 
  ggplot(aes(x=percentile, y=ratio, grou=group))+
  geom_point(aes(colour=group))+
  geom_line(aes(colour=group)) +
  scale_x_continuous("percentile") +
  geom_hline(yintercept = 1,
             linetype="dotted") +
  theme_minimal()+
  labs(title="Change in real income for each percentie by time points",
       subtitle = "Adjusting for inflation and accounting for denominational change, \nincome has grown for each percentile. \nWe drop the bottom 2 and top 2 percentiles from our analysis",
       x='Income percentile',
       y='Rubles (2020 Prices CPI adjusted)',
       caption = "Source: RLMS 1991-2020 \nAnalysis: Ivan Privalko")+
  scale_colour_discrete(name = "Time period", labels = c("2007 vs 2012",
                                                         "2012 vs 2020")) +
  theme(
    plot.title = element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5))


p4
save(p4, file = "Sandbox/p4_wages")

p4

p3 <- percent %>% 
  filter(percentile <98 & percentile > 2,
         group=='ratio_2000vs1995' | group=='ratio_2007vs2000') %>% 
  ggplot(aes(x=percentile, y=ratio, grou=group))+
  geom_point(aes(colour=group))+
  geom_line(aes(colour=group)) +
  scale_x_continuous("percentile") +
  geom_hline(yintercept = 1,
             linetype="dotted")+
  theme_minimal()+
  labs(title="Change in real income for each percentie by time points",
       subtitle = "Adjusting for inflation and accounting for denominational change, \nincome has grown for each percentile. \nWe drop the bottom 2 and top 2 percentiles from our analysis",
       x='Income percentile',
       y='Rubles (2020 Prices CPI adjusted)',
       caption = "Source: RLMS 1991-2020 \nAnalysis: Ivan Privalko")+
  scale_colour_discrete(name = "Time period", labels = c("1995 vs 2000", 
                                                         "2000 vs 2007")) +
  theme(
    plot.title = element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5))



p3
save(p3, file = "Sandbox/p3_wages")





# p90/p10 ratio ----------------------------------------------------------

p90 <- ru[, as.list(quantile(cpi_wage_2020,probs = 0:100/100,na.rm=TRUE)), by=. (year)]
ru
p90[, ]

setnames(p90, '1%', 'p1') 
setnames(p90, '10%', 'p10')
setnames(p90, '50%', 'p50')
setnames(p90, '90%', 'p90') 

p90[,]


p90 <- p90[, .(year, p1, p10, p50, p90)]




p90[, `:=` (p90_p10 = p90/p10, 
            p50_p10 = p50/p10,
            p90_p50 = p90/p50)]
p90[,]


p90 <- p90[, .(year, p90_p10, p50_p10, p90_50)]

p90[]

m1 = melt(p90, id.vars = "year",
             measure.vars = c("p90_p10", 
                              "p50_p10",
                              "p90_p50"))

p5 <- m1[!is.na(value),] %>% 
  ggplot(aes(x=year,
             y=value,
             group=variable))+
  geom_point(aes(col=variable))+
  geom_line(aes(col=variable)) +
  theme_minimal()+
  labs(title="Change in inequality measures since 1994",
       subtitle = "Within years, we see different rates of inequality for each year. \nThe most extreme is that of the P90:P10 ratio",
       x='Year',
       y='Ratio',
       caption = "Source: RLMS 1991-2020 \nAnalysis: Ivan Privalko") +
  scale_colour_discrete(name = "Ratio", labels = c("P90:P10",
                                                   "P50:P10",
                                                   "P90:P50")) +
  theme(
    plot.title = element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5))

save(p5, file = "Sandbox/p5_wages")
p5



# share of top 10% --------------------------------------------------------



 d90_share <- ru[, total := sum(cpi_wage_2020, na.rm = T),
          by=.(year)]

d90_share[,]


d90_share[, dec_total := sum(cpi_wage_2020, na.rm=T),
          by=.(year, wage_decile)]


p6 <- d90_share %>% 
  ggplot(aes(x=year,
             y=top_income_share))+
  geom_point()+
  geom_line()  +
  theme_minimal() +
labs(title="Top Decile's Share of Total Income",
     subtitle = "The top decile's share of total income fell gradually to 27% of total income in 2020",
     x='Year',
     y='Share',
     caption = "Source: RLMS 1991-2020 \nAnalysis: Ivan Privalko") +
  theme(
    plot.title = element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5))+
  scale_y_continuous(labels=scales::label_percent())
  

save(p6, file = "Sandbox/p6")
p6






d90_share <- ru[, total := sum(cpi_wage_2020, na.rm = T),
                by=.(year)]

d90_share[,]


d90_share[, dec_total := sum(cpi_wage_2020, na.rm=T),
          by=.(year, wage_decile)]


d90_share <- d90_share[,       .(total_wage= mean(total, na.rm=T),
                         total_top_wage= mean(dec_total, na.rm=T)), by=.(year, wage_decile)]

d90_share[, ratio := total_top_wage/ total_wage]


d90_share[wage_decile==10,] %>% 
  ggplot(aes(x=year,
             y=ratio))+
  geom_point()+
  geom_line()





# stacked area chart

d90_share

p7 <- d90_share[!is.na(wage_decile),] %>% 
  ggplot(aes(x=year,
             y=ratio,
             fill=wage_decile))+
  geom_area(position = position_stack(reverse = T))+
  theme_minimal()+
  labs(title="Change in income decile's share of total income since 1994",
       subtitle = "Top earners have seen their share if total income decline over time",
       x='Year',
       y='Share of total income',
       caption = "Source: RLMS 1991-2020 \nAnalysis: Ivan Privalko") +
  scale_fill_viridis(discrete = TRUE,
                     guide = guide_legend(reverse = T),
                        name = "Income Decile", labels = c("Bottom Decile",
                                                   "2",
                                                   "3",
                                                   "4",
                                                   "5",
                                                   "6",
                                                   "7",
                                                   "8",
                                                   "9",
                                                   "Top Decile")) +
  scale_y_continuous(labels=scales::label_percent())+
  theme(
    plot.title = element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5))

save(p7, file = "Sandbox/p7_wage")
p7


# standard deviation of income measures --------------------------------------------------------


