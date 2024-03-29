
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


setDT(ru)

setorder(ru, year,idind) 

setnames(ru, 'ID_H', 'hhid') 
setnames(ru, 'idind', 'pid') 
setnames(ru, 'I1', 'born') 
setnames(ru, 'I2', 'cob')
setnames(ru, 'J1', 'employed')
setnames(ru, 'J10', 'wages')
setnames(ru, 'J60', 'income')


ru <- na.omit(ru, cols = c('pid', 'income', 'wages'))
ru <- ru[(income > 0 & wages > 0 & income < 99999995 & wages < 99999995), ]




ru[, income_denom := ifelse(year < 1998, income/1000, income)]

ru[, wage_denom := ifelse(year < 1998, wages/1000, wages)]



ru[, income_quintile := cut(income_denom, 
                            breaks =quantile(income_denom, probs = 0:5/5, na.rm=TRUE),
                            labels = 1:5, right = FALSE), by=year]

ru[, wage_quintile := cut(wage_denom, 
                          breaks =quantile(wage_denom, probs = 0:5/5, na.rm=TRUE),
                          labels = 1:5, right = FALSE), by=year]

ru2 = ru[!is.na(income_quintile), .(mean_income=mean(income_denom, na.rm=TRUE),
                                    mean_wages=mean(wage_denom,na.rm=TRUE),
                                    median_income=median(income_denom,na.rm=TRUE),
                                    median_wages=median(wage_denom, na.rm=TRUE)), by = .(year)]



#  add cpi  ---------------------------------------------------------------


cpi = fread('C:/Users/Ivan/Documents/Projects/Stata2R/cpi ru.csv')
cpi
setDT(cpi)


cpi <- cpi[!is.na(CPI_2010)]



cpi[1:200,]

setnames(cpi, 'Year', 'year')
cpi <- cpi[!is.na(CPI_2010)]


cpi[, cpi_2010 := CPI_2010/CPI_2010[year==2010]]
cpi[, cpi_2020 := CPI_2010/CPI_2010[year==2020]]

ru <- ru[cpi, on = .(year = year)]


ru
ru[, cpi_value_2010 := income_denom / (cpi_2010)]
ru[, cpi_value_2020 := income_denom / cpi_2020]




# group differences in quintiles over time --------------------------------


setorder(ru, year, income_quintile)



ru2 <- ru[!is.na(income_quintile), 
   .(mean_income=mean(cpi_value_2020, na.rm=TRUE)), 
   by = .(year, income_quintile)]

#save(ru2, file = "Sandbox/ru2.Rdata")


ru2

ggplot(ru2, aes(y=mean_income,
                x=year,
                group=income_quintile))+
  geom_point(aes(colour=income_quintile)) + 
  geom_line(aes(colour=income_quintile)) +
  scale_y_log10(labels=scales::comma) +
  theme_minimal()+
  labs(title="Trends in real income using 2020 prices by quintile (1994-2020)",
       subtitle = "Adjusting for inflation and accounting for denominational change, \nincome gaps have remained persistent over time",
       x='Year',
       y='Rubles (2020 Prices CPI adjusted)',
       caption = "Source: RLMS 1991-2020 \nAnalysis: Ivan Privalko") +
  scale_colour_discrete(name = "Income quintile", labels = c("Lowest", 
                                                            "Second", 
                                                            "Third",
                                                            "Fourth",
                                                            "Highest")) +
  theme(
    plot.title = element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5))





percent <-  ru[year==2020 | year==2000 | year==2007 | year==2012 | year==1995, 
               as.list(quantile(cpi_value_2020,probs = 0:100/100,na.rm=TRUE)), by=year]
view(percent)

percent <- pivot_longer(percent, cols = -year, names_to = 'percentile', values_to = 'income')
percent
percent$percentile <-  as.numeric(gsub("[\\%,]", "", percent$percentile))
percent


percent %>% 
  filter(year==2020,
         percentile <98 & percentile > 2) %>% 
  arrange(desc(income))

percent %>% 
  filter(year==2020,
         percentile <98 & percentile > 2)

percent %>% 
  filter(year==2012,
         percentile <98 & percentile > 2) %>% 
  arrange(desc(income))

percent %>% 
  filter(year==2012,
         percentile <98 & percentile > 2)



percent %>% 
  filter(year==2020,
         percentile <98 & percentile > 2) %>% 
  ggplot(aes(x=percentile, 
             y= income,
             group=percentile))+
  geom_col()+
  scale_y_continuous(labels=scales::comma) +
  theme_minimal()+
  labs(title="Average earnings by percentile for 2020",
       subtitle = "Adjusting for inflation and accounting for denominational change. \nWe drop the bottom 2 and top 2 percentiles from our analysis",
       x='Income percentile',
       y='Rubles (2020 Prices CPI adjusted)',
       caption = "Source: RLMS 1991-2020 \nAnalysis: Ivan Privalko")+
  theme(
    plot.title = element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5))


percent %>% 
  filter(year==2020 | year == 2012,
         percentile <98 & percentile > 2)

percent %>% 
  filter(year==2020 | year == 2012,
         percentile <98 & percentile > 2) %>% 
  summarise(mea = mean(income, na.rm=T), by=year)






p1 <- percent %>% 
  filter(year==2020 | year == 2012,
         percentile <98 & percentile > 2) %>% 
  ggplot(aes(x=percentile, 
             y= income,
             group=percentile))+
  geom_col()+
  facet_wrap(.~year) +
  scale_y_continuous(labels=scales::comma) +
  theme_minimal()+
  labs(title="Average earnings by percentile for 2012 and 2020",
       subtitle = "Adjusting for inflation and accounting for denominational change, \nincome has grown for each percentile. We drop the bottom 2 and top 2 percentiles from our analysis",
       x='Income percentile',
       y='Rubles (2020 Prices CPI adjusted)',
       caption = "Source: RLMS 1991-2020 \nAnalysis: Ivan Privalko")+
  theme(
    plot.title = element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5))


save(p1, file = "Sandbox/p1")

p1


# compare time points -----------------------------------------------------



percent <- percent %>% 
  pivot_wider(names_from = year, values_from = income) %>% 
  rename('income_2020' = `2020`,
         'income_2012' = `2012`,
         'income_2007' = `2007`,
         'income_2000' = `2000`,
         'income_1995' = `1995`)

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
save(percent, file = "Sandbox/percent2.Rdata")


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
       subtitle = "Adjusting for inflation and accounting for denominational change, \nincome has grown for each percentile in two specific periods. \nWe drop the bottom 2 and top 2 percentiles from our analysis",
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

save(p2, file = "Sandbox/p2")

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



save(p4, file = "Sandbox/p4")

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
save(p3, file = "Sandbox/p3")





# p90/p10 ratio ----------------------------------------------------------

p90 <- ru[, as.list(quantile(cpi_value_2020,probs = 0:100/100,na.rm=TRUE)), by=. (year)]

p90[, ]

setnames(p90, '1%', 'p1') 
setnames(p90, '10%', 'p10')
setnames(p90, '50%', 'p50')
setnames(p90, '90%', 'p90') 

p90[,]


p90 <- p90[, .(year, p1, p10, p50, p90)]


p90

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
m1


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

save(p5, file = "Sandbox/p5")
p5



# share of top 10% --------------------------------------------------------



d90_share <- ru[, total := sum(cpi_value_2020, na.rm = T),
          by=.(year)]

d90_share[,]


d90_share[, dec_total := sum(cpi_value_2020, na.rm=T),
          by=.(year, income_quintile)]


d90_share <- d90_share[income_quintile==10, 
          .(total_income= mean(total, na.rm=T),
            total_top_income= mean(dec_total, na.rm=T)), by=.(year)]

d90_share[, top_income_share := total_top_income/total_income]



d90_share

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






d90_share <- ru[, total := sum(cpi_value_2020, na.rm = T),
                by=.(year)]

d90_share[,]


d90_share[, dec_total := sum(cpi_value_2020, na.rm=T),
          by=.(year, income_quintile)]


d90_share <- d90_share[,       .(total_income= mean(total, na.rm=T),
                         total_top_income= mean(dec_total, na.rm=T)), by=.(year, income_quintile)]



d90_share[, ratio := total_top_income/ total_income]
d90_share[income_quintile==10,] %>% 
  ggplot(aes(x=year,
             y=ratio))+
  geom_point()+
  geom_line()





# stacked area chart

d90_share
d90_share[, p10 := cut(income, 
                            breaks =quantile(income, 
                                             probs = 0:10/10, 
                                             na.rm=TRUE),
                            labels = 1:12, 
                       right = FALSE), by=year]






p7 <- d90_share[!is.na(income_quintile),] %>% 
  ggplot(aes(x=year,
             y=ratio,
             fill=income_quintile))+
  geom_area(position = position_stack(reverse = T)) +
  theme_minimal()+
  labs(title="Change in income quintile's share of total income since 1994",
       subtitle = "Top earners have seen their share of total income decline over time",
       x='Year',
       y='Share of total income',
       caption = "Source: RLMS 1991-2020 \nAnalysis: Ivan Privalko") +
  scale_fill_viridis(discrete = TRUE,
                     guide = guide_legend(reverse = T),
                        name = "Income Quintile", labels = c("Bottom Quintile",
                                                   "2",
                                                   "3",
                                                   "4",
                                                   "Top Quintile")) +
  scale_y_continuous(labels=scales::label_percent())+
  theme(
    plot.title = element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5))

save(p7, file = "Sandbox/p7")
p7


# standard deviation of income measures --------------------------------------------------------



sd_measure <- ru[, .(sd= sd(cpi_value_2020, na.rm=T)), by=.(year)]



sd_measure[,] %>% 
  ggplot(aes(x=year,
             y=sd))+
  geom_line()

