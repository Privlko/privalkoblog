library(tidyverse)
library(lubridate)
library(data.table)


 following <- readxl::read_excel(sheet = 'Sheet1',
                   path = 'C:/Users/Ivan/Documents/Projects/Blog/data/Ukraine/rosstat_following.xlsx')

 
following <-  following %>% 
   pivot_longer(!Date, names_to = "following", values_to = "percent")

following$following <- as_factor(following$following)




p1 <- following %>% 
  ggplot()+
  geom_bar(aes(y = percent, 
               x = Date, 
               fill = following), 
           stat="identity") +
  scale_y_continuous(labels = scales::percent)+
  labs(x='Month of 2022',
       y='Percentage of Respondents',
       title='Attention to the War in Ukraine has fallen since March but remains high',
       subtitle = 'Over 50% of respondents are following coverage of the war closely or very closely',
       caption = 'Source: Levada Center \nPlot: Ivan Privalko',
       fill='')+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5))+
  coord_flip()

save(p1, file = "Sandbox/ukraine1")



success <- readxl::read_excel(sheet = 'Sheet2',
                                path = 'C:/Users/Ivan/Documents/Projects/Blog/data/Ukraine/rosstat_following.xlsx')

success <- success %>% 
  pivot_longer(!Date, names_to = "success", values_to = "percent")

success$success <- as_factor(success$success)
view(success)

success <- success %>% 
  mutate(mnth = case_when(Date == ymd('2022-04-01') ~ "April",
                       Date == ymd('2022-05-01') ~ "May"))
         

p2 <- success %>% 
  ggplot()+
  geom_bar(aes(y=percent,
               x=success,
               fill=mnth,
               group=mnth),
           stat='identity',
           position='dodge')+
  scale_y_continuous(labels=scales::percent)+
  theme_minimal()+
  labs(x='Answer',
       y='Percentage of Respondents',
       title='Public perception of success has increased slightly since April and remains high',
       subtitle = 'In May, over 75% of respondents believed the conflict was successful or very successful \nThe number of respondents who believe the operation is unsuccessful has declined',
       caption = 'Source: Levada Center \nPlot: Ivan Privalko',
       fill='Month of 2022')+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5))

save(p2, file = "Sandbox/ukraine2")





support <- readxl::read_excel(sheet = 'Sheet3',
                              path = 'C:/Users/Ivan/Documents/Projects/Blog/data/Ukraine/rosstat_following.xlsx')



support <- support %>% 
  pivot_longer(!Date, names_to = "support", values_to = "percent")

support$support <- as_factor(support$support)
view(support)



support <- support %>% 
  mutate(mnth = as_factor(case_when(Date == ymd('2022-03-01') ~ "March",
                          Date == ymd('2022-04-01') ~ "April",
                          Date == ymd('2022-05-01') ~ "May")))


p3 <- support %>% 
  ggplot()+
  geom_bar(aes(y=percent,
               x=support,
               fill=mnth,
               group=mnth),
           stat='identity',
           position='dodge')+
  scale_y_continuous(labels=scales::percent)+
  theme_minimal()+
  labs(x='Answer',
       y='Percentage of Respondents',
       title='Public support for troops is rising and remains high',
       subtitle = 'In May, over 75% of respondents reported some support for Russian troops \nOpposition has grown only slightly',
       caption = 'Source: Levada Center \nPlot: Ivan Privalko',
       fill='Month of 2022')+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5))



save(p3, file = "Sandbox/ukraine3")








trend <- readxl::read_excel(sheet = 'Sheet5',
                              path = 'C:/Users/Ivan/Documents/Projects/Blog/data/Ukraine/rosstat_following.xlsx')

trend$Date <- as.numeric(trend$Date)
trend$Date <-as.Date(trend$Date, origin = "1899-12-30")

trend

trend <- trend %>% 
  pivot_longer(!Date, names_to = "group", values_to = "percent")

p4 <- trend %>% 
  ggplot(aes(x=Date,
             y=percent))+
  geom_point(aes(colour=group))+
  geom_line(aes(colour=group))+
  theme_minimal() +
  labs(x='',
       y='Percentage of Respondents',
       title='Public opinion of Ukraine is low, but above 2015 levels',
       subtitle = 'The overall trend is lower than usual and unlikely to recover soon \nNegative sentiment is also rising',
       caption = 'Source: Levada Center \nPlot: Ivan Privalko',
       colour='Public opinion')+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust=0.5),
    plot.subtitle = element_text(hjust=0.5))

save(p4, file = "Sandbox/ukraine4")
