---
title: "Industry Differences in Pay"
author: "Ivan Privalko"
date: '2017-12-30'
slug: gender-parental
tags:
- gender
- pay
categories: gender
---
In this post I outline industry differences in pay, with a special focus on gender. I already listed the occupational differences in a previous post. Here, I follow the same idea, but substitute industry for occupations. 

As before, I use the [Russian Longitudinal Monitoring Survey](http://www.cpc.unc.edu/projects/rlms-hse) for data on participants in 2015. I use a measure for gender, industry, the number of weekly hours worked, and gross wages. 

I do not limit the industry codes, but focus on industry as it appears in the survey. I limit the analysis to women and men who work between thirty and sixty hours per week, although this experience is not representative of all women. 

```{r, include = FALSE}
library(readxl)         # for reading in Excel data
library(dplyr)          # for data manipulation
library(tidyr)          # for data shaping
library(ggplot2)        # for generating the visualizations

gender <- load("C:/Users/Administrator.admin-PC2/Desktop/Russian Inequality/blog/data/adult2015x.RData")

gender <- x %>%
  select(gndr = ixgender,
         industry = ixpriind,
         isco = ixilpjb8,
         hours = ixpwrkwh, 
         pay = ixpjpayt) %>%
  mutate(i10 = isco / 1000)

```



```{r, include = FALSE}

gender$ind[gender$industry == 1] <- "LIGHT INDUSTRY, FOOD"
gender$ind[gender$industry == 2] <- "CIVIL MACHINE CONSTRUCTION"
gender$ind[gender$industry == 3] <- "MILITARY INDUSTRIAL COMPLEX"
gender$ind[gender$industry == 4] <- "OIL AND GAS INDUSTRY"
gender$ind[gender$industry == 5] <- "OTHER HEAVY INDUSTRY"
gender$ind[gender$industry == 6] <- "CONSTRUCTION"
gender$ind[gender$industry == 7] <- "TRANSPORTATION, COMMUNICATION"
gender$ind[gender$industry == 8] <- "AGRICULTURE"
gender$ind[gender$industry == 9] <- "GOVERNMENT AND PUBLIC ADMIN"
gender$ind[gender$industry == 10] <- "EDUCATION"
gender$ind[gender$industry == 11] <- "SCIENCE, CULTURE"
gender$ind[gender$industry == 12] <- "PUBLIC HEALTH"
gender$ind[gender$industry == 13] <- "ARMY, INTERNAL AFFAIRS, SECURITY"
gender$ind[gender$industry == 14] <- "TRADE, CONSUMER SERVICES"
gender$ind[gender$industry == 15] <- "FINANCES"
gender$ind[gender$industry == 16] <- "ENERGY/POWER"
gender$ind[gender$industry == 17] <- "HOUSING/COMMUNAL SERVICES"
gender$ind[gender$industry == 18] <- "REAL ESTATE"
gender$ind[gender$industry > 19] <- "OTHER"


 gender$gndr1[gender$gndr==1 ] <- "Male"
 gender$gndr1[gender$gndr==2 ] <- "Female"
 
industry_income <- gender %>%
   group_by(ind) %>%
   filter(hours > 30 & hours < 60)%>%
   summarise(earnings = mean(pay, na.rm = TRUE)) %>%
   filter(!is.na(earnings)) %>%
   arrange(earnings) %>%
   mutate(industry = factor(ind, levels = .$ind)) 
  

 

```

I arrange the data in order of earnings and focus on gender differences in average pay by industry. The data is organised in the following way. 

```{r}
  gender_ind_earnings <- gender %>%
   group_by(ind, gndr1) %>%
   filter(hours > 30 & hours < 60) %>%
   summarise(earnings = mean(pay, na.rm = TRUE)) %>%
   filter(!is.na(earnings)) %>%
   filter(!is.na(ind)) %>%
   ungroup() %>%
   mutate(ind = factor(ind, levels = industry_income$ind))


```


```{r, include=FALSE}

left_label <- gender_ind_earnings %>%
   group_by(ind) %>%
   arrange(earnings) %>%
   slice(1)
 
 right_label <- gender_ind_earnings %>%
   group_by(ind) %>%
   arrange(earnings) %>%
   slice(2)
 

 
 
 ggplot(gender_ind_earnings, aes(earnings, ind)) +
   geom_line(aes(group = ind)) +
   geom_point(aes(color = gndr1), size = 1.5) +
   geom_text(data = left_label, aes(color = gndr1, label = round(earnings, 0)),
             size = 3, hjust = 1.25) +
   geom_text(data = right_label, aes(color = gndr1, label = round(earnings, 0)),
             size = 3, hjust = -.33) +
   scale_x_continuous(limits = c(-1500, 55000))
 
 big_diff <- gender_ind_earnings %>% 
   spread(gndr1, earnings) %>% 
   group_by(ind) %>% 
   mutate(Max = max(Female, Male),
          Min = min(Female, Male),
          Diff = Max / Min - 1) %>% 
   arrange(Diff) %>%
   filter(Diff > .30)
 

 
 right_label <- filter(right_label, ind %in% big_diff$ind)
 left_label <- filter(left_label, ind %in% big_diff$ind)

  highlight <- filter(gender_ind_earnings, ind %in% big_diff$ind)
  
  
  plot_label <- big_diff %>%
   select(ind, earings = Max, Diff) %>%
   left_join(right_label)
 
p <-  ggplot(gender_ind_earnings, aes(earnings, ind)) +
   geom_line(aes(group = ind), alpha=0.5) +
   geom_point(aes(color = gndr1), size = 1.5) +
   geom_line(data = highlight, aes(group = ind)) +
   geom_point(data = highlight, aes(color = gndr1), size = 2) +
   geom_text(data = plot_label, aes(color = gndr1, 
                                   label = paste0("+", scales::percent(round(Diff, 2)))),
            size = 3, hjust = -.25)

```


The resulting graph looks like the plot in Figure \@ref(fig:dot).

```{r dot, fig.cap='Dot plot showing gender differences in pay', echo=FALSE}
p + scale_color_discrete(labels = c("Female", "Male")) +
  scale_x_continuous(limits = c(-1000, 60000),
                     breaks = seq(0, 50000, by = 15000))+
  scale_y_discrete() +
  labs(title = "Total Earnings by Gender and Industry",
       subtitle = "Out of 18 industries (and a residual cat), eight have noticalble \npremiums for men (30% higher pay than women). Real estate \nand Construction have the largest differences.",
       caption = "Source: RLMS 2015 individual file") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.justification = c(0, 0.5), 
        legend.position = c(.1, 1.075),
        legend.background = element_blank(),
        legend.direction="horizontal",
        plot.title = element_text(size = 10, color = "darkslategrey", margin = margin(b = 5)),
        plot.subtitle = element_text(size = 10, color = "darkgrey", margin = margin(b = 25)),
        plot.caption = element_text(size = 10, margin = margin(t = 10), color = "grey70", hjust = 0)) 


```

A number of interesting differences are worth mentioning. First, women in each industry earn less than men (the only exception is education). This could be the product of differences in working time, women are more likely to work lower hours than men. Importantly, the data above does not consider part-time workers. Only those who work 30+ hours per week are included. However, differences in working time can also be at play, with money working fewer hours on the available scale. 

Second, there are several industries with higher premiums for men than women. Real estate, finances, and science and cutlural industries have the highest premiums. Here, the differences in pay may stem from differences in occupation. Women may rely on lower paid occupations in each industry, while the managerial roles may be more dominated by men. The results for finance, are not surprising, since the industry has a masculine culture, however the resutls for science are surprising. Here, I would expect a culture of meritocracy to keep wages equal between genders, instead men are often paid a 70% higher wage than women. A number of other industries contain smaller bonuses. The military sector typically rewards men more than women. This is interesting since the military typically relies on pay similar to public administration, yet that sector of the economy appears to have a smaller gender gap. Further, the industry "Army and internal affairs", appears to have no pay differences. 

Last, a number of sectors contain no large difference between genders. Public health, education, and public administration. Here the gap in pay between genders appear relatively minor. 

There are a number of other topics to research here. First, there may be gender differences in the numbers of hours worked. Second, it's possible that women are facing increased discrimination in the labour market, where their labour is undervalued. Lastly, it may be that women receive less training and grooming for higher positions than men. I explore these in another post. 
