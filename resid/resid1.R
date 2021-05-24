# Check if the packages that we need are installed
want <-  c("foreign", "ggplot2", "dplyr", "haven", 
           "survey", "srvyr", "essurvey", "labelled")
have <-  want %in% rownames(installed.packages())


# Install the packages that we miss
if ( any(!have) ) { install.packages( want[!have] ) }
# Load the packages
junk <- lapply(want, library, character.only = T)
# Remove the objects we created
rm(have, want, junk)

library(rcompanion)


set_email("privalki@tcd.ie") # set your registered email


ess1 <- import_rounds(rounds = c(8),
                      format = 'spss')


labelled::look_for(ess1, 'lr')

attributes(ess1$dfincac)
summary(ess1$dweight) 
attributes(ess1$hinctnta)
summary(ess1$dweight) 


# check weights -----------------------------------------------------------

summary(ess1$pweight)
summary(ess1$dweight)
summary(ess1$pspwght)

ess1 %>%
  group_by(cntry) %>%
  summarize(min_dw = min(dweight),
            max_dw = max(dweight))

ess1 %>%
  group_by(cntry) %>%
  summarize(n = n(),
            mean = mean(pweight))

##consider survey weights

ess1 %>%
  as_survey(weights = c(pweight)) %>%
  group_by(cntry) %>%
  summarize(n = survey_total())


##here n's are the multiplication of survey n by pweight


#what difference do surveys make

ess1 %>%
  group_by(cntry) %>%
  summarize(age_mean = mean(agea, na.rm=TRUE),
            age_sd = sd(agea, na.rm=TRUE))

ess1 %>%
  as_survey(weights = c(pweight)) %>%
  group_by(cntry) %>%
  summarize(age_mean = survey_mean(agea, na.rm = T))


##no difference in age but consider lrscale

ess1 %>%
  summarize(lr_mean = mean(lrscale, na.rm=TRUE),
            lr_sd = sd(lrscale, na.rm=TRUE))

ess1 %>%
  as_survey(weights = c(pweight)) %>%
  summarize(lr_mean = survey_mean(lrscale, na.rm = T))

ess1 %>%
  as_survey(weights = c(dweight, pweight)) %>%
  summarize(lr_m = survey_mean(lrscale, na.rm = T))




# gender differences in lr scale using weights ----------------------------

# Create summary data
lr_gen <- ess1 %>%
  mutate(sex = ifelse(gndr == 1, "Male", "Female")) %>%
  filter( !is.na(sex)) %>% 
  group_by(cntry, sex) %>%
  summarize(n = mean(lrscale, na.rm = T))

# Plot it
ggplot(lr_gen, aes(x = sex, y = n)) +
  geom_bar(aes(fill = sex), stat = "identity") +
  facet_wrap(~cntry) +
  theme_bw()

# Create summary data
lr_gen_w1 <- ess1 %>%
  mutate(sex = ifelse(gndr == 1, "Male", "Female")) %>%
  filter(!is.na(sex)) %>% 
  as_survey_design(weights = c(dweight)) %>%
  group_by(cntry, sex) %>%
  summarize(n = survey_mean(lrscale, na.rm = T, vartype = "ci"))

# Plot it
ggplot(lr_gen_w1, aes(x = sex, y = n)) +
  geom_bar(aes(fill = sex), stat = "identity") +
  geom_errorbar(aes(ymin = n_low, max = n_upp), width = 0.2) +
  facet_wrap(~cntry) +
  theme_bw()


##adding both design weights and stratification weights

# Create summary data
lr_gen_w2 <- ess1 %>%
  mutate(sex = ifelse(gndr == 1, "Male", "Female")) %>%
  as_survey_design(weights = c(dweight, pspwght)) %>%
  group_by(cntry, sex) %>%
  summarize(n = survey_mean(lrscale, na.rm = T, vartype = "ci"))

# Plot it
ggplot(lr_gen_w2, aes(x = sex, y = n)) +
  geom_bar(aes(fill = sex), stat = "identity") +
  geom_errorbar(aes(ymin = n_low, max = n_upp), width = 0.2) +
  facet_wrap(~cntry) +
  ggthemes::theme_fivethirtyeight()



ess1 <- ess1 %>% 
  mutate(sex = ifelse(gndr == 1, "Male", "Female"))


m_1 <- glm(lrscale~imsmetn + imdfetn + impcntr 
           + imbgeco + imueclt + imwbcnt +factor(sex), 
           data = ess1)



##now try weighted data
ess1_we <- ess1 %>%
  as_survey(weights = c(pweight))

m_2 <- svyglm(lrscale~imsmetn + imdfetn + impcntr 
              + imbgeco + imueclt + imwbcnt +factor(sex), 
              design = ess1_we)
summary(m_2)

m_3 <- estimatr::lm_robust(formula = lrscale~imsmetn + imdfetn + impcntr 
                                  + imbgeco + imueclt + imwbcnt +factor(sex),
                        data = ess1,
                        clusters = cntry,
                        se_type = "stata") 

m_3
summary(m_3)

texreg::screenreg(list(m_1, m_2),
                  stars = numeric(0),
                  single.row = TRUE,
                  digits = 3,
                  custom.model.names = c("GLM", "GLM Weighted"))




broom::glance(m_1) %>% 
  select(AIC, BIC)

broom::glance(m_2) %>% 
  select(AIC, BIC)

broom::tidy(m_1) 
broom::glance(m_1)
broom::augment(m_1)

broom::tidy(m_2)
broom::glance(m_2)

anova(m_1, m_2)



stargazer::stargazer(list(m_1, m_2),
                     type = "text",
                     single.row = TRUE)

AIC(m_1)
AIC(m_2)

summary(m_1)


m_1$AIC <- broom::glance(m_1) %>% 
  select(AIC)


m_1$AIC

m_2$AIC <- broom::glance(m_2) %>% 
  select(AIC)

m_2$AIC

m_1$AIC

library(modelr)
data.frame(
  R2 = rsquare(m_1, data = ess1),
  RMSE = rmse(m_1, data = ess1),
  MAE = mae(m_1, data = ess1)
)


library(caret)
predictions <- m_1 %>% predict(ess1)

predictions

data.frame(
  R2 = R2(predictions, ess1$lrscale, na.rm = TRUE),
  RMSE = RMSE(predictions, ess1$lrscale, na.rm = TRUE),
  MAE = MAE(predictions, ess1$lrscale, na.rm = TRUE)
)




ru8 %>% 
  count(lrscale)



m_2 <- svyglm(lrscale~imsmetn + imdfetn + impcntr + imbgeco + imueclt + imwbcnt, design = data_we)