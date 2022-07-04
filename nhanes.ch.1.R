########################################################## 
# Project: 1.11.2.1 Chapter Exercises: Coder Edition
# Purpose: Examine marijuana use in the United States 
# Author: Azad
# Edit date: July 2, 2022
# Data: National Health and Nutrition Examination Survey (NHANES) 
# Data: Marijuana Use in US
# DUQ200 SAS LABEL Ever used marijuana or hashish 1 Y 2 N
# RIDAGEYR: Age in years, at the time of the screening interview, is reported for
# survey participants between the ages of 1 and 79 years of age. All responses of 
# participants aged 80 years and older are coded as ‘80.’ The reporting of age in 
# single years for adults 80 years and older was determined to be a disclosure 
# risk. In NHANES 2015-2016, the weighted mean age for participants 80 years and 
# older is 85 years.
# RIAGNDR SAS Label Both males and females 0 YEARS - 150 YEARS 1 M 2 F
##########################################################
# bring in GSS 2016 data from the web and examine it

library(package = "data.table")
nhanes.2013 <- fread(file = "[data folder location]/data/nhanes_2013_ch1.csv")

# delete redundant columns
nhanes.2013$RIAGENDR <- NULL
nhanes.2013$V1 <- NULL


# use tidyverse to clean the data
library(package = "tidyverse")
nhanes.2013.cleaned <- nhanes.2013 %>%
  mutate(DUQ200 = as.character(x = DUQ200)) %>%
  mutate(RIDAGEYR = as.numeric(x = RIDAGEYR)) %>%
  mutate(DUQ200 = recode(.x = DUQ200, "1" = "Yes")) %>%
  mutate(DUQ200 = recode(.x = DUQ200, "2" = "No")) %>%
  mutate(DUQ200 = na_if(x = DUQ200, y = "7")) %>%
  mutate(DUQ200 = na_if(x = DUQ200, y = "9")) %>%
  mutate(DUQ200 = as.factor(x = DUQ200 )) %>%
  mutate(DUQ200 = droplevels(x = DUQ200)) %>%
  mutate(RIDAGEYR.CAT = cut(x = RIDAGEYR,
                            breaks = c(-Inf, 29, 39, 49, Inf),               
                            labels = c("18 - 29", "30 - 39", "40 - 49", "50 - 59" )))
# check the summary
summary(object = nhanes.2013.cleaned)


# create bar chart 
marijuana.use.by.age.groups <- nhanes.2013.cleaned %>%
  drop_na(DUQ200) %>%
  drop_na(RIDAGEYR.CAT) %>%
  group_by(DUQ200, RIDAGEYR.CAT) %>%
  count() %>%
  group_by(RIDAGEYR.CAT) %>%
  mutate(perc.DUQ200 = 100*n/sum(n)) %>%
  ggplot(aes(x = RIDAGEYR.CAT, fill = DUQ200,
             y = perc.DUQ200)) +
  geom_col(position = 'dodge') +
  theme_minimal() +
  scale_fill_manual(values = c("gray40", '#88398a'),              
                    name = "Marijuana use in\nthe United States") +  
  labs(x = "Age group (in years)",
       y = "Percent of responses in age group")

marijuana.use.by.age.groups

