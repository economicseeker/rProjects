#################################################################################
# Name: Azad Hüseyin Kılıç                                                      #
# Date Created: Dec 5, 2023                                                     #
# Purpose: Checking understading of  Chapter 7 of Dr. Harris' Book              #
#          Analysis of Variance Chapter exercises: Coder edition                #
# Packages Used: tidyverse                                                      #
# Data Used: General Social Survey 2018                                         #
# Last Update:  Dec 5, 2023                                                     #
#################################################################################

#install packages
install.packages("tidyverse")

#load packages
library(tidyverse)

# QUESTION 1 
# Open the data using the strategy shown in this chapter.

# load GSS rda file
load(file = "/Users/azadhuseyinkilic/Documents/GitHub/rProjects/gss2018.rda")

# assign GSS to gss.2018
gss.2018 <- GSS

# remove GSS
rm(GSS)

#check the summary 
summary(object = gss.2018)


# QUESTION 2 
# Clean the marital status, sex, and tech use variables so they have clear 
# variable names, category labels, and missing value coding.
gss.2018.cleaned <- gss.2018 %>%
  select(MARITAL, SEX, USETECH) %>%
  mutate(USETECH = na_if(x = USETECH, y = -1)) %>%
  mutate(USETECH = na_if(x = USETECH, y = 999)) %>%
  mutate(USETECH = na_if(x = USETECH, y = 998)) %>%
  mutate(SEX = factor(x = SEX, labels = c("male","female"))) %>%
  mutate(MARITAL = factor(x = MARITAL, labels = c("married",
                                                  "widowed",
                                                  "divorced",
                                                  "separeted",
                                                  "never married",
                                                  "no answer"))) %>%
  filter(MARITAL != "no answer") %>%
  mutate(MARITAL = droplevels(MARITAL))

#check the summary 
summary(object = gss.2018.cleaned)


# QUESTION 3
# Use graphics and descriptive statistics to examine tech use on its own, by sex, 
# and by marital status (Achievement 1).  

# histogram graph of technology use
gss.2018.cleaned %>%
  ggplot(aes(x = USETECH)) +  
  geom_histogram(bins = 10, fill = "#7463AC", color = "gray70") +  
  labs(x = "Percent of work time using technology",     
       y = "Number of participants") +
  theme_minimal() 

# mean and sd of USETECH
use.stats <- gss.2018.cleaned %>%  
  drop_na(USETECH) %>%
  summarize(m.techuse = mean(x = USETECH),       
            sd.techuse = sd(x = USETECH)) 
use.stats

# box-plot graph of technology use by sex 
gss.2018.cleaned %>%
  ggplot(aes(y = USETECH, x = SEX)) +
  geom_jitter(aes(color = SEX), alpha = .4) +
  geom_boxplot(aes(fill = SEX), alpha = .6) +  
  scale_fill_manual(values = c("gray70", "#7463AC"), 
                    guide = FALSE) +  
  scale_color_manual(values = c("gray70", "#7463AC"), guide = FALSE) +  
  theme_minimal() +
  labs(x = "Sex", 
       y = "Percent of work time using technology")

# mean and sd of USETECH by group
use.sex.stats <- gss.2018.cleaned %>%  
  drop_na(USETECH) %>%  
  group_by(SEX) %>%  
  summarize(m.techuse = mean(x = USETECH),       
            sd.techuse = sd(x = USETECH)) 
use.sex.stats

# box-plot graph of technology use by marital status 
gss.2018.cleaned %>%
  drop_na(USETECH) %>%
  ggplot(aes(y = USETECH, x = MARITAL)) +  
  geom_jitter(aes(color = MARITAL), alpha = .6) +  
  geom_boxplot(aes(fill = MARITAL), alpha = .4) +  
  scale_fill_brewer(palette = "Dark2", guide = FALSE) +  
  scale_color_brewer(palette = "Dark2", guide = FALSE) +  
  theme_minimal() +
  labs(x = "Marital status", 
       y = "Percent of time spent using technology")

# mean and sd of USETECH by group
use.marital.stats <- gss.2018.cleaned %>%  
  drop_na(USETECH) %>%  
  group_by(MARITAL) %>%  
  summarize(m.techuse = mean(x = USETECH),       
            sd.techuse = sd(x = USETECH)) 
use.marital.stats

# QUESTION 4
# Based on the graphs and statistics from Question 3, make a prediction about 
# what you would find when you compare tech use across groups by marital status 
# and by sex (Achievement 1).

# Analysis of Technology Use in the Workplace by Sex and Marital Status

# -------------------------------
# Analysis by Sex
# -------------------------------

# Males show lower tech use in the workplace compared to females, 
# as evident from both graphical representation and descriptive statistics.

# Graphical Analysis of Tech Use by Sex:
# - For males, the upper bound of tech use is approximately 90, 
#   median is around 50, and the lower bound is less than 12.5.
# - For females, the upper bound exceeds 90, median is about 70, 
#   and the lower bound is nearly 25.

# Descriptive Statistics for Tech Use by Sex:
# - Mean tech use for males is 50.8 with a standard deviation of 38.3.
# - For females, the mean is 59.2 with a standard deviation of 37.0, 
#   indicating higher average use of technology with similar spread in data.

# -------------------------------
# Analysis by Marital Status
# -------------------------------

# There are variations in technology use in the workplace when comparing 
# different marital status groups. Mean tech use hovers around 50, 
# with standard deviation of approximately 37 for most groups.

# Graphical Analysis of Tech Use by Marital Status:
# - Married and separated individuals have a similar median in tech use.
# - Widowed, divorced, and never married participants show similar pattern 
#   with approximately equal upper bounds, medians, and lower bounds in tech use.

# Descriptive Statistics for Tech Use by Marital Status:
# - Married individuals have a mean tech use of 59.4 and a standard deviation of 36.5.
# - Widowed individuals show lower mean tech use of 48.9 with a standard deviation of 39.5.
# - Similar patterns observed for divorced (mean 51.0, SD 37.6), 
#   separated (mean 56.1, SD 40.5), and never married individuals (mean 52.0, SD 38.9).

# -------------------------------
# Concluding Remarks
# -------------------------------

# The analysis reveals noticeable differences in technology use in the workplace 
# when comparing across sex and marital status.
# - Women, on average, use more technology at work than men.
# - Among marital status groups, married and separated individuals show higher 
#   mean technology use, while widowed, divorced, and never married participants 
#   show lower and more consistent pattern of technology use.

# QUESTION 5
# Conduct the appropriate test to compare mean time using tech across marital
# status groups. If the F-statistic is significant, use a post hoc test to determine which
# means are statistically significantly different from each other. Interpret your results
# (Achievements 2 and 3).



techuse.by.marital <- oneway.test(formula = USETECH ~ MARITAL,
                              data = gss.2018.cleaned,
                              var.equal = TRUE)
techuse.by.marital