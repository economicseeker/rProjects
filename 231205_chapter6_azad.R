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

# -------------------------------
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

# -------------------------------
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

# -------------------------------
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

# -------------------------------
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

# -------------------------------
# QUESTION 5
# Conduct the appropriate test to compare mean time using tech across marital
# status groups. If the F-statistic is significant, use a post hoc test to determine which
# means are statistically significantly different from each other. Interpret your results
# (Achievements 2 and 3).

#NHST for ANOVA

# NHST Step 1: Write the null and alternate hypotheses
# H0: The mean time spent on technology use is equal across marital status.
# H1: The mean time spent on technology use is not equal across marital status.

# NHST Step 2: Compute the test statistic
techuse.by.marital <- oneway.test(formula = USETECH ~ MARITAL,
                              data = gss.2018.cleaned,
                              var.equal = TRUE)
techuse.by.marital

# NHST Step 3: Calculate the probability that your test statistic is at least as 
# big as it is if there is no relationship (i.e.,the null is true)

# The p-value is < 0.004273, which is very small. The value of an F-statistic
# being at least this large happens a tiny percentage of the time when the null
# hypothesis is true.

# NHST Steps 4 and 5: Interpret the probability and write a conclusion
# With a p-value < .05, the ANOVA indicates that there is likely a difference
# among the means of time spent using technology based on marital status.

# The mean time spent on technology use was significantly different
# across marital status groups [F(4, 1404) = 3.822; p < .05], indicating that 
# these groups likely came from a population with different mean time spent
# on technology use by educational attainment. The highest mean was
# the percent of time used for technology by those with married status.
# The lowest mean was the percent of time used for technology
# by those with widowed.

# find differences in mean tech use by degree groups
bonf.tech.by.marital <- pairwise.t.test(x = gss.2018.cleaned$USETECH,
                                    g = gss.2018.cleaned$MARITAL,
                                    p.adj = "bonf")
bonf.tech.by.marital

# The highest mean was 59.4% of time used for technology for those with
# married marital status. The lowest mean was 48.9% of the time for those
# who widowed . Mean percentage of time using technology was statistically 
# significantly (p < .05) lower for people who are married (m = 59.4) compared to 
#  divorced and never married where means percentage of time using technology are 
# 51.0 and and 52.0 respectively.

# -------------------------------
# QUESTION 6
# Check assumptions for the ANOVA and conduct an appropriate alternate
# analysis if it does not pass assumptions. Interpret your results 
# (Achievements 5 and 6).

# The assumptions for ANOVA 
# having a continuous outcome and independent groups, 
# independent observations, 
# an outcome that is normally distributed within groups, 
# and equal variance of the outcome within groups.

# (checking normality) 
# graph tech use by marital status 
gss.2018.cleaned %>%
  drop_na(USETECH) %>%
  ggplot(aes(x = USETECH)) +
  geom_density(aes(fill = MARITAL)) +
  facet_wrap(facets = vars(MARITAL), nrow = 2) +
  scale_fill_brewer(palette = "Dark2", guide = FALSE) +
  theme_minimal() +
  labs(x = "Percent of time using tech",
       y = "Probability density")
# Based on the density plots, none of the groups looked normally distributed

# graph tech use by marital status (Figure 7.13)
gss.2018.cleaned %>%
  drop_na(USETECH) %>%
  ggplot(aes(sample = USETECH)) +
  geom_abline(aes(intercept = mean(USETECH), slope = sd(USETECH),
                  linetype = "Normally distributed"),
              color = "gray60", size = 1) +
  stat_qq(aes(color = MARITAL)) +
  scale_color_brewer(palette = "Dark2", guide = FALSE) +
  scale_linetype_manual(values = 1, name = "") +
  labs(x = "Theoretical normal distribution",
       y = "Observed values of percent time using tech") +
  theme_minimal() +
  facet_wrap(facets = vars(MARITAL), nrow = 2)

# statistical test of normality for groups
gss.2018.cleaned %>%
  drop_na(USETECH) %>%
  group_by(MARITAL) %>%
  summarize(shapiro.pval = shapiro.test(x = USETECH)$p.value)
# Based on the p-values, all five of the Shapiro-Wilk tests were statistically
# significant, indicating that the null hypothesis for this test (i.e., the data are
# normally distributed) was rejected in each group.

# equal variances for USETECH by MARITAL STATUS
car::leveneTest(y = USETECH ~ MARITAL, data = gss.2018.cleaned, center = mean)
## Levene’s Test for Homogeneity of Variance (center = mean)
# The p-value for the Levene’s test suggests rejecting the null hypothesis; the
# variances of USETECH are statistically significantly different across groups
# (p < .05). The ANOVA fails the assumption of homogeneity of variances.

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
# BOTH NORMALITY AND EQUAL VARIANCE FOR USETECH BY MARITAL STATUS ARE NOT MET  #
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

# NHST Step 1: Write the null and alternate hypotheses
# H0: The mean rank of technology use is the same across marital status groups.
# H1: The mean rank of technology use is not the same across marital status groups.

# NHST Step 2: Compute the test statistic
# compare usetech by degree
kw.usetech.by.marital <- kruskal.test(formula = USETECH ~ MARITAL,
                                     data = gss.2018.cleaned)
kw.usetech.by.marital

# NHST Step 3: Calculate the probability that your test statistic is at least as 
# big as it is if there is no relationship (i.e., the null is true)
# The p-value is < 0.01187, which, as usual, is very tiny. The value of an 
# H-statistic being this large or larger happens a tiny percentage of the time
# when the null hypothesis is true.

# NHST Steps 4 and 5: Interpret the probability and write a conclusion
# The conclusion is that there is a difference in the mean rank for technology
# use by marital status group [H(4) = 12.881; p < .05]. Like the ANOVA results, the
# K-W test identifies whether there is a difference somewhere among the
# means, but it does not identify which groups are different from one another.
# A post hoc test like Bonferroni or Tukey’s HSD could help. For K-W, the
# Dunn’s post hoc test of multiple comparisons is useful for identifying
# which groups are statistically significantly different from which other
# groups.

# I am not gonna conduct Dun's post hoc test 

# -------------------------------
# QUESTION 7
# Conduct a two-way ANOVA with time using technology by sex and marital
# status. Interpret results and check assumptions (Achievement 7).

# graph usetech by degree and sex (Figure 7.19)
gss.2018.cleaned %>%
  ggplot(aes(y = USETECH, x = MARITAL)) +
  geom_boxplot(aes(fill = SEX), alpha = .4) +
  scale_fill_manual(values = c("gray70", "#7463AC")) +
  theme_minimal() +
  labs(x = "Educational attainment",
       y = "Percent of work time using technology")
