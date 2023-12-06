#################################################################################
# Name: Azad Hüseyin Kılıç                                                      #
# Date Created: Nov 28, 2023                                                    #
# Purpose: Understanding of conducting and interpreting t-test                  # 
#          Hacker Edition                                                       #
# Packages Used: tidyverse, RNHANES, lsr, car, BSDA, rcompanion                 #
# Data Used: NHANES 2015-2016                                                   #
# Last Update: Nov 28, 2023                                                     #
#################################################################################

# Depending on your score in the knowledge check, choose either the coder or 
# hacker edition of the chapter exercises. Use the NHANES data from this  
# chapter and the appropriate tests to examine diastolic blood pressure for 
# males and females

# Question 1 
# Open the 2015–2016 NHANES data using the strategy shown in this chapter.

# 1.1 install the required packages 
lapply(c("RNHANES", "tidyverse"), install.packages)

# 1.2 open the package
lapply(c("RNHANES", "tidyverse"), require, character.only = TRUE)

# 1.3 load the data set
nhanes.2015 <- nhanes_load_data("BPX", "2015-2016", cache = "./nhanes_data", demographics = TRUE)

# Question 2
# Clean the sex variable and the two diastolic blood pressure measurement 
# variables so they have clear names, category labels, and missing value coding.

# 2.1 check the RIAGENDR (sex) and systolic blood pressure 1 (BPXSY1) and
# systolic blood pressure 1 (BPXSY2) measurement variables
lapply(nhanes.2015[c("RIAGENDR", "BPXSY1", "BPXSY2")], summary)

# 2.2 add labels to sex and rename variables
nhanes.2015.cleaned <- nhanes.2015 %>%
  mutate(RIAGENDR = recode_factor(.x = RIAGENDR,
                                  `1` = 'Male',
                                  `2` = 'Female')) %>%
  rename(sex = RIAGENDR) %>%
  rename(systolic = BPXSY1) %>%
  rename(systolic2 = BPXSY2)

# 2.3 check the variables
lapply(nhanes.2015.cleaned[c("sex", "systolic", "systolic2")], summary)

# Question 3
# Use graphics and descriptive statistics to examine Measure 1 on its own and 
# by participant sex (Achievement 1)

# 3.1 graph systolic bp (mmHg)
bp1.histo <- nhanes.2015.cleaned %>%
ggplot(aes(x = systolic)) +
  geom_histogram(fill = "#7463AC", color = "white") +  
  theme_minimal() +
  labs(x = "Systolic blood pressure (mmHg)",
       y = "NHANES participants")

# print plotting
bp1.histo

# 3.2 graph systolic bp (mmHg) by sex
bp1.histo.by.sex <- nhanes.2015.cleaned %>%
  ggplot(aes(x = systolic)) +
  geom_histogram(fill = "#7463AC", col = "white") +  
  facet_grid(cols = vars(sex)) +
  theme_minimal() +
  labs(x="Systolic blood pressure (mmHg)",     
       y="NHANES participants")

# print plotting by sex
bp1.histo.by.sex

# 3.3 mean and sd of systolic blood pressure
bp1.descriptive <- nhanes.2015.cleaned %>%
  drop_na(systolic) %>% 
  summarize(m.sbp = mean(x = systolic),        
            sd.sbp = sd(x = systolic),        
            n.spb = n())

# print descriptive statistics
bp1.descriptive

# Question 4
# Use graphics and descriptive statistics to examine Measure 2 
# on its own (Achievement 1)

# 4.1 graph systolic bp 2 (mmHg)
bp2.histo <- nhanes.2015.cleaned %>%
  ggplot(aes(x = systolic2)) +
  geom_histogram(fill = "#7463AC", color = "white") +  
  theme_minimal() +
  labs(x = "Systolic blood pressure (mmHg)",
       y = "NHANES participants")

# print plotting
bp2.histo

# 4.2 mean and sd of systolic blood pressure
bp2.descriptive <- nhanes.2015.cleaned %>%
  drop_na(systolic2) %>% 
  summarize(m.sbp = mean(x = systolic2),        
            sd.sbp = sd(x = systolic2),        
            n.spb = n())

# print descriptive statistics
bp2.descriptive

# Question 5
# Based on the graphs and statistics from Questions 3 and 4, make predictions 
# about what you would find when you compare the mean diastolic blood pressure 
# from Measure 1 and Measure 2 (Achievement 1)

# In the both of the graphs, the x axis of this bar graph is labeled systolic 
# blood pressure (mmHg) and the  values on this axis range from 100 to 200, 
# in intervals of 50 mmHg. The y axis is labeled number of NHANES participants, 
# and the values on this axis range from  0 to 1,000, in intervals of 250.

# Based on the graphs, the first Systolic blood pressure test graphs has 2 median and 
# right skewed. On the other hand, the second Systolic blood pressure test on the
# graph shows that also right skewed. Mean dbp from measure one is slightly 
# higher than the second measure but the number of participants are a little bit
# higher in the second measure.

# Question 6
# Based on the graphs and statistics from Questions 3 and 4, make predictions 
# about what you would find when you compare the mean diastolic blood pressure 
# from Measure 1 by sex (Achievement 1).

# compare means of systolic by sex
nhanes.2015.cleaned %>%
  drop_na(systolic) %>%
  group_by(sex) %>%
  summarize(m.sbp = mean(x = systolic),
            var.sbp = var(x = systolic),
            samp.size = n()) %>%
  mutate_if(is.numeric, format, 4)

# When I compare the male and female sbp measures, mean value of male measure is
# slightly higher than female systolic blood pressure.

# Question 7
# Select and use the appropriate t-test to compare Measure 1 for males and 
# females, then interpret your results using the test statistics and p-value 
# along with a graph showing the two groups. Check assumptions for this test. If 
# the assumptions were not met, conduct and interpret the appropriate alternate 
# test (Achievements 3– 5).

# NHST Step 1: Write the null and alternate hypotheses
# H0: There is no difference in mean systolic blood pressure between males and females in the U.S. population.
# H1: There is a difference in mean systolic blood pressure between males and females in the U.S. population.

# NHST Step 2: Compute the test statistic
twosampt <- t.test(formula = nhanes.2015.cleaned$systolic ~ nhanes.2015.cleaned$sex)
twosampt

# NHST Step 3: Calculate the probability that your test statistic is at least 
# as big as it is if there is no relationship (i.e., the null is true)

# NHST Steps 4 and 5: Interpret the probability and write a conclusion
# There was a statistically significant difference [t(7143) = 7.31; p < .05] 
# in mean systolic blood pressure between males (m = 122.18) and females (m = 118.97) in the sample. 
# The sample was taken from the U.S. population, indicating that males in the US 
# likely have a different mean systolic blood pressure than females in the US. 
# The difference between male and female mean systolic blood pressure was 3.21 in the sample; 
# in the population this sample came from, the difference between 
# male and female mean blood pressure was likely to be between 2.35 and 4.07 (d = 3.21; 95% CI: 2.35–4.07).

# density plot of systolic by sex
dens.sex.bp <- nhanes.2015.cleaned %>%  
  ggplot(aes(x = systolic,         
             fill = sex)) +  
  geom_density(alpha = .8) +  
  theme_minimal() +
  labs(x = "Systolic blood pressure", y = "Probability density") +  
  scale_fill_manual(values = c('gray', '#7463AC'),             
                    name = "Sex")
dens.sex.bp

#graph systolic bp (Figure 6.14)
qq.plot <- nhanes.2015.cleaned %>%
  drop_na(systolic) %>%
  ggplot(aes(sample = systolic)) +
  stat_qq(aes(color = "NHANES participant"), alpha = .6) +
  facet_grid(cols = vars(sex)) +
  geom_abline(aes(intercept = mean(x = systolic),
                  slope = sd(x = systolic), linetype = "Normally distributed"),
              color = "gray", size = 1) +
  theme_minimal() +
  labs(x = "Theoretical normal distribution",
       y = "Observed systolic blood pressure (mmHg)")+
  scale_color_manual(values = "#7463AC", name = "") +
  scale_linetype_manual(values = 1, name = "")
qq.plot 
# The shape of distribution, which is s-shape, indicates that the distribution is
# not normal as is seen in the graph. So, we should use alternate test-
# The Man Witnes U Test-for independent samples t-test.

# NHST Step 1: Write the null and alternate hypotheses
# H0: There is no difference in ranked systolic blood pressure values for males and females in the U.S. population.
# H1: There is a difference in ranked systolic blood pressure values for males and females in the U.S. population.

# NHST Step 2: Compute the test statistic
# test the distribution of systolic by sex
u.syst.by.sex <- wilcox.test(formula =                    
                               nhanes.2015.cleaned$systolic ~ nhanes.2015.cleaned$sex,                  
                             paired = FALSE)
u.syst.by.sex

# NHST Step 3: Calculate the probability that your test statistic is at least as
# big as it is if there is no relationship (i.e., the null is true)
# The p-value is shown in scientific notation in the output as < 2.2e-16, which is well below .05.

# NHST Steps 4 and 5: Interpret the probability and write a conclusion
# A Mann-Whitney U test comparing systolic blood pressure for males and females 
# in the US found a statistically significant difference between the two groups (p < .05).

# Question 8
# Select and use the appropriate t-test to compare the means of Measure 1 and Measure 2, 
# then interpret your results using the test statistics and p-value. Check 
# assumptions for this test. If the assumptions were not met, conduct and interpret 
# the appropriate alternate test (Achievements 4, 6).

# NHST Step 1: Write the null and alternate hypotheses
# H0: There is no difference between Measures 1 and 2 for systolic blood pressure.
# HA: There is a difference between Measures 1 and 2 for systolic blood pressure.

# dependent-samples t-test for systolic measures 1 and 2
t.test(x = nhanes.2015.cleaned$systolic,
       y = nhanes.2015.cleaned$systolic2,
       paired = TRUE)

# NHST Step 3: Calculate the probability that your test statistic is at least as 
# big as it is if there is no relationship (i.e., the null is true)
# The p-value was shown in scientific notation as < 2.2e-16 which is well below .05. 

# NHST Steps 4 and 5: Interpret the probability and write a conclusion
# The mean difference between two measures of systolic blood pressure was statistically 
# significantly different from zero [t(7100) = 9.38; p < .05]. The positive 
# difference of 0.54 indicated that systolic blood pressure was significantly 
# higher for the first measure compared to the second measure. While the mean 
# difference in the sample was .54, the mean difference between the first and 
# second measures in the population was likely between 0.43 and 0.66 
# (md = 0.54; 95% CI: 0.43–0.66).
                                                

# I am tired for anternate tests, but there is brief information related tests:

# In summary, each type of t-test has specific assumptions:

# One-Sample T-Test: Assumes normal distribution, independent observations, 
# random sampling, and continuous data.

# Dependent (Paired) T-Test: Requires the differences between pairs to be 
# normally distributed, the pairs to be meaningfully connected, absence of 
# outliers in differences, and each pair to be independent and randomly sampled.

# Independent T-Test: Needs each group's data to be normally distributed, equal 
# variances across groups (homogeneity of variances), independent groups, random 
# sampling, and absence of outliers.

# Violations of these assumptions can affect the validity of the test results, 
# and checking these assumptions (through plots, tests like Shapiro-Wilk for normality, 
# and Levene's test for equal variances) is crucial. If assumptions are not met, 
# alternative methods like non-parametric tests or data transformations may be needed.

