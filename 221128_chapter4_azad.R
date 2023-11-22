#################################################################################
# Name: Azad Hüseyin Kılıç                                                      #
# Date Created: Nov 20, 2023                                                    #
# Purpose: Review Chapter 4 with end chapter exercises:Coder edition            #
# Packages Used: tidyverse                                                      #
# Data Used: A prescription drug monitoring program (PDMP) opioid step therapy  #
#            requirement variable. Yes if the state had adopted step therapy    #
#            guidelines as of 2017, and No if the state not adopted.            #
# data:https://edge.sagepub.com/harris1e/student-resources/datasets-and-r-code  #
# Last Update: Nov 22, 2023                                                     #
#################################################################################

# 1. install and load the required "tidyverse" package
# 1.1 install
install.packages("tidyverse")

# 1.2 load
library(tidyverse)

# get the pdmp_2017_kff_ch4.csv data 
# the data file might be get from edge.sagepub.com/harris1e
# "/Datasets/data/pdmp_2017_kff_ch4.csv"
pdmp.2017.kff <- read.csv(file = " ")

# summarize the dataset
summary(object = pdmp.2017.kff)

# change the long name of Opioid.Step.Therapy.Requirements variable 
# to to ostr then recode Yes to 1 and No to 0.  
pdmp.2017.kff.cleaned <- pdmp.2017.kff %>%
  rename(ostr = Opioid.Step.Therapy.Requirements) %>%
  mutate(ostr = ifelse(ostr == "Yes", 1, 0))

# check the variable
summary(object = pdmp.2017.kff.cleaned$ostr)
head(x = pdmp.2017.kff.cleaned$ostr)

### Question 1
# What percentage of states (including Washington, DC) adopted the step therapy 
# guideline as of 2017? 
pdmp.2017.kff.cleaned %>%
  summarize("States adopt the step therapy" = mean(x = ostr))

# What percentage of states did not adopt the step therapy?
pdmp.2017.kff.cleaned %>%
  summarize("States did not adopt the step therapy" = 1 - mean(x = ostr))

### Question 2
# If 10 states were chosen at random, what is the probability that exactly 5 of 
# them would have adopted the step therapy policy? (Achievement 2)
# where successes = 5, n = 10, prob = 0.7647059
dbinom(x = 5, size = 10, prob = 0.76)

### Question 3
# If 15 states were chosen at random, what is the probability that 5 of the 15 
# would have adopted step therapy policy? (Achievement 2)
# where successes = 5, n = 15, prob = 0.7647059
dbinom(x = 5, size = 15, prob = 0.76)

### Question 4
# Take a sample of 30 states, compute the proportion of states in the sample 
# that have step therapy guidelines, and compute the 95% confidence interval 
# for the proportion. Interpret your results. Compare the sample proportion to 
# the population proportion computed in #1. 
# Was the population proportion in  the 95% confidence interval for the sample 
# proportions? (Achievement 6)
sum.pdmp.30 <- pdmp.2017.kff.cleaned %>%
  mutate(as.numeric(x = pdmp.2017.kff.cleaned$ostr)) %>%
  select(ostr)%>%
  sample_n(size = 30, replace = TRUE) %>%
  summarize(mean.s1 = mean(x = ostr),
            sd.s1 = sd(x = ostr),
            se.s1 = sd(x = ostr)/sqrt(x = length(x = ostr)),
            lower.ci.s1 = mean.s1 - 1.96 * se.s1,
            upper.ci.s1 = mean.s1 + 1.96 * se.s1)
sum.pdmp.30

# /Datasets/data/opioid_dist_to_needle_exchange_2018.csv
odtne <- read.csv(file = " ")

#summarize and examine the data set
summary(object = odtne)

# Find the population mean and standard deviation of distance to needle exchange
sum.odtne <- odtne %>%
  summarize(mean.distance = mean(x = VALUE),
            sd.distance = sd(x=VALUE))
sum.odtne

# Print county names
unique(odtne$STATE)

# I have choose one of the data point randomly
# z = (x - m) / s
# z : Z-score
# x : data point
# m : population mean
# s : standard deviation

(89.55 - 105.48) / 87.35
# [1] -0.1823698, so the point selected from dataset is 0.18 standard deviations
# away from mean

# sample 500 counties at random
counties.500 <- odtne %>%
  drop_na(VALUE) %>%
  sample_n(size = 500, replace = TRUE) %>%
  summarise(mean.dist = mean(VALUE),
            sd.dist = sd(VALUE),
            se.dist = sd(VALUE)/sqrt(length(VALUE)),
            lower.ci.s1 = mean.dist - 1.96 * se.dist,
            upper.ci.s1 = mean.dist + 1.96 * se.dist)
counties.500

# get 100 samples
set.seed(seed = 159)
samples.100 <- bind_rows(replicate(n = 100, odtne %>%
                                      drop_na(VALUE) %>%
                                      sample_n(size = 30, replace = TRUE),
                                    simplify = FALSE), .id = "sample_num")

# find the mean for each sample
sample.100.means <- samples.100 %>%
  group_by(sample_num) %>%
  summarize(mean.distance = mean(x = VALUE))

# find the mean of the sample means
sample.100.means %>%
  summarize(mean.100.means = mean(x = mean.distance))

#histogram of the 1000 means
sample.100.means %>%
  ggplot(aes(x = mean.distance)) +
  geom_histogram(bins = 10, fill = "#7463AC", color = "white") +
  labs(x = "Mean distance to facility",
       y = "Number of samples") +
  theme_minimal()
# As the sample size increases, the distribution of the sample means 
# becomes more normal (bell-shaped), 
# regardless of the shape of the population distribution.



