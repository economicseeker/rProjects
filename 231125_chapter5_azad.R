#################################################################################
# Name: Azad H. Kılıç                                                           #
# Date Created: Nov 25, 2023                                                    #
# Purpose: 5.12.2.1 Chapter Exercise: Coder Edition                             #
#         Examine the relationships between the ease of voting variable and sex,#
#         political party membership, and employment status.                    #
# Packages Used: tidyverse, haven, descr, fmsb, lsr                             #
# Data Used: Pew Research Center April 19-23 2017 survey                        #
# Last Update: Nov 16, 2019                                                     #
#################################################################################


#################################################################################
# 1) Open the data using the strategy shown in this chapter, 
# and follow the data- cleaning steps in Section 5.4.1.
#import the April 17-23 Pew Research Center data
library(package = "haven")
library(package = "tidyverse")

#create a temporary file for downloading the data
temp <- tempfile()

#download from web address
download.file("https://www.people-press.org/wp-content/uploads/sites/4/2018/11/Apr-19-23-2017-public.zip", temp)

#unzip and read in using haven's read_sav for SPSS type data
vote <- read_sav(unz(temp, "Apr 19-23 2017 public.sav"))
unlink(temp)

# select variables of interest
vote.cleaned <- vote %>%
  select(pew1a, sex, employ, polparty)

# check data
summary(object = vote.cleaned)
unique(vote.cleaned$polparty)

#################################################################################
# 2) Clean the remaining variables in the data set so they have clear variable names, 
# category labels, and missing value coding. For the employ variable, 
# the category labels are 1 = Employed full-time, 2 = Employed part-time, 
# 3 through 8 = Not employed, 9 = Refused.

# select variables of interest and clear variable names, category labels,
# and missing value coding
vote.cleaned <- vote %>%
  select(pew1a, sex, employ, polparty) %>%
  zap_labels() %>%
  mutate(pew1a = recode_factor(.x = pew1a,
                               `1` = 'Register to vote',
                               `2` = 'Make easy to vote',
                               `5` = NA_character_,
                               `9` = NA_character_)) %>%
  rename(ease.vote = pew1a) %>%
  drop_na(ease.vote) %>%
  mutate(sex = recode_factor(.x = sex,
                             `1` = 'Male',
                             `2` = 'Female')) %>%
  mutate(employ = recode_factor(.x = employ,
                                `1` = 'Employed full-time', 
                                `2` = 'Employed part-time', 
                                `3` = 'Not employed',
                                `4` = 'Not employed', 
                                `5` = 'Not employed', 
                                `6` = 'Not employed', 
                                `7` = 'Not employed', 
                                `8` = 'Not employed', 
                                `9` = 'Refused')) %>%
  filter(employ != "Refused") %>%
  mutate(polparty =recode_factor(.x = polparty,
                                 `1` = 'A Republican',
                                 `2` = 'A Democrat',
                                 `3` = 'An Independent',
                                 `8` = 'Don\'t know',
                                 `9` = 'Refused',
                                 `0` = 'Other',)) %>%
  mutate(polparty = recode(polparty,
                         'Don\'t know' = 'Other',
                         'Refused' = 'Other'))

# check data
summary(object = vote.cleaned)

#################################################################################
# 3) Compute the appropriate descriptive statistics to examine 
# the relationships between the ease of voting variable and 
# sex, marital status, employment status, and political party (Achievement 1).
# Note: Employ variable could be changed with others and could see the relationships

# voting ease by employment-eth no spread
vote.cleaned %>%
  drop_na(ease.vote) %>%
  group_by(ease.vote, employ) %>%
  summarize(freq.n = n())

# voting ease by employment-eth with spread
vote.cleaned %>%
  drop_na(ease.vote) %>%
  group_by(ease.vote, employ) %>%
  summarize(freq.n = n()) %>%
  spread(key = employ, value = freq.n)

# voting ease by employment-eth with table
table(vote.cleaned$ease.vote, vote.cleaned$employ)

# table of percents voting ease by employment-eth
prop.table(x = table(Voting.ease = vote.cleaned$ease.vote,
                     Employ.eth = vote.cleaned$employ))

# table of percents voting ease by employment-eth
prop.table(x = table(Voting.ease = vote.cleaned$ease.vote,
                     Employ.eth = vote.cleaned$employ),
           margin = 2)

# open gridExtra to put graphs together
library(package = "gridExtra")

# graph the relationship between registration ease and sex 
ease.graph <- vote.cleaned %>%
  drop_na(ease.vote) %>%
  drop_na(polparty) %>%
  group_by(ease.vote, sex) %>%
  count() %>%
  group_by(sex) %>%
  mutate(perc = 100*n/sum(n)) %>%
  ggplot(aes(x = sex, y = perc, fill = ease.vote)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("gray", "#7463AC"), 
                    name = "Opinion on\nvoter registration") +
  labs(x = "Sex", y = "Percent within group")

# graph the relationship between registration ease and polparty 
pol.graph <- vote.cleaned %>%
  drop_na(ease.vote) %>%
  drop_na(polparty) %>%
  group_by(ease.vote, polparty) %>%
  count() %>%
  group_by(polparty) %>%
  mutate(perc = 100*n/sum(n)) %>%
  ggplot(aes(x = polparty, y = perc, fill = ease.vote)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("gray", "#7463AC"), 
                    name = "Opinion on\nvoter registration") +
  labs(x = "Party group", y = "Percent within group")

# graph the relationship between registration ease and employment 
employ.graph <- vote.cleaned %>%
  drop_na(ease.vote) %>%
  drop_na(employ) %>%
  group_by(ease.vote, employ) %>%
  count() %>%
  group_by(employ) %>%
  mutate(perc = 100*n/sum(n)) %>%
  ggplot(aes(x = employ, y = perc, fill = ease.vote)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("gray", "#7463AC"), 
                    name = "Opinion on\nvoter registration") +
  labs(x = "Employment group", y = "Percent within group")

# print the graph of relationship between registration ease and variables  
grid.arrange(ease.graph, employ.graph, pol.graph, nrow = 2)

# chi square TESTS

#chi-squared statistic for ease of voting and sex
chisq.test(x = vote.cleaned$ease.vote,
           y = vote.cleaned$sex)

# H0: People’s opinions on voter registration are the same across sex groups.
# H1: People’s opinions on voter registration are not the same across sex groups.

# With such a high p-value (>0.05), fail to reject the null hypothesis. 
# In the context of study, this means there is no statistically significant 
# evidence to suggest that people's opinions on voter registration differ across 
# sex groups. In other words, the data does not provide sufficient evidence to 
# support the claim that opinions on voter registration are not the same across 
# different sex groups.
# fail to reject H0.

#chi-squared statistic for ease of voting and sex
chisq.test(x = vote.cleaned$ease.vote,
           y = vote.cleaned$employ)

# H0: People’s opinions on voter registration are the same across employment-status groups.
# H1: People’s opinions on voter registration are not the same across employment-status groups.

# With such a high p-value (>0.05), fail to reject the null hypothesis. 
# In the context of study, this means there is no statistically significant 
# evidence to suggest that people's opinions on voter registration differ across 
# employment groups. In other words, the data does not provide sufficient evidence to 
# support the claim that opinions on voter registration are not the same across 
# different employment groups.
# Fail to reject H0.


#chi-squared statistic for ease of voting and sex
chisq.test(x = vote.cleaned$ease.vote,
           y = vote.cleaned$polparty)

# H0: People’s opinions on voter registration are the same across political party groups.
# H1: People’s opinions on voter registration are not the same across political party groups.

# The test result indicates there is statistically significant evidence to suggest 
# that people's opinions on voter registration do differ across political party groups. 
# The data does not support the claim that opinions on voter registration are 
# the same across different political party groups.
# Reject H0 and accept H1 based on these results.

# Using standardized residuals to identify which groups have lower or higher frequencies than expected
std.test <- chisq.test(x = vote.cleaned$ease.vote,
                       y = vote.cleaned$polparty)
std.test$stdres

# Standardized residuals indicate that there are more Republicans who prefer
# "Register to vote" and more Democrats who prefer "Make easy to vote" than 
# what would be expected if there was no association between political party and 
# voting ease preference. Conversely, there are fewer Republicans who prefer 
# "Make easy to vote" and fewer Democrats who prefer "Register to vote" than 
# expected under the null hypothesis.

# compute Cramér's V for voting ease and political party
cramersV(x = vote.cleaned$ease.vote,
         y = vote.cleaned$polparty)

# The output indicates that Cramér's V for the association between 
# vote.cleaned$ease.vote and vote.cleaned$polparty is 0.349887. 
# This value is a measure of the strength of association between 
# the two categorical variables in your data.



