library(tidyverse)
library(ggplot2)
library(lmtest)
library(sandwich)
library(Metrics)
library(car)
data <- read_csv("cps99_ps1.csv")


# Task 5

data <- data %>%
  mutate(female_x_yrseduc = female * yrseduc)

lm_1 <- lm(log(ahe) ~ yrseduc + female + female_x_yrseduc, data)
summary(lm_1)
# The coefficient on female shows that without education women's income is 41.4 percentage points less than men's, other regressors held constant.
# The coefficient on female_x_yrseduc shows that every additional year of education gives women a bigger increase in earnings by 1.4 percentage points, other regressors held constant

data %>%
  mutate(female = case_when(female == 0 ~ 'male', female == 1 ~ 'female')) %>%
  ggplot() +
  geom_point(aes(yrseduc, log(ahe), color=factor(female)), alpha=0.4, position = position_jitter(0.25)) + 
  geom_abline(intercept = lm_1$coefficients[1], slope = lm_1$coefficients[2], color='#3399AA', linewidth=1) + 
  geom_abline(intercept = lm_1$coefficients[1] + lm_1$coefficients[3], slope = lm_1$coefficients[2] + lm_1$coefficients[4], color='#AA5544', linewidth=1) +
  labs(colour = 'gender')

coeftest(lm_1, vcov.=vcovHC(lm_1, "HC1"))
# The t value for null hypothesis "value of an additional year of school is the same for men and women" is 2.41, meaning we reject null hypothesis at 5% significance

myH0 <- c("female", "female_x_yrseduc")
linearHypothesis(lm_1, myH0, vcovHC(lm_1, "HC1"))
# the p value for null hypothesis "regresson line is the same for men and women" is lower than 0.05, meaning we reject null hypothesis at the 5% significance

# An additional year of school provides men 8.3% more income and women 9.7% more income on average, i.e. a year of education is about 17% more valuable for women.
# Moreover, because most post school education (bachelors, masters and PHD programs) lasts several years, 
# this difference accumulates and becomes a significant difference in a real-world sense



# Task 6

data <- data %>%
  mutate(male = abs(1-female),
         male_x_yrseduc = male*yrseduc)
lm_2 <- lm(log(ahe) ~ yrseduc + female + female_x_yrseduc + male_x_yrseduc, data)
summary(lm_2)
# The coefficient on male_yrseduc is NA. This is because we created a kind of dummy variable trap. 
# yrseduc, female_x_yrseduc and male_x_yrseduc have prefect multicollinearity, because 
# male_x_yrseduc = (1-female) * yrseduc = yrseduc - female_x_yrseduc
# R notices this mistake and discards the extra variable, so we get NA



# Task 7

data <- data %>%
  mutate(age2 = age^2,
         age3 = age^3,
         female_x_age = female * age,
         female_x_age2 = female * age2,
         female_x_age3 = female * age3)
lm_3 <- lm(log(ahe) ~ yrseduc + female + female_x_yrseduc + age + age2 + age3, data)
lm_4 <- lm(log(ahe) ~ yrseduc + female + female_x_yrseduc + age + age2 + age3 + female_x_age + female_x_age2 + female_x_age3, data)
summary(lm_3)
summary(lm_4)
# In specification (1) an additional year of education increases income by 8.05% for men
# In specification (2) an additional year of education increases income by 7.94% for men
# The difference is unnoticeable in the real-world sense

# Create a function that estimates the gender gap in earnings for given age and years of education
gender_gap_est <- function(age, yrseduc, lm) {
  return(as.double(lm$coefficients %*% c(1, yrseduc, 0, 0, age, age^2, age^3, 0, 0, 0) - lm$coefficients %*% c(1, yrseduc, 1, yrseduc, age, age^2, age^3, age, age^2, age^3)))
}
# The gender gap for 25 year old workers with 16 years of education
gender_gap_est(25, 16, lm_4)

# The gender gap for 55 year old workers with 16 years of education
gender_gap_est(55, 16, lm_4)

# Test the hypothesis that the gender gap in earnings does not depend on age
myH0 <- c("female_x_age", "female_x_age2", "female_x_age3")
linearHypothesis(lm_4, myH0, vcovHC(lm_4, "HC1"))
# get p value lower than 0.05, meaning we reject null hypothesis at the 5% significance level

# Based on the hypothesis testing and the comparison of the gender gap for 25 and 55 year-old workers (8 versus 23 percentage points, respectively)
# it does appear that the gender gap is smaller for younger workers. 
# This makes sense because in recent years there have been a lot of policies aiming at reducing the gender gap that would have affected younger workers more