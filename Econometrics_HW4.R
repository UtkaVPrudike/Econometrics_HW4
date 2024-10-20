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

#

data_male <- data %>%
  filter(female = 0)
data_female <- data %>% 
  filter(female = 1)
ggplot(data) +
  geom_point(aes(yrseduc, log(ahe), color=factor(female)), alpha=0.4, position = position_jitter(0.1), shape='o') + 
  geom_abline(intercept = lm_1$coefficients[1], slope = lm_1$coefficients[2], color='red') + 
  geom_abline(intercept = lm_1$coefficients[1] + lm_1$coefficients[3], slope = lm_1$coefficients[2] + lm_1$coefficients[4], color='blue')
