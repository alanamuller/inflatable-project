library(tidyverse)
library(ggpubr)
library(rstatix)
library(readxl)
library(magrittr)

# Start fresh by removing everything from the environment
rm(list = ls())

# Set working directory
setwd("E:/Nav Stress Data/Salimetrics reports") # for hard drive

# Enter the data 
samples9Data <- readxl::read_excel("saliva_data_bySubject.xlsx", sheet = "9samples")
samples12Data <- readxl::read_excel("saliva_data_bySubject.xlsx", sheet = "12samples")

samples9Data$cort_nmol_L <- samples9Data$mean_cort*276
samples12Data$cort_nmol_L <- samples12Data$mean_cort*276

small_data <- samples9Data %>% 
  filter(subjNum >= 2 & subjNum <= 3)

data_ctrl <- samples9Data %>%
  filter(condition == "ctrl")

data_cp <- samples9Data %>%
  filter(condition == "cp")

data_fire <- samples9Data %>%
  filter(condition == "fire")

summaryStats <- samples9Data%>%
  group_by(condition, time) %>%
  get_summary_stats(mean_cort, type = "mean_sd")

# Set the order of the x-axis
level_order <- c('pre', 'post1', 'post15', 'post30')

# Plot with all participant separated by condition and time
ggplot(data = samples9Data, aes(x=factor(time, level = level_order), y=cort_nmol_L)) +
  geom_boxplot() +
  geom_jitter() +
  facet_wrap(vars(condition)) +
  labs(x = "Time", y = "Cortisol")

ggplot(data = samples9Data, aes(x=factor(time, level = level_order), y=cort_nmol_L)) +
  geom_point() +
  geom_line(aes(group = subjNum)) +
  facet_grid(vars(condition)) +
  labs(x = "Time", y = "Cortisol")


ggplot(data = small_data, aes(x = factor(time, level = level_order), y = cort_nmol_L, group = condition)) +
  geom_point() +
  geom_line(aes(color = condition)) +
  labs(x = "Time", y = "Cortisol")

# Checking for outliers
samples9Data %>%
  group_by(time) %>%
  identify_outliers(cort_nmol_L)

# Checking normality
samples9Data %>%
  group_by(time) %>%
  shapiro_test(cort_nmol_L)

# quick and dirty anova
res.aov <- anova_test(data = data_fire, dv = cort_nmol_L, wid = subjNum, within = time)
get_anova_table(res.aov)
