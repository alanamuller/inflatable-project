# start fresh so we don't get weird errors
rm(list = ls())

# Set working directory
# work computer uses E but laptop uses D, change accordingly
setwd("D:/Nav_1stYr_project_data/GazeCode data")

library(readxl)
library(dplyr)
library(tidyverse)

# read in data
oaya_fixation_data <- read_excel("total_OA_YA_fixations_study.xlsx", sheet = 1)
oaya_timing_data <- read_excel("total_OA_YA_fixations_study.xlsx", sheet = 2)

# check the structure of the data and make factors
oaya_fixation_data$group <- as.factor(oaya_fixation_data$group)
oaya_fixation_data$subjectID <- as.factor(oaya_fixation_data$subjectID)
oaya_fixation_data$group_subj <- as.factor(oaya_fixation_data$group_subj)
oaya_fixation_data$trial <- as.factor(oaya_fixation_data$trial)
oaya_fixation_data$label <- as.factor(oaya_fixation_data$label)

# Table of summary values

fixation_table <- oaya_fixation_data %>%
  group_by(group, group_subj, trial) %>%
  summarize(
    count = n(),
    total_fix_num_per_trial = sum(fix_num), 
    total_fix_dur_per_trial = sum(total_fix_dur_ms), 
    avg_fix_dur_per_trial = sum(total_fix_dur_ms)/sum(fix_num)
  )

# merge fixation table with timings of encoding trials
fixation_table <- merge(fixation_table,oaya_timing_data, by = c("group_subj", "trial"))

# create columns to normalize everything by dividing by encoding study time
fixation_table$total_fix_num_per_trial_norm <- fixation_table$total_fix_num_per_trial/fixation_table$total_study_time_s
fixation_table$total_fix_dur_per_trial_norm <- fixation_table$total_fix_dur_per_trial/fixation_table$total_study_time_s
fixation_table$avg_fix_dur_per_trial_norm <- fixation_table$avg_fix_dur_per_trial/fixation_table$total_study_time_s

#write.csv(fixation_table, "D:/Nav_1stYr_project_data/GazeCode data/oaya_fixation_table.csv", row.names = FALSE)

##### Get rid of the Inf row
fixation_table <- fixation_table %>%
  filter_all(all_vars(!is.infinite(.)))

##### Check for outliers

# Check for number fixation outliers in OAs and YAs
oa_outliers <- fixation_table %>% 
  filter(group == "OA") %>%
  identify_outliers(total_fix_num_per_trial)
  
ya_outliers <- fixation_table %>% 
  filter(group == "YA") %>%
  identify_outliers(total_fix_num_per_trial)
  
# make groups of oa and ya to check means quicker
oa_group <- fixation_table %>%
  filter(group == "OA")
ya_group <- fixation_table %>%
  filter(group == "YA")
  
##### Remove outliers

# Remove the outliers from the original table
oa_cleaned_table <- fixation_table %>%
  filter(!(group == "OA" & total_fix_num_per_trial %in% oa_outliers$total_fix_num_per_trial))

num_fix_cleaned_table <- oa_cleaned_table %>%
  filter(!(group == "YA" & total_fix_num_per_trial %in% ya_outliers$total_fix_num_per_trial))

# Make summary table for RA
summary_table <- num_fix_cleaned_table %>%
  group_by(group, group_subj) %>%
  summarize(
    trial_count = n(), 
    avg_total_fix_num_per_trial = mean(total_fix_num_per_trial, na.rm = TRUE), 
    avg_total_fix_dur_per_trial = mean(total_fix_dur_per_trial, na.rm = TRUE), 
    avg_avg_fix_dur_per_trial = mean(avg_fix_dur_per_trial, na.rm = TRUE),
    avg_total_study_time_s = mean(total_study_time_s, na.rm = TRUE),
    avg_total_fix_num_per_trial_norm = mean(total_fix_num_per_trial_norm, na.rm = TRUE), 
    avg_total_fix_dur_per_trial_norm = mean(total_fix_dur_per_trial_norm, na.rm = TRUE), 
    avg_avg_fix_dur_per_trial_norm = mean(avg_fix_dur_per_trial_norm, na.rm = TRUE)
  )

#write.csv(summary_table, "D:/Nav_1stYr_project_data/GazeCode data/oaya_fixation_summary_table.csv", row.names = FALSE)

##### t tests

plot(avg_total_fix_num_per_trial ~ group, data = summary_table)

summary_stats <- summary_table %>%
  group_by(group) %>%
  get_summary_stats()

t.test(avg_total_fix_num_per_trial ~ group, data = summary_table) # not sig
t.test(avg_avg_fix_dur_per_trial ~ group, data = summary_table) # not sig
t.test(avg_avg_fix_dur_per_trial_norm ~ group, data = summary_table) # not sig
