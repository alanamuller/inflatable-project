# Average placement error by trial
# Alana Muller
# amuller@arizona.edu
# 2022-07-12

library(ggplot2)
library(reshape)
library(reshape2)
library(readxl)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(rstatix)
library(mosaic)
library(PerformanceAnalytics)
library(rcompanion)
library(PMCMRplus)
library(officer)
library(tidyr)
library(PairedData)

rm(list = ls())

setwd("E:/Nav_1stYr_project_data")

# Read in data
inputData <- read_excel("E:/Nav_1stYr_project_data/manuscript_data_N30_gazecode_byTrial.xlsx")
inputData <- as.data.frame(inputData)
str(inputData) # check the structure of the data

# Make a copy of inputData that I'll use for analysis
myData <- inputData


myData$subject <- as.factor(myData$subject)
myData$trial <- as.factor(myData$trial)
myData$x_study <- as.numeric(myData$x_study)
myData$y_study <- as.numeric(myData$y_study)
myData$x_replace <- as.numeric(myData$x_replace)
myData$y_replace <- as.numeric(myData$y_replace)
myData$x_error <- as.numeric(myData$x_error)
myData$y_error <- as.numeric(myData$y_error)
myData$placement_error <- as.numeric(myData$placement_error)
myData$x_error_cm <- as.numeric(myData$x_error_cm)
myData$y_error_cm <- as.numeric(myData$y_error_cm)
myData$placement_error_cm <- as.numeric(myData$placement_error_cm)

# make abs value columns for x_error_cm and y_error_cm
myData$abs_x_error_cm <- abs(myData$x_error_cm)
myData$abs_y_error_cm <- abs(myData$y_error_cm)
str(myData)

data_by_trial <- myData %>%
  group_by(subject, trial) %>%
  summarize(
    x_error = mean(x_error, na.rm = TRUE),
    y_error = mean(y_error, na.rm = TRUE),
    placement_error = mean(placement_error, na.rm = TRUE),
    x_error_cm = mean(x_error_cm, na.rm = TRUE),
    y_error_cm = mean(y_error_cm, na.rm = TRUE),
    placement_error_cm = mean(placement_error_cm, na.rm = TRUE),
    Total_duration_of_fixations = mean(Total_duration_of_fixations, na.rm = TRUE),
    Average_duration_of_fixations = mean(Average_duration_of_fixations, na.rm = TRUE),
    Minimum_duration_of_fixations = mean(Minimum_duration_of_fixations, na.rm = TRUE),
    Maximum_duration_of_fixations = mean(Maximum_duration_of_fixations, na.rm = TRUE),
    Number_of_fixations = mean(Number_of_fixations, na.rm = TRUE),
    Time_to_first_fixation = mean(Time_to_first_fixation, na.rm = TRUE),
    Duration_of_first_fixation = mean(Duration_of_first_fixation, na.rm = TRUE),
    Total_duration_of_whole_fixations = mean(Total_duration_of_whole_fixations, na.rm = TRUE),
    Average_duration_of_whole_fixations = mean(Average_duration_of_whole_fixations, na.rm = TRUE),
    Minimum_duration_of_whole_fixations = mean(Minimum_duration_of_whole_fixations, na.rm = TRUE),
    Maximum_duration_of_whole_fixations = mean(Maximum_duration_of_whole_fixations, na.rm = TRUE),
    Number_of_whole_fixations = mean(Number_of_whole_fixations, na.rm = TRUE),
    Time_to_first_whole_fixation = mean(Time_to_first_whole_fixation, na.rm = TRUE),
    Duration_of_first_whole_fixation = mean(Duration_of_first_whole_fixation, na.rm = TRUE),
    Number_of_saccades_in_AOI = mean(Number_of_saccades_in_AOI, na.rm = TRUE),
    Time_to_entry_saccade = mean(Time_to_entry_saccade, na.rm = TRUE),
    Time_to_exit_saccade = mean(Time_to_exit_saccade, na.rm = TRUE),
    Peak_velocity_of_entry_saccade = mean(Peak_velocity_of_entry_saccade, na.rm = TRUE),
    Peak_velocity_of_exit_saccade = mean(Peak_velocity_of_exit_saccade, na.rm = TRUE),
    abs_x_error_cm = mean(abs_x_error_cm, na.rm = TRUE),
    abs_y_error_cm = mean(abs_y_error_cm, na.rm = TRUE)
  )

setwd("E:/Nav_1stYr_project_data/GazeCode data/R_outputs")

sink("manuscript_data_N30_gazecode_byTrial_output.csv")
write.csv(data_by_trial, row.names = FALSE)
sink()


















