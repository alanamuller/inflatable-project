# Data file used for the manuscript results section
# Alana Muller
# amuller@arizona.edu
# 2022-07-11

# start fresh so we don't get weird errors
rm(list = ls())

# import library stuff
library(readxl)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(rstatix)
library(car)

# work computer uses E but laptop uses D, change accordingly
setwd("D:/Nav_1stYr_project_data/GazeCode data")

# Read in data
inputData <- read_excel("D:/Nav_1stYr_project_data/GazeCode data/R_outputs/manuscript_data_N29_gazecode_byTrial_output_paste.xlsx")
inputData <- as.data.frame(inputData)
str(inputData) # check the structure of the data

# make a copy so there is also a clean copy (inputData)
myData <- inputData

# make all the rows their correct data type (numeric or factor)
i <- c(1:59)
myData [ , i] <- apply(myData, 2, function(x) as.numeric(x))
myData$subject <- as.factor(myData$subject)
myData$trial <- as.factor(myData$trial)

# create new columns for placement error, x error, and y error log transformed
myData$placement_error_cm_log <- log(myData$placement_error_cm+1)
myData$abs_x_error_cm_log <- log(myData$abs_x_error_cm+1)
myData$abs_y_error_cm_log <- log(myData$abs_y_error_cm+1)
str(myData)

# create normalized values by dividing study fixation count by study duration (in seconds)
myData$s.landmarks_norm <- myData$s.landmarks/myData$s.duration
myData$s.same_object_norm <- myData$s.same_object/myData$s.duration
myData$s.DOSW_norm <- myData$s.DOSW/myData$s.duration
myData$s.wall_norm <- myData$s.wall/myData$s.duration
myData$s.DODW_norm <- myData$s.DODW/myData$s.duration
myData$s.cart_norm <- myData$s.cart/myData$s.duration
myData$s.other_norm <- myData$s.other/myData$s.duration
myData$s.obj_to_lm_norm <- myData$s.obj_to_lm/myData$s.duration
myData$s.lm_to_obj_norm <- myData$s.lm_to_obj/myData$s.duration
myData$s.obj_to_so_norm <- myData$s.obj_to_so/myData$s.duration
myData$s.obj_to_diffObj_norm <- myData$s.obj_to_diffObj/myData$s.duration
myData$s.lm_to_lm_norm <- myData$s.lm_to_lm/myData$s.duration

# create normalized values by dividing retrieval fixation count by retrieval duration (in seconds)
myData$r.landmarks_norm <- myData$r.landmarks/myData$r.duration
myData$r.same_object_norm <- myData$r.same_object/myData$r.duration
myData$r.DOSW_norm <- myData$r.DOSW/myData$r.duration
myData$r.wall_norm <- myData$r.wall/myData$r.duration
myData$r.DODW_norm <- myData$r.DODW/myData$r.duration
myData$r.cart_norm <- myData$r.cart/myData$r.duration
myData$r.other_norm <- myData$r.other/myData$r.duration
myData$r.obj_to_lm_norm <- myData$r.obj_to_lm/myData$r.duration
myData$r.lm_to_obj_norm <- myData$r.lm_to_obj/myData$r.duration
myData$r.obj_to_so_norm <- myData$r.obj_to_so/myData$r.duration
myData$r.obj_to_diffObj_norm <- myData$r.obj_to_diffObj/myData$r.duration
myData$r.lm_to_lm_norm <- myData$r.lm_to_lm/myData$r.duration

# create log transformed values by for skewed categories (but spoiler alert: they are not all skewed)
myData$s.landmarks_norm_log <- log(myData$s.landmarks_norm+1)
myData$s.same_object_norm_log <- log(myData$s.same_object_norm+1)
myData$s.DOSW_norm_log <- log(myData$s.DOSW_norm+1)
myData$s.wall_norm_log <- log(myData$s.wall_norm+1)
myData$s.DODW_norm_log <- log(myData$s.DODW_norm+1)
myData$s.cart_norm_log <- log(myData$s.cart_norm+1)
myData$s.other_norm_log <- log(myData$s.other_norm+1)
myData$s.obj_to_lm_norm_log <- log(myData$s.obj_to_lm_norm+1)
myData$s.lm_to_obj_norm_log <- log(myData$s.lm_to_obj_norm+1)
myData$s.obj_to_so_norm_log <- log(myData$s.obj_to_so_norm+1)
myData$s.obj_to_diffObj_norm_log <- log(myData$s.obj_to_diffObj_norm+1)
myData$s.lm_to_lm_norm_log <- log(myData$s.lm_to_lm_norm+1)

myData$r.landmarks_norm_log <- log(myData$r.landmarks_norm+1)
myData$r.same_object_norm_log <- log(myData$r.same_object_norm+1)
myData$r.DOSW_norm_log <- log(myData$r.DOSW_norm+1)
myData$r.wall_norm_log <- log(myData$r.wall_norm+1)
myData$r.DODW_norm_log <- log(myData$r.DODW_norm+1)
myData$r.cart_norm_log <- log(myData$r.cart_norm+1)
myData$r.other_norm_log <- log(myData$r.other_norm+1)
myData$r.obj_to_lm_norm_log <- log(myData$r.obj_to_lm_norm+1)
myData$r.lm_to_obj_norm_log <- log(myData$r.lm_to_obj_norm+1)
myData$r.obj_to_so_norm_log <- log(myData$r.obj_to_so_norm+1)
myData$r.obj_to_diffObj_norm_log <- log(myData$r.obj_to_diffObj_norm+1)
myData$r.lm_to_lm_norm_log <- log(myData$r.lm_to_lm_norm+1)

# create a dataset that groups fixation counts, normed fixations, and log fixations by subject
subject_df <- myData %>%
  group_by(subject) %>%
  summarize(
    s.landmarks = mean(s.landmarks, na.rm = TRUE),
    s.same_object = mean(s.same_object, na.rm = TRUE),
    s.DOSW = mean(s.DOSW, na.rm = TRUE),
    s.wall = mean(s.wall, na.rm = TRUE),
    s.DODW = mean(s.DODW, na.rm = TRUE),
    s.cart = mean(s.cart, na.rm = TRUE),
    s.other = mean(s.other, na.rm = TRUE),
    s.obj_to_lm = mean(s.obj_to_lm, na.rm = TRUE),
    s.lm_to_obj = mean(s.lm_to_obj, na.rm = TRUE),
    s.obj_to_so = mean(s.obj_to_so, na.rm = TRUE),
    s.obj_to_diffObj = mean(s.obj_to_diffObj, na.rm = TRUE),
    s.lm_to_lm = mean(s.lm_to_lm, na.rm = TRUE),
    r.landmarks = mean(r.landmarks, na.rm = TRUE),
    r.same_object = mean(r.same_object, na.rm = TRUE),
    r.DOSW = mean(r.DOSW, na.rm = TRUE),
    r.wall = mean(r.wall, na.rm = TRUE),
    r.DODW = mean(r.DODW, na.rm = TRUE),
    r.cart = mean(r.cart, na.rm = TRUE),
    r.other = mean(r.other, na.rm = TRUE),
    r.obj_to_lm = mean(r.obj_to_lm, na.rm = TRUE),
    r.lm_to_obj = mean(r.lm_to_obj, na.rm = TRUE),
    r.obj_to_so = mean(r.obj_to_so, na.rm = TRUE),
    r.obj_to_diffObj = mean(r.obj_to_diffObj, na.rm = TRUE),
    r.lm_to_lm = mean(r.lm_to_lm, na.rm = TRUE),
    s.landmarks_norm = mean(s.landmarks_norm, na.rm = TRUE),
    s.same_object_norm = mean(s.same_object_norm, na.rm = TRUE),
    s.DOSW_norm = mean(s.DOSW_norm, na.rm = TRUE),
    s.wall_norm = mean(s.wall_norm, na.rm = TRUE), 
    s.DODW_norm = mean(s.DODW_norm, na.rm = TRUE),
    s.cart_norm = mean(s.cart_norm, na.rm = TRUE),
    s.other_norm = mean(s.other_norm, na.rm = TRUE),
    s.obj_to_lm_norm = mean(s.obj_to_lm_norm, na.rm = TRUE),
    s.lm_to_obj_norm = mean(s.lm_to_obj_norm, na.rm = TRUE),
    s.obj_to_so_norm = mean(s.obj_to_so_norm, na.rm = TRUE),
    s.obj_to_diffObj_norm = mean(s.obj_to_diffObj_norm, na.rm = TRUE),
    s.lm_to_lm_norm = mean(s.lm_to_lm_norm, na.rm = TRUE),
    r.landmarks_norm = mean(r.landmarks_norm, na.rm = TRUE),
    r.same_object_norm = mean(r.same_object_norm, na.rm = TRUE),
    r.DOSW_norm = mean(r.DOSW_norm, na.rm = TRUE),
    r.wall_norm = mean(r.wall_norm, na.rm = TRUE), 
    r.DODW_norm = mean(r.DODW_norm, na.rm = TRUE),
    r.cart_norm = mean(r.cart_norm, na.rm = TRUE),
    r.other_norm = mean(r.other_norm, na.rm = TRUE),
    r.obj_to_lm_norm = mean(r.obj_to_lm_norm, na.rm = TRUE),
    r.lm_to_obj_norm = mean(r.lm_to_obj_norm, na.rm = TRUE),
    r.obj_to_so_norm = mean(r.obj_to_so_norm, na.rm = TRUE),
    r.obj_to_diffObj_norm = mean(r.obj_to_diffObj_norm, na.rm = TRUE),
    r.lm_to_lm_norm = mean(r.lm_to_lm_norm, na.rm = TRUE),
    s.landmarks_norm_log = mean(s.landmarks_norm_log, na.rm = TRUE),
    s.same_object_norm_log = mean(s.same_object_norm_log, na.rm = TRUE),
    s.DOSW_norm_log = mean(s.DOSW_norm_log, na.rm = TRUE),
    s.wall_norm_log = mean(s.wall_norm_log, na.rm = TRUE),
    s.DODW_norm_log = mean(s.DODW_norm_log, na.rm = TRUE),
    s.cart_norm_log = mean(s.cart_norm_log, na.rm = TRUE),
    s.other_norm_log = mean(s.other_norm_log, na.rm = TRUE),
    s.obj_to_lm_norm_log = mean(s.obj_to_lm_norm_log, na.rm = TRUE),
    s.lm_to_obj_norm_log = mean(s.lm_to_obj_norm_log, na.rm = TRUE),
    s.obj_to_so_norm_log = mean(s.obj_to_so_norm_log, na.rm = TRUE),
    s.obj_to_diffObj_norm_log = mean(s.obj_to_diffObj_norm_log, na.rm = TRUE),
    s.lm_to_lm_norm_log = mean(s.lm_to_lm_norm_log, na.rm = TRUE),
    r.landmarks_norm_log = mean(r.landmarks_norm_log, na.rm = TRUE),
    r.same_object_norm_log = mean(r.same_object_norm_log, na.rm = TRUE),
    r.DOSW_norm_log = mean(r.DOSW_norm_log, na.rm = TRUE),
    r.wall_norm_log = mean(r.wall_norm_log, na.rm = TRUE),
    r.DODW_norm_log = mean(r.DODW_norm_log, na.rm = TRUE),
    r.cart_norm_log = mean(r.cart_norm_log, na.rm = TRUE),
    r.other_norm_log = mean(r.other_norm_log, na.rm = TRUE),
    r.obj_to_lm_norm_log = mean(r.obj_to_lm_norm_log, na.rm = TRUE),
    r.lm_to_obj_norm_log = mean(r.lm_to_obj_norm_log, na.rm = TRUE),
    r.obj_to_so_norm_log = mean(r.obj_to_so_norm_log, na.rm = TRUE),
    r.obj_to_diffObj_norm_log = mean(r.obj_to_diffObj_norm_log, na.rm = TRUE),
    r.lm_to_lm_norm_log = mean(r.lm_to_lm_norm_log, na.rm = TRUE), 
    placement_error_cm_log = mean(placement_error_cm_log),
    abs_x_error_cm_log = mean(abs_x_error_cm_log),
    abs_y_error_cm_log = mean(abs_y_error_cm_log),
    Total_duration_of_fixations = mean(Total_duration_of_fixations),
    Average_duration_of_fixations = mean(Average_duration_of_fixations),
    Number_of_fixations = mean(Number_of_fixations), 
    Peak_velocity_of_entry_saccade = mean(Peak_velocity_of_entry_saccade),
    Peak_velocity_of_exit_saccade = mean(Peak_velocity_of_exit_saccade)
  )

# create dataset for study normed and logged values by subject
study_subject_norm_df <- myData %>%
  group_by(subject) %>%
  summarize(
    s.landmarks_norm = mean(s.landmarks_norm, na.rm = TRUE),
    s.same_object_norm = mean(s.same_object_norm, na.rm = TRUE),
    s.DOSW_norm = mean(s.DOSW_norm, na.rm = TRUE),
    s.wall_norm = mean(s.wall_norm, na.rm = TRUE), 
    s.DODW_norm = mean(s.DODW_norm, na.rm = TRUE),
    s.cart_norm = mean(s.cart_norm, na.rm = TRUE),
    s.other_norm = mean(s.other_norm, na.rm = TRUE),
    s.obj_to_lm_norm = mean(s.obj_to_lm_norm, na.rm = TRUE),
    s.lm_to_obj_norm = mean(s.lm_to_obj_norm, na.rm = TRUE),
    s.obj_to_so_norm = mean(s.obj_to_so_norm, na.rm = TRUE),
    s.obj_to_diffObj_norm = mean(s.obj_to_diffObj_norm, na.rm = TRUE),
    s.lm_to_lm_norm = mean(s.lm_to_lm_norm, na.rm = TRUE),
    placement_error_cm = mean(placement_error_cm),
    abs_x_error_cm = mean(abs_x_error_cm),
    abs_y_error_cm = mean(abs_y_error_cm),
    s.landmarks_norm_log = mean(s.landmarks_norm_log, na.rm = TRUE),
    s.same_object_norm_log = mean(s.same_object_norm_log, na.rm = TRUE),
    s.DOSW_norm_log = mean(s.DOSW_norm_log, na.rm = TRUE),
    s.wall_norm_log = mean(s.wall_norm_log, na.rm = TRUE), 
    s.DODW_norm_log = mean(s.DODW_norm_log, na.rm = TRUE),
    s.cart_norm_log = mean(s.cart_norm_log, na.rm = TRUE),
    s.other_norm_log = mean(s.other_norm_log, na.rm = TRUE),
    s.obj_to_lm_norm_log = mean(s.obj_to_lm_norm_log, na.rm = TRUE),
    s.lm_to_obj_norm_log = mean(s.lm_to_obj_norm_log, na.rm = TRUE),
    s.obj_to_so_norm_log = mean(s.obj_to_so_norm_log, na.rm = TRUE),
    s.obj_to_diffObj_norm_log = mean(s.obj_to_diffObj_norm_log, na.rm = TRUE),
    s.lm_to_lm_norm_log = mean(s.lm_to_lm_norm_log, na.rm = TRUE),
    placement_error_cm_log = mean(placement_error_cm_log),
    abs_x_error_cm_log = mean(abs_x_error_cm_log),
    abs_y_error_cm_log = mean(abs_y_error_cm_log)
  )

# create dataset for retrieval normed and logged values by subject
retrieval_subject_norm_df <- myData %>%
  group_by(subject) %>%
  summarize(
    r.landmarks_norm = mean(r.landmarks_norm, na.rm = TRUE),
    r.same_object_norm = mean(r.same_object_norm, na.rm = TRUE),
    r.DOSW_norm = mean(r.DOSW_norm, na.rm = TRUE),
    r.wall_norm = mean(r.wall_norm, na.rm = TRUE), 
    r.DODW_norm = mean(r.DODW_norm, na.rm = TRUE),
    r.cart_norm = mean(r.cart_norm, na.rm = TRUE),
    r.other_norm = mean(r.other_norm, na.rm = TRUE),
    r.obj_to_lm_norm = mean(r.obj_to_lm_norm, na.rm = TRUE),
    r.lm_to_obj_norm = mean(r.lm_to_obj_norm, na.rm = TRUE),
    r.obj_to_so_norm = mean(r.obj_to_so_norm, na.rm = TRUE),
    r.obj_to_diffObj_norm = mean(r.obj_to_diffObj_norm, na.rm = TRUE),
    r.lm_to_lm_norm = mean(r.lm_to_lm_norm, na.rm = TRUE),
    placement_error_cm = mean(placement_error_cm),
    abs_x_error_cm = mean(abs_x_error_cm),
    abs_y_error_cm = mean(abs_y_error_cm),
    r.landmarks_norm_log = mean(r.landmarks_norm_log, na.rm = TRUE),
    r.same_object_norm_log = mean(r.same_object_norm_log, na.rm = TRUE),
    r.DOSW_norm_log = mean(r.DOSW_norm_log, na.rm = TRUE),
    r.wall_norm_log = mean(r.wall_norm_log, na.rm = TRUE), 
    r.DODW_norm_log = mean(r.DODW_norm_log, na.rm = TRUE),
    r.cart_norm_log = mean(r.cart_norm_log, na.rm = TRUE),
    r.other_norm_log = mean(r.other_norm_log, na.rm = TRUE),
    r.obj_to_lm_norm_log = mean(r.obj_to_lm_norm_log, na.rm = TRUE),
    r.lm_to_obj_norm_log = mean(r.lm_to_obj_norm_log, na.rm = TRUE),
    r.obj_to_so_norm_log = mean(r.obj_to_so_norm_log, na.rm = TRUE),
    r.obj_to_diffObj_norm_log = mean(r.obj_to_diffObj_norm_log, na.rm = TRUE),
    r.lm_to_lm_norm_log = mean(r.lm_to_lm_norm_log, na.rm = TRUE),
    placement_error_cm_log = mean(placement_error_cm_log),
    abs_x_error_cm_log = mean(abs_x_error_cm_log),
    abs_y_error_cm_log = mean(abs_y_error_cm_log)
  )

# put the study subject grouped data in long format
study_subject_norm_long <- study_subject_norm_df %>%
  gather(key = "trial", value = "norm_fixation_mean", s.landmarks_norm, s.same_object_norm, s.DOSW_norm, s.wall_norm, s.DODW_norm, s.cart_norm, s.other_norm,
         s.obj_to_lm_norm, s.lm_to_obj_norm, s.obj_to_so_norm, s.obj_to_diffObj_norm, s.lm_to_lm_norm,
         s.landmarks_norm_log, s.same_object_norm_log, s.DOSW_norm_log, s.other_norm_log,
         s.landmarks_norm_log, s.same_object_norm_log, s.DOSW_norm_log, s.wall_norm_log, s.DODW_norm_log, s.cart_norm_log, s.other_norm_log, 
         s.obj_to_lm_norm_log, s.lm_to_obj_norm_log, s.obj_to_so_norm_log, s.obj_to_diffObj_norm_log, s.lm_to_lm_norm_log) %>%
  convert_as_factor(subject,trial)
  
# put the retrieval subject grouped data in long format
retrieval_subject_norm_long <- retrieval_subject_norm_df %>%
  gather(key = "trial", value = "norm_fixation_mean", r.landmarks_norm, r.same_object_norm, r.DOSW_norm, r.wall_norm, r.DODW_norm, r.cart_norm, r.other_norm,
         r.obj_to_lm_norm, r.lm_to_obj_norm, r.obj_to_so_norm, r.obj_to_diffObj_norm, r.lm_to_lm_norm,
         r.landmarks_norm_log, r.same_object_norm_log, r.DOSW_norm_log, r.other_norm_log,
         r.landmarks_norm_log, r.same_object_norm_log, r.DOSW_norm_log, r.wall_norm_log, r.DODW_norm_log, r.cart_norm_log, r.other_norm_log, 
         r.obj_to_lm_norm_log, r.lm_to_obj_norm_log, r.obj_to_so_norm_log, r.obj_to_diffObj_norm_log, r.lm_to_lm_norm_log) %>%
  convert_as_factor(subject,trial)

# filter data into norm and norm_log values
study_only_norm_long <- study_subject_norm_long[1:361,]
study_only_norm_log_long <- study_subject_norm_long[362:720,]

retrieval_only_norm_long <- retrieval_subject_norm_long[1:361,]
retreival_only_norm_log_long <- retrieval_subject_norm_long[362:720,]

### check for skew - significant values noted, otherwise not significant
shapiro.test(subject_df$s.landmarks_norm) # p = .04124
shapiro.test(subject_df$s.same_object_norm_log)
shapiro.test(subject_df$s.DOSW_norm_log)
shapiro.test(subject_df$s.wall_norm_log)
shapiro.test(subject_df$s.DODW_norm_log)
shapiro.test(subject_df$s.cart_norm_log) # p = .02583
shapiro.test(subject_df$s.other_norm_log) # p < .001
shapiro.test(subject_df$s.obj_to_lm_norm_log)
shapiro.test(subject_df$s.lm_to_obj_norm_log) # p = .008417
shapiro.test(subject_df$s.obj_to_so_norm_log) # p = .001912
shapiro.test(subject_df$s.obj_to_diffObj_norm_log)
shapiro.test(subject_df$s.lm_to_lm_norm_log) # p = .007871

shapiro.test(subject_df$r.landmarks_norm) # not sig but p = .05057
shapiro.test(subject_df$r.same_object_norm_log)
shapiro.test(subject_df$r.DOSW_norm_log)
shapiro.test(subject_df$r.wall_norm_log)
shapiro.test(subject_df$r.DODW_norm_log)
shapiro.test(subject_df$r.cart_norm_log) # p = .02651
shapiro.test(subject_df$r.other_norm_log)
shapiro.test(subject_df$r.obj_to_lm_norm_log)
shapiro.test(subject_df$r.lm_to_obj_norm_log)
shapiro.test(subject_df$r.obj_to_so_norm_log)
shapiro.test(subject_df$r.obj_to_diffObj_norm_log) # p = .01584
shapiro.test(subject_df$r.lm_to_lm_norm_log) # p = .03103

### test study against test in each category # all sig expect when specified as not sig
t.test(subject_df$s.landmarks_norm_log, subject_df$r.landmarks_norm_log, paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.same_object_norm_log, subject_df$r.same_object_norm_log, paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.DOSW_norm_log , subject_df$r.DOSW_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.wall_norm_log , subject_df$r.wall_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.DODW_norm_log , subject_df$r.DODW_norm_log , paired = TRUE, alternative = "two.sided") # not sig
t.test(subject_df$s.cart_norm_log , subject_df$r.cart_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.other_norm_log , subject_df$r.other_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.obj_to_lm_norm_log , subject_df$r.obj_to_lm_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.lm_to_obj_norm_log , subject_df$r.lm_to_obj_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.obj_to_so_norm_log , subject_df$r.obj_to_so_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.obj_to_diffObj_norm_log , subject_df$r.obj_to_diffObj_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.lm_to_lm_norm_log , subject_df$r.lm_to_lm_norm_log , paired = TRUE, alternative = "two.sided") # sig at p = .03357

ggpaired(subject_df, cond1 = "s.landmarks_norm_log", cond2 = "r.landmarks_norm_log")
ggpaired(subject_df, cond1 = "s.same_object_norm_log", cond2 = "r.same_object_norm_log" )
ggpaired(subject_df, cond1 = "s.DOSW_norm_log", cond2 = "r.DOSW_norm_log")
ggpaired(subject_df, cond1 = "s.wall_norm_log", cond2 = "r.wall_norm_log")
ggpaired(subject_df, cond1 = "s.DODW_norm_log", cond2 = "r.DODW_norm_log")
ggpaired(subject_df, cond1 = "s.cart_norm_log", cond2 = "r.cart_norm_log")
ggpaired(subject_df, cond1 = "s.other_norm_log", cond2 = "r.other_norm_log")
ggpaired(subject_df, cond1 = "s.obj_to_lm_norm_log", cond2 = "r.obj_to_lm_norm_log")
ggpaired(subject_df, cond1 = "s.lm_to_obj_norm_log", cond2 = "r.lm_to_obj_norm_log")
ggpaired(subject_df, cond1 = "s.obj_to_so_norm_log", cond2 = "r.obj_to_so_norm_log")
ggpaired(subject_df, cond1 = "s.obj_to_diffObj_norm_log", cond2 = "r.obj_to_diffObj_norm_log")
ggpaired(subject_df, cond1 = "s.lm_to_lm_norm_log", cond2 = "r.lm_to_lm_norm_log")

mean_and_sd <- function(data, ttestvalue1, ttestvalue2) {
  numbers1 <- eval(parse(text = paste(data,"$",ttestvalue1)))
  numbers2 <- eval(parse(text = paste(data,"$",ttestvalue2)))
  
  cats <- data.frame(matrix(ncol = 2, nrow = 2))
  x <- c("Mean", "SD")
  colnames(cats) <- x
  
  y <- c(ttestvalue1,ttestvalue2)
  rownames(cats) <- y
  
  cats[1,1] <- mean(numbers1, na.rm = TRUE)
  cats[1,2] <- sd(numbers1, na.rm = TRUE)
  cats[2,1] <- mean(numbers2, na.rm = TRUE)
  cats[2,2] <- sd(numbers2, na.rm = TRUE)
  
  return(cats)
}

# get mean and sd for making a table
mean_and_sd("subject_df", "s.landmarks_norm_log", "r.landmarks_norm_log")
mean_and_sd("subject_df", "s.same_object_norm_log", "r.same_object_norm_log")
mean_and_sd("subject_df", "s.DOSW_norm_log", "r.DOSW_norm_log")
mean_and_sd("subject_df", "s.wall_norm_log", "r.wall_norm_log")
mean_and_sd("subject_df", "s.DODW_norm_log", "r.DODW_norm_log")
mean_and_sd("subject_df", "s.cart_norm_log", "r.cart_norm_log")
mean_and_sd("subject_df", "s.other_norm_log", "r.other_norm_log")
mean_and_sd("subject_df", "s.obj_to_lm_norm_log", "r.obj_to_lm_norm_log")
mean_and_sd("subject_df", "s.lm_to_obj_norm_log", "r.lm_to_obj_norm_log")
mean_and_sd("subject_df", "s.obj_to_so_norm_log", "r.obj_to_so_norm_log")
mean_and_sd("subject_df", "s.obj_to_diffObj_norm_log", "r.obj_to_diffObj_norm_log")
mean_and_sd("subject_df", "s.lm_to_lm_norm_log", "r.lm_to_lm_norm_log")

# make a boxplot of just the study categories - norm values
study_bxp <- ggboxplot(study_only_norm_long, x = "trial", y = "norm_fixation_mean", add = "point", 
                       title = "Normalized Fixation Number per Category during Study", 
                       xlab = "", ylab = "Mean of Normalized Fixations") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), plot.title = element_text(hjust = 0.5))
study_bxp

# make a boxplot of just the study categories - norm log values
study_log_bxp <- ggboxplot(study_only_norm_log_long, x = "trial", y = "norm_fixation_mean", add = "point", 
                       title = "Log Normalized Fixation Number per Category during Study", 
                       xlab = "", ylab = "Mean of Log Normalized Fixations") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), plot.title = element_text(hjust = 0.5))
study_log_bxp # same pattern as just normalized

# make a boxplot of just the retrieval categories - norm values
retrieval_bxp <- ggboxplot(retrieval_only_norm_long, x = "trial", y = "norm_fixation_mean", add = "point", 
                       title = "Normalized Fixation Number per Category during Retrieval", 
                       xlab = "", ylab = "Mean of Normalized Fixations") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), plot.title = element_text(hjust = 0.5))
retrieval_bxp

# make a boxplot of just the retrieval categories - norm log values
retrieval_log_bxp <- ggboxplot(retreival_only_norm_log_long, x = "trial", y = "norm_fixation_mean", add = "point", 
                           title = "Log Normalized Fixation Number per Category during Retrieval", 
                           xlab = "", ylab = "Mean of Log Normalized Fixations") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), plot.title = element_text(hjust = 0.5))
retrieval_log_bxp



study_norm_aov <- anova_test(data = study_only_norm_long, dv = norm_fixation_mean, wid = subject, within = trial)
get_anova_table(study_norm_aov) # sig

study_norm_pwc <- pairwise.t.test(study_only_norm_long$norm_fixation_mean, study_only_norm_long$trial, p.adj = "bonferroni")
study_norm_pwc

study_norm_log_aov <- anova_test(data = study_only_norm_log_long, dv = norm_fixation_mean, wid = subject, within = trial)
get_anova_table(study_norm_log_aov) # sig

study_norm_log_pwc <- pairwise.t.test(study_only_norm_log_long$norm_fixation_mean, study_only_norm_log_long$trial, p.adj = "bonferroni")
study_norm_log_pwc

retrieval_norm_aov <- anova_test(data = retrieval_only_norm_long, dv = norm_fixation_mean, wid = subject, within = trial)
get_anova_table(retrieval_norm_aov) # sig

retrieval_norm_pwc <- pairwise.t.test(retrieval_only_norm_long$norm_fixation_mean, retrieval_only_norm_long$trial, p.adj = "bonferroni")
retrieval_norm_pwc

retrieval_norm_log_aov <- anova_test(data = retreival_only_norm_log_long, dv = norm_fixation_mean, wid = subject, within = trial)
get_anova_table(retrieval_norm_log_aov) # sig

retrieval_norm_log_pwc <- pairwise.t.test(retreival_only_norm_log_long$norm_fixation_mean, retreival_only_norm_log_long$trial, p.adj = "bonferroni")
retrieval_norm_log_pwc

pwc <- subject_counts_long %>%
  pairwise_t_test(
    means ~ trial, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
pwc

# correlations with performance
cor.test(subject_df$s.landmarks_norm_log, subject_df$placement_error_cm_log, method = "pearson")
plot(subject_df$s.landmarks_norm_log, subject_df$placement_error_cm_log)
cor.test(subject_df$s.same_object_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.DOSW_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.wall_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.DODW_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.cart_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.other_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.obj_to_lm_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.lm_to_obj_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.obj_to_so_norm_log, subject_df$placement_error_cm_log, method = "pearson") # sig neg cor
plot(subject_df$s.obj_to_so_norm_log, subject_df$placement_error_cm_log)
cor.test(subject_df$s.obj_to_diffObj_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.lm_to_lm_norm_log, subject_df$placement_error_cm_log, method = "pearson")

cor.test(subject_df$r.same_object_norm_log, subject_df$placement_error_cm_log, method = "pearson") # p = .07048
cor.test(subject_df$r.DOSW_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$r.wall_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$r.DODW_norm_log, subject_df$placement_error_cm_log, method = "pearson") # sig pos cor
cor.test(subject_df$r.cart_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$r.other_norm_log, subject_df$placement_error_cm_log, method = "pearson")
plot(subject_df$r.DODW_norm_log, subject_df$placement_error_cm_log)
cor.test(subject_df$r.obj_to_lm_norm_log, subject_df$placement_error_cm_log, method = "pearson") 
cor.test(subject_df$r.lm_to_obj_norm_log, subject_df$placement_error_cm_log, method = "pearson") 
cor.test(subject_df$r.obj_to_so_norm_log, subject_df$placement_error_cm_log, method = "pearson") # sig neg cor
cor.test(subject_df$r.obj_to_diffObj_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$r.lm_to_lm_norm_log, subject_df$placement_error_cm_log, method = "pearson")

cor.test(subject_df$Total_duration_of_fixations, subject_df$placement_error_cm_log, method = "pearson") # sig neg cor
cor.test(subject_df$Average_duration_of_fixations, subject_df$placement_error_cm_log, method = "pearson") # sig neg cor
cor.test(subject_df$Number_of_fixations, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$Peak_velocity_of_entry_saccade, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$Peak_velocity_of_exit_saccade, subject_df$placement_error_cm_log, method = "pearson") # sig pos cor
plot(subject_df$Peak_velocity_of_exit_saccade, subject_df$placement_error_cm_log)


# big regression with all sig ones to see which explains more variance with time to first fixation
# only r.DODW sig
only_sig_stuff_reg <- lm(formula = placement_error_cm_log ~ s.obj_to_so + r.same_object + r.DODW + r.obj_to_so + 
                 Total_duration_of_fixations + Average_duration_of_fixations + Peak_velocity_of_exit_saccade, data = subject_df)
summary(only_sig_stuff_reg)

only_sig_stuff_reg_norm <- lm(formula = placement_error_cm_log ~ s.obj_to_so_norm_log + r.DODW_norm_log + r.obj_to_so_norm_log + 
                           Total_duration_of_fixations + Average_duration_of_fixations + Peak_velocity_of_exit_saccade, data = subject_df)
summary(only_sig_stuff_reg_norm)

big_reg <- lm(formula = placement_error_cm_log ~ s.landmarks + s.same_object + s.DOSW + s.other + s.DODW +
                s.obj_to_lm + s.lm_to_obj + s.obj_to_so + s.obj_to_diffObj + s.lm_to_lm +
                r.landmarks + r.same_object + r.DOSW + r.other + r.DODW +
                r.obj_to_lm + r.lm_to_obj + r.obj_to_so + r.obj_to_diffObj + r.lm_to_lm +
                Total_duration_of_fixations + Average_duration_of_fixations + Number_of_fixations, data = subject_df)
summary(big_reg)

big_reg_norm <- lm(formula = placement_error_cm_log ~ s.landmarks_norm_log + s.same_object_norm_log + s.DOSW_norm_log + s.other_norm_log + s.DODW_norm_log +
                s.obj_to_lm_norm_log + s.lm_to_obj_norm_log + s.obj_to_so_norm_log + s.obj_to_diffObj_norm_log + s.lm_to_lm_norm_log +
                r.landmarks_norm_log + r.same_object_norm_log + r.DOSW_norm_log + r.other_norm_log + r.DODW_norm_log +
                r.obj_to_lm_norm_log + r.lm_to_obj_norm_log + r.obj_to_so_norm_log + r.obj_to_diffObj_norm_log + r.lm_to_lm_norm_log +
                Total_duration_of_fixations + Average_duration_of_fixations + Number_of_fixations, data = subject_df)
summary(big_reg_norm)
 











