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
setwd("E:/Nav_1stYr_project_data/GazeCode data")

# Read in data
inputData <- read_excel("E:/Nav_1stYr_project_data/GazeCode data/R_outputs/manuscript_data_N29_gazecode_byTrial_badTrialsDeleted.xlsx")
inputData <- as.data.frame(inputData)
str(inputData) # check the structure of the data

# make a copy so there is also a clean copy (inputData)
myData <- inputData

# make all the rows their correct data type (numeric or factor)
i <- c(1:59)
myData [i] <- lapply(myData[i], as.numeric)
myData$subject <- as.factor(myData$subject)
myData$trial <- as.factor(myData$trial)

# create new columns for placement error, x error, and y error log transformed
myData$placement_error_cm_log <- log(myData$placement_error_cm+1)
myData$abs_x_error_cm_log <- log(myData$abs_x_error_cm+1)
myData$abs_y_error_cm_log <- log(myData$abs_y_error_cm+1)
str(myData)

# create columns to combine specific fixation columns
myData$s.lm_obj_lm <- myData$s.lm_to_obj + myData$s.obj_to_lm
myData$r.lm_obj_lm <- myData$r.lm_to_obj + myData$r.obj_to_lm

myData$s.objects <- myData$s.DOSW + myData$s.DODW + myData$s.same_object
myData$r.objects <- myData$r.DOSW + myData$r.DODW + myData$r.same_object

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
myData$s.lm_obj_lm_norm <- myData$s.lm_obj_lm/myData$s.duration
myData$s.objects_norm <- myData$s.objects/myData$s.duration

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
myData$r.lm_obj_lm_norm <- myData$r.lm_obj_lm/myData$r.duration
myData$r.objects_norm <- myData$r.objects/myData$r.duration

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
myData$s.lm_obj_lm_norm_log <- log(myData$s.lm_obj_lm_norm +1)
myData$s.objects_norm_log <- log(myData$s.objects_norm +1)

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
myData$r.lm_obj_lm_norm_log <- log(myData$r.lm_obj_lm_norm+1)
myData$r.objects_norm_log <- log(myData$r.objects_norm+1)

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
    s.lm_obj_lm = mean(s.lm_obj_lm, na.rm = TRUE),
    s.objects = mean(s.objects, na.rm = TRUE),
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
    r.lm_obj_lm = mean(r.lm_obj_lm, na.rm = TRUE),
    r.objects = mean(r.objects, na.rm = TRUE),
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
    s.lm_obj_lm_norm = mean(s.lm_obj_lm_norm, na.rm = TRUE),
    s.objects_norm = mean(s.objects_norm, na.rm = TRUE),
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
    r.lm_obj_lm_norm = mean(r.lm_obj_lm_norm, na.rm = TRUE),
    r.objects_norm = mean(r.objects_norm, na.rm = TRUE),
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
    s.lm_obj_lm_norm_log = mean(s.lm_obj_lm_norm_log, na.rm = TRUE),
    s.objects_norm_log = mean(s.objects_norm_log, na.rm = TRUE),    
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
    r.lm_obj_lm_norm_log = mean(r.lm_obj_lm_norm_log, na.rm = TRUE),
    r.objects_norm_log = mean(r.objects_norm_log, na.rm = TRUE),
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
    s.lm_obj_lm_norm = mean(s.lm_obj_lm_norm, na.rm = TRUE),
    s.objects_norm = mean(s.objects_norm, na.rm = TRUE),
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
    s.lm_obj_lm_norm_log = mean(s.lm_obj_lm_norm_log, na.rm = TRUE),
    s.objects_norm_log = mean(s.objects_norm_log, na.rm = TRUE),   
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
    r.lm_obj_lm_norm = mean(r.lm_obj_lm_norm, na.rm = TRUE),
    r.objects_norm = mean(r.objects_norm, na.rm = TRUE),
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
    r.lm_obj_lm_norm_log = mean(r.lm_obj_lm_norm_log, na.rm = TRUE),
    r.objects_norm_log = mean(r.objects_norm_log, na.rm = TRUE),
    placement_error_cm_log = mean(placement_error_cm_log),
    abs_x_error_cm_log = mean(abs_x_error_cm_log),
    abs_y_error_cm_log = mean(abs_y_error_cm_log)
  )

# put the study subject grouped data in long format
study_subject_norm_long <- study_subject_norm_df %>%
  gather(key = "trial", value = "norm_fixation_mean", s.landmarks_norm, s.same_object_norm, s.DOSW_norm, s.wall_norm, s.DODW_norm, s.cart_norm, s.other_norm,
         s.obj_to_lm_norm, s.lm_to_obj_norm, s.obj_to_so_norm, s.obj_to_diffObj_norm, s.lm_to_lm_norm, s.lm_obj_lm_norm, s.objects_norm,
         s.landmarks_norm_log, s.same_object_norm_log, s.DOSW_norm_log, s.wall_norm_log, s.DODW_norm_log, s.cart_norm_log, s.other_norm_log, 
         s.obj_to_lm_norm_log, s.lm_to_obj_norm_log, s.obj_to_so_norm_log, s.obj_to_diffObj_norm_log, s.lm_to_lm_norm_log, s.lm_obj_lm_norm_log, s.objects_norm_log) %>%
  convert_as_factor(subject,trial)
  
# put the retrieval subject grouped data in long format
retrieval_subject_norm_long <- retrieval_subject_norm_df %>%
  gather(key = "trial", value = "norm_fixation_mean", r.landmarks_norm, r.same_object_norm, r.DOSW_norm, r.wall_norm, r.DODW_norm, r.cart_norm, r.other_norm,
         r.obj_to_lm_norm, r.lm_to_obj_norm, r.obj_to_so_norm, r.obj_to_diffObj_norm, r.lm_to_lm_norm, r.lm_obj_lm_norm, r.objects_norm, 
         r.landmarks_norm_log, r.same_object_norm_log, r.DOSW_norm_log, r.wall_norm_log, r.DODW_norm_log, r.cart_norm_log, r.other_norm_log, 
         r.obj_to_lm_norm_log, r.lm_to_obj_norm_log, r.obj_to_so_norm_log, r.obj_to_diffObj_norm_log, r.lm_to_lm_norm_log, r.lm_obj_lm_norm_log, r.objects_norm_log) %>%
  convert_as_factor(subject,trial)

# filter data into norm and norm_log values
study_only_norm_long <- study_subject_norm_long[1:420, ]
study_only_norm_log_long <- study_subject_norm_long[421:840, ]

retrieval_only_norm_long <- retrieval_subject_norm_long[1:420, ]
retreival_only_norm_log_long <- retrieval_subject_norm_long[421:840, ]

# make group for only LM, Wall, Other, and Objects categories during study and retrieval separately
study_lm_wall_other <- study_subject_norm_long[c(1:30, 91:120, 181:210, 391:420), ]
study_lm_wall_other_log <- study_subject_norm_long[c(421:450, 511:540, 601:630, 811:840), ]

retrieval_lm_wall_other <- retrieval_subject_norm_long[c(1:30, 91:120, 181:210, 391:420), ]
retrieval_lm_wall_other_log <- retrieval_subject_norm_long[c(421:450, 511:540, 601:630, 811:840), ]

# make a group for successive fixations: lm_lm, obj_obj, lm_obj_lm,
study_successive_fix <- study_subject_norm_long[301:390, ]
retrieval_successive_fix <- retrieval_subject_norm_long[301:390, ]

### check for skew - significant values noted, otherwise not significant
shapiro.test(subject_df$s.landmarks_norm) # not sig but p = .06489
shapiro.test(subject_df$s.same_object_norm_log)
shapiro.test(subject_df$s.DOSW_norm_log)
shapiro.test(subject_df$s.wall_norm_log)
shapiro.test(subject_df$s.DODW_norm_log)
shapiro.test(subject_df$s.cart_norm_log) # p < .001
shapiro.test(subject_df$s.other_norm_log) # p < .001
shapiro.test(subject_df$s.obj_to_lm_norm_log)
shapiro.test(subject_df$s.lm_to_obj_norm_log) # p = .02624
shapiro.test(subject_df$s.obj_to_so_norm_log) # p = .002047
shapiro.test(subject_df$s.obj_to_diffObj_norm_log)
shapiro.test(subject_df$s.lm_to_lm_norm_log) # p = .002967
shapiro.test(subject_df$s.lm_obj_lm_norm_log)
shapiro.test(subject_df$s.objects_norm_log)

shapiro.test(subject_df$r.landmarks_norm) # p = .009755
shapiro.test(subject_df$r.same_object_norm_log)
shapiro.test(subject_df$r.DOSW_norm_log) # not sig but p = .07021
shapiro.test(subject_df$r.wall_norm_log)
shapiro.test(subject_df$r.DODW_norm_log) # p = .01362
shapiro.test(subject_df$r.cart_norm_log)
shapiro.test(subject_df$r.other_norm_log)
shapiro.test(subject_df$r.obj_to_lm_norm_log)
shapiro.test(subject_df$r.lm_to_obj_norm_log)
shapiro.test(subject_df$r.obj_to_so_norm_log)
shapiro.test(subject_df$r.obj_to_diffObj_norm_log) # not sig but p = .09054
shapiro.test(subject_df$r.lm_to_lm_norm_log) # p = .002219
shapiro.test(subject_df$r.lm_obj_lm_norm_log)
shapiro.test(subject_df$r.objects_norm_log)

### test study against test in each category # all sig expect when specified as not sig
t.test(subject_df$s.landmarks_norm_log, subject_df$r.landmarks_norm_log, paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.same_object_norm_log, subject_df$r.same_object_norm_log, paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.DOSW_norm_log , subject_df$r.DOSW_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.wall_norm_log , subject_df$r.wall_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.DODW_norm_log , subject_df$r.DODW_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.other_norm_log , subject_df$r.other_norm_log , paired = TRUE, alternative = "two.sided") # sig at p = .001593
t.test(subject_df$s.obj_to_lm_norm_log , subject_df$r.obj_to_lm_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.lm_to_obj_norm_log , subject_df$r.lm_to_obj_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.obj_to_so_norm_log , subject_df$r.obj_to_so_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.obj_to_diffObj_norm_log , subject_df$r.obj_to_diffObj_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.lm_to_lm_norm_log , subject_df$r.lm_to_lm_norm_log , paired = TRUE, alternative = "two.sided") # sig at p = .03938
t.test(subject_df$s.objects_norm_log , subject_df$r.objects_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.lm_obj_lm_norm_log , subject_df$r.lm_obj_lm_norm_log , paired = TRUE, alternative = "two.sided")

ggpaired(subject_df, cond1 = "s.landmarks_norm_log", cond2 = "r.landmarks_norm_log")
ggpaired(subject_df, cond1 = "s.same_object_norm_log", cond2 = "r.same_object_norm_log" )
ggpaired(subject_df, cond1 = "s.DOSW_norm_log", cond2 = "r.DOSW_norm_log")
ggpaired(subject_df, cond1 = "s.wall_norm_log", cond2 = "r.wall_norm_log")
ggpaired(subject_df, cond1 = "s.DODW_norm_log", cond2 = "r.DODW_norm_log")
ggpaired(subject_df, cond1 = "s.other_norm_log", cond2 = "r.other_norm_log")
ggpaired(subject_df, cond1 = "s.obj_to_lm_norm_log", cond2 = "r.obj_to_lm_norm_log")
ggpaired(subject_df, cond1 = "s.lm_to_obj_norm_log", cond2 = "r.lm_to_obj_norm_log")
ggpaired(subject_df, cond1 = "s.obj_to_so_norm_log", cond2 = "r.obj_to_so_norm_log")
ggpaired(subject_df, cond1 = "s.obj_to_diffObj_norm_log", cond2 = "r.obj_to_diffObj_norm_log")
ggpaired(subject_df, cond1 = "s.objects_norm_log", cond2 = "r.objects_norm_log")
ggpaired(subject_df, cond1 = "s.lm_obj_lm_norm_log", cond2 = "r.lm_obj_lm_norm_log")

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
mean_and_sd("subject_df", "s.other_norm_log", "r.other_norm_log")
mean_and_sd("subject_df", "s.obj_to_lm_norm_log", "r.obj_to_lm_norm_log")
mean_and_sd("subject_df", "s.lm_to_obj_norm_log", "r.lm_to_obj_norm_log")
mean_and_sd("subject_df", "s.obj_to_so_norm_log", "r.obj_to_so_norm_log")
mean_and_sd("subject_df", "s.obj_to_diffObj_norm_log", "r.obj_to_diffObj_norm_log")
mean_and_sd("subject_df", "s.lm_to_lm_norm_log", "r.lm_to_lm_norm_log")
mean_and_sd("subject_df", "s.objects_norm_log", "r.objects_norm_log")
mean_and_sd("subject_df", "s.lm_obj_lm_norm_log", "r.lm_obj_lm_norm_log")

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


# big ANOVAs for study and retrieval
study_norm_aov <- anova_test(data = study_only_norm_long, dv = norm_fixation_mean, wid = subject, within = trial)
get_anova_table(study_norm_aov) # sig

study_norm_pwc <- pairwise.t.test(study_only_norm_long$norm_fixation_mean, study_only_norm_long$trial, p.adj = "bonferroni", paired = TRUE)
study_norm_pwc

study_norm_log_aov <- anova_test(data = study_only_norm_log_long, dv = norm_fixation_mean, wid = subject, within = trial)
get_anova_table(study_norm_log_aov) # sig

study_norm_log_pwc <- pairwise.t.test(study_only_norm_log_long$norm_fixation_mean, study_only_norm_log_long$trial, p.adj = "bonferroni", paired = TRUE)
study_norm_log_pwc

retrieval_norm_aov <- anova_test(data = retrieval_only_norm_long, dv = norm_fixation_mean, wid = subject, within = trial)
get_anova_table(retrieval_norm_aov) # sig

retrieval_norm_pwc <- pairwise.t.test(retrieval_only_norm_long$norm_fixation_mean, retrieval_only_norm_long$trial, p.adj = "bonferroni", paired = TRUE)
retrieval_norm_pwc

retrieval_norm_log_aov <- anova_test(data = retreival_only_norm_log_long, dv = norm_fixation_mean, wid = subject, within = trial)
get_anova_table(retrieval_norm_log_aov) # sig

retrieval_norm_log_pwc <- pairwise.t.test(retreival_only_norm_log_long$norm_fixation_mean, retreival_only_norm_log_long$trial, p.adj = "bonferroni", paired = TRUE)
retrieval_norm_log_pwc


# correlations with performance
cor.test(subject_df$s.landmarks_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.same_object_norm_log, subject_df$placement_error_cm_log, method = "pearson") # sig
plot(subject_df$s.same_object_norm_log, subject_df$placement_error_cm_log)
cor.test(subject_df$s.DOSW_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.wall_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.DODW_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.cart_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.other_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.obj_to_lm_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.lm_to_obj_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.obj_to_so_norm_log, subject_df$placement_error_cm_log, method = "pearson") # sig neg cor, almost .05
plot(subject_df$s.obj_to_so_norm_log, subject_df$placement_error_cm_log)
cor.test(subject_df$s.obj_to_diffObj_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.lm_to_lm_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.lm_obj_lm_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.objects_norm_log, subject_df$placement_error_cm_log, method = "pearson")

cor.test(subject_df$r.same_object_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$r.DOSW_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$r.wall_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$r.DODW_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$r.other_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$r.obj_to_lm_norm_log, subject_df$placement_error_cm_log, method = "pearson") 
cor.test(subject_df$r.lm_to_obj_norm_log, subject_df$placement_error_cm_log, method = "pearson") 
cor.test(subject_df$r.obj_to_so_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$r.obj_to_diffObj_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$r.lm_to_lm_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$r.lm_obj_lm_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$r.objects_norm_log, subject_df$placement_error_cm_log, method = "pearson")

cor.test(subject_df$Total_duration_of_fixations, subject_df$placement_error_cm_log, method = "pearson") # sig neg cor, almost .05
cor.test(subject_df$Average_duration_of_fixations, subject_df$placement_error_cm_log, method = "pearson") # sig neg cor
cor.test(subject_df$Number_of_fixations, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$Peak_velocity_of_entry_saccade, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$Peak_velocity_of_exit_saccade, subject_df$placement_error_cm_log, method = "pearson") # sig pos cor
plot(subject_df$Peak_velocity_of_exit_saccade, subject_df$placement_error_cm_log)


plot(subject_df$s.landmarks_norm_log, subject_df$placement_error_cm_log)

plot(subject_df$Total_duration_of_fixations, subject_df$placement_error_cm_log) + stat_cor(method = "pearson", label.x = 600, label.y = 3.8)
plot(subject_df$Average_duration_of_fixations, subject_df$placement_error_cm_log)

# uncomment this to save manuscript-quality pics to this folder
setwd("C:/Users/amuller/Desktop/Alana/UA/HSCL/First-Year Project/Manuscript/Pics")

x <- subject_df$Total_duration_of_fixations
y<- subject_df$placement_error_cm_log

ggplot( subject_df, aes( x=x, y=y ))+
  geom_point()+
  stat_cor(method = "pearson", label.x = 600, label.y = 3.8)


x <- subject_df$Average_duration_of_fixations
y<- subject_df$placement_error_cm_log

avg_fix_dur <-ggplot(subject_df, aes( x=x, y=y ))+
  geom_point()+
  stat_cor(method = "pearson", label.x = 600, label.y = 3.8) +
  theme_classic() + xlab("Average duration of fixations (sec)") +
  ylab("Placement Error (log cm)") + 
  geom_smooth(method = 'lm')
jpeg("avg_fix_dur.jpeg", width = 7, height = 5, units = 'in', res = 500)
avg_fix_dur
dev.off()

# big regression with all sig ones to see which explains more variance with time to first fixation

big_reg_norm <- lm(formula = placement_error_cm_log ~ s.landmarks_norm_log + s.objects_norm_log + s.DOSW_norm_log + s.wall_norm_log + s.DODW_norm_log +
                s.other_norm_log + s.lm_obj_lm_norm_log + s.obj_to_so_norm_log + s.obj_to_diffObj_norm_log + s.lm_to_lm_norm_log +
                r.landmarks_norm_log + r.objects_norm_log + r.DOSW_norm_log + r.wall_norm_log + r.DODW_norm_log + r.other_norm_log +
                r.lm_obj_lm_norm_log + r.obj_to_so_norm_log + r.obj_to_diffObj_norm_log + r.lm_to_lm_norm_log +
                Total_duration_of_fixations + Average_duration_of_fixations + Number_of_fixations, data = subject_df)
summary(big_reg_norm)



### conceptual ANOVAs and t-tests and figures

# ANOVA for LM, wall, objects, and other for study and retrieval separately
study_lmWallOther_aov <- anova_test(data = study_lm_wall_other_log, dv = norm_fixation_mean, wid = subject, within = trial)
get_anova_table(study_lmWallOther_aov) # sig

study_lmWallOther_pwc <- pairwise.t.test(study_lm_wall_other_log$norm_fixation_mean, study_lm_wall_other_log$trial, p.adj = "bonferroni", paired = TRUE)
study_lmWallOther_pwc # all sig diff from each other

pair <- study_lm_wall_other_log %>%
  pairwise_t_test(norm_fixation_mean~trial, paired = TRUE, p.adjust.method = "bonferroni", paired = TRUE)
data.frame(pair)


retrieval_lmWallOther_aov <- anova_test(data = retrieval_lm_wall_other_log, dv = norm_fixation_mean, wid = subject, within = trial)
get_anova_table(retrieval_lmWallOther_aov) # sig

retrieval_lmWallOther_pwc <- pairwise.t.test(retrieval_lm_wall_other_log$norm_fixation_mean, retrieval_lm_wall_other_log$trial, p.adj = "bonferroni", paired = TRUE)
retrieval_lmWallOther_pwc # all sig diff from each other


# need to make trial a character, not a factor or the renaming won't work
study_lm_wall_other_log$trial <- as.character(study_lm_wall_other_log$trial)
study_lm_wall_other_log$trial[study_lm_wall_other_log$trial=="s.landmarks_norm_log"]<-"landmarks_norm_log"
study_lm_wall_other_log$trial[study_lm_wall_other_log$trial=="s.wall_norm_log"]<-"wall_norm_log"
study_lm_wall_other_log$trial[study_lm_wall_other_log$trial=="s.other_norm_log"]<-"other_norm_log"
study_lm_wall_other_log$trial[study_lm_wall_other_log$trial=="s.objects_norm_log"]<-"objects_norm_log"

retrieval_lm_wall_other_log$trial <- as.character(retrieval_lm_wall_other_log$trial)
retrieval_lm_wall_other_log$trial[retrieval_lm_wall_other_log$trial=="r.landmarks_norm_log"]<-"landmarks_norm_log"
retrieval_lm_wall_other_log$trial[retrieval_lm_wall_other_log$trial=="r.wall_norm_log"]<-"wall_norm_log"
retrieval_lm_wall_other_log$trial[retrieval_lm_wall_other_log$trial=="r.other_norm_log"]<-"other_norm_log"
retrieval_lm_wall_other_log$trial[retrieval_lm_wall_other_log$trial=="r.objects_norm_log"]<-"objects_norm_log"

study_lm_wall_other_log$trial_type <- "Encoding"
retrieval_lm_wall_other_log$trial_type <- "Retrieval"
singlefixData <- rbind(study_lm_wall_other_log, retrieval_lm_wall_other_log)

# FIGURE FOR MANUSCRIPT

singlefixData$trial <- factor(singlefixData$trial, levels = c("objects_norm_log", "landmarks_norm_log", "wall_norm_log", "other_norm_log"))
bxp_singlefix <- ggboxplot(singlefixData, x = "trial", y = "norm_fixation_mean", group = "subject", color = "black", size = 0.25, 
                     add = "jitter", facet.by = "trial_type", add.params = list(size = 1.5)) +
  xlab("") +
  ylab("Mean of Log Normalized Fixations") +
  theme(legend.position = "none")+
  scale_x_discrete(breaks=c("landmarks_norm_log", "wall_norm_log", "other_norm_log", "objects_norm_log"),
                   labels=c("Landmarks", "Wall", "Other", "Object")) +
  theme(plot.title = element_text(hjust = 0.5))
jpeg("lmWallObject_anova.jpeg", width = 7, height = 5, units = 'in', res = 500)
bxp_singlefix
dev.off()

# make smaller dataframe for t-test graph
dosw_dodw_study <- study_subject_norm_long[c(481:510,541:570),]
dosw_dodw_retrieval <- retrieval_subject_norm_long[c(481:510,541:570),]

# need to make trial a character, not a factor or the renaming won't work
dosw_dodw_study$trial <- as.character(dosw_dodw_study$trial)
dosw_dodw_study$trial[dosw_dodw_study$trial=="s.DOSW_norm_log"]<-"DOSW_norm_log"
dosw_dodw_study$trial[dosw_dodw_study$trial=="s.DODW_norm_log"]<-"DODW_norm_log"

dosw_dodw_retrieval$trial <- as.character(dosw_dodw_retrieval$trial)
dosw_dodw_retrieval$trial[dosw_dodw_retrieval$trial=="r.DOSW_norm_log"]<-"DOSW_norm_log"
dosw_dodw_retrieval$trial[dosw_dodw_retrieval$trial=="r.DODW_norm_log"]<-"DODW_norm_log"

dosw_dodw_study$trial_type <- "Encoding"
dosw_dodw_retrieval$trial_type <- "Retrieval"
dosw_dodw_data <- rbind(dosw_dodw_study, dosw_dodw_retrieval)

# FIGURE FOR MANUSCRIPT
bxp_dosw_dodw <- ggboxplot(dosw_dodw_data, x = "trial", y = "norm_fixation_mean", group = "subject", color = "black", size = 0.25, 
                        add = "jitter", facet.by = "trial_type", add.params = list(size = 1.5)) +
  xlab("") +
  ylab("Mean of Log Normalized Fixations") +
  theme(legend.position = "none") +
  scale_x_discrete(breaks=c("DOSW_norm_log", "DODW_norm_log"),
                   labels=c("Same Wall", "Different Wall")) +
  theme(plot.title = element_text(hjust = 0.5))
jpeg("dosw_dodw_bxp.jpeg", width = 7, height = 5, units = 'in', res = 500)
bxp_dosw_dodw
dev.off()


# t-test for DOSW and DODW (both of these are not skewed so don't use the log transformed values)
t.test(subject_df$s.DOSW_norm , subject_df$s.DODW_norm , paired = TRUE, alternative = "two.sided") # sig < .001
ggpaired(subject_df, cond1 = "s.DOSW_norm", cond2 = "s.DODW_norm", ylab = "Fixations per Second") +
  ggtitle("Normalized Fixations During Study") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(breaks=c("s.DOSW_norm", "s.DODW_norm"),
                   labels=c("Same Wall", "Different Wall"))


t.test(subject_df$r.DOSW_norm , subject_df$r.DODW_norm , paired = TRUE, alternative = "two.sided") # sig < .001
ggpaired(subject_df, cond1 = "r.DOSW_norm", cond2 = "r.DODW_norm", ylab = "Fixations per Second") +
  ggtitle("Normalized Fixations During Retrieval") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(breaks=c("r.DOSW_norm", "r.DODW_norm"),
                   labels=c("Same Wall", "Different Wall"))

# ANOVA for successive fixations

#Old boxplot code - but I don't want to just delete it. What if I need it later?
#bxp_study_fixfix <- ggboxplot(study_successive_fix, x = "trial", y = "norm_fixation_mean", add = "point", 
#                                   title = "Log Normalized Fixation Number per Category during Study", 
#                                   xlab = "", ylab = "Mean of Log Normalized Fixations") +
#  scale_x_discrete(breaks=c("s.lm_obj_lm_norm", "s.lm_to_lm_norm", "s.obj_to_diffObj_norm"),
#                   labels=c("Landmark to Object", "Landmark to Landmark", "Object to Object"))

# boxplot for study alone
#bxp_study_fixfix <- ggline(study_successive_fix, x = "trial", y = "norm_fixation_mean", group = "subject", color = "black", 
#                           title = "Log Normalized Fixation Number per Category during Study", add = "boxplot") +
#  xlab("") +
#  ylab("Mean of Log Normalized Fixations") +
#  theme(legend.position = "none") +
#  scale_x_discrete(breaks=c("s.lm_obj_lm_norm", "s.lm_to_lm_norm", "s.obj_to_diffObj_norm"),
#                   labels=c("Landmark to Object", "Landmark to Landmark", "Object to Object"))

#bxp_study_fixfix


study_fixfix_aov <- anova_test(data = study_successive_fix, dv = norm_fixation_mean, wid = subject, within = trial)
get_anova_table(study_fixfix_aov) # sig

study_fixfix_pwc <- pairwise.t.test(study_successive_fix$norm_fixation_mean, study_successive_fix$trial, p.adj = "bonferroni", paired = TRUE)
study_fixfix_pwc # all sig diff from each other except obj->lm and lm->obj

t.test(subject_df$s.lm_to_lm_norm , subject_df$s.lm_obj_lm_norm , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.obj_to_diffObj_norm , subject_df$s.lm_obj_lm_norm , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.obj_to_diffObj_norm , subject_df$s.lm_to_lm_norm , paired = TRUE, alternative = "two.sided")

# boxplot for retrieval only
#bxp_retrieval_fixfix <- ggline(retrieval_successive_fix, x = "trial", y = "norm_fixation_mean", group = "subject", color = "black", 
#                           title = "Log Normalized Fixation Number per Category during Retrieval", add = "boxplot") +
#  xlab("") +
#  ylab("Mean of Log Normalized Fixations") +
#  theme(legend.position = "none") +
#  scale_x_discrete(breaks=c("r.lm_obj_lm_norm", "r.lm_to_lm_norm", "r.obj_to_diffObj_norm"),
#                   labels=c("Landmark to Object", "Landmark to Landmark", "Object to Object"))
#bxp_retrieval_fixfix

retrieval_fixfix_aov <- anova_test(data = retrieval_successive_fix, dv = norm_fixation_mean, wid = subject, within = trial)
get_anova_table(retrieval_fixfix_aov) # sig

retrieval_fixfix_pwc <- pairwise.t.test(retrieval_successive_fix$norm_fixation_mean, retrieval_successive_fix$trial, p.adj = "bonferroni", paired = TRUE)
retrieval_fixfix_pwc # about half comparisons are sig

t.test(subject_df$r.lm_to_lm_norm , subject_df$r.lm_obj_lm_norm , paired = TRUE, alternative = "two.sided")
t.test(subject_df$r.obj_to_diffObj_norm , subject_df$r.lm_obj_lm_norm , paired = TRUE, alternative = "two.sided")
t.test(subject_df$r.obj_to_diffObj_norm , subject_df$r.lm_to_lm_norm , paired = TRUE, alternative = "two.sided")

# need to make trial a character, not a factor or the renaming won't work
study_successive_fix$trial <- as.character(study_successive_fix$trial)
study_successive_fix$trial[study_successive_fix$trial=="s.obj_to_diffObj_norm"]<-"obj_to_diffObj_norm"
study_successive_fix$trial[study_successive_fix$trial=="s.lm_to_lm_norm"]<-"lm_to_lm_norm"
study_successive_fix$trial[study_successive_fix$trial=="s.lm_obj_lm_norm"]<-"lm_obj_lm_norm"

retrieval_successive_fix$trial <- as.character(retrieval_successive_fix$trial)
retrieval_successive_fix$trial[retrieval_successive_fix$trial=="r.obj_to_diffObj_norm"]<-"obj_to_diffObj_norm"
retrieval_successive_fix$trial[retrieval_successive_fix$trial=="r.lm_to_lm_norm"]<-"lm_to_lm_norm"
retrieval_successive_fix$trial[retrieval_successive_fix$trial=="r.lm_obj_lm_norm"]<-"lm_obj_lm_norm"

study_successive_fix$trial_type <- "Encoding"
retrieval_successive_fix$trial_type <- "Retrieval"
fixfixData <- rbind(study_successive_fix, retrieval_successive_fix)

# FIGURE FOR MANUSCRIPT
bxp_fixfix <- ggboxplot(fixfixData, x = "trial", y = "norm_fixation_mean", group = "subject", color = "black", size = 0.25,
                     add = "jitter", facet.by = "trial_type", add.params = list(size = 1.5)) +
  xlab("") +
  ylab("Mean of Log Normalized Fixations") +
  theme(legend.position = "none")+
  scale_x_discrete(breaks=c("lm_obj_lm_norm", "lm_to_lm_norm", "obj_to_diffObj_norm"),
                   labels=c("Landmark to Object", "Landmark to Landmark", "Object to Object")) +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust = 1), plot.title = element_text(hjust = 0.5))
jpeg("fixfix_anova.jpeg", width = 7, height = 6, units = 'in', res = 500)
bxp_fixfix
dev.off()

# figuring out average duration for encoding and retrieval
duration_data <- myData %>%
  dplyr::select("subject", "trial", "s.duration", "r.duration") %>%
  group_by(subject) %>%
  summarize(
    s.mean = mean(s.duration, na.rm = TRUE),
    r.mean = mean(r.duration, na.rm = TRUE)
  )

mean(duration_data$s.mean, na.rm = TRUE) # 34.38
mean(duration_data$r.mean, na.rm = TRUE) # 113.60





