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
setwd("D:/Nav_1stYr_project_data")

# Read in data
inputData <- read.csv("OA_dataByTrial_gazecode_manualCheck_nov8.csv")
inputData <- as.data.frame(inputData)
str(inputData) # check the structure of the data

# make a copy so there is also a clean copy (inputData)
myData <- inputData

# make all the rows their correct data type (numeric or factor)
i <- c(4:44)
myData [i] <- lapply(myData[i], as.numeric)
myData$group <- as.factor(myData$group)
myData$subject <- as.factor(myData$subject)
myData$trial <- as.factor(myData$trial)
myData$use_trial <- as.factor(myData$use_trial)

# Check the structure to make sure it looks good
str(myData)

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
  group_by(group,subject) %>%
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
    placement_error_cm_log = mean(placement_error_cm_log, na.rm = TRUE),
    abs_x_error_cm_log = mean(abs_x_error_cm_log, na.rm = TRUE),
    abs_y_error_cm_log = mean(abs_y_error_cm_log, na.rm = TRUE), 
    fix_num = mean(fix_num, na.rm = TRUE), 
    total_fix_dur_ms = mean(total_fix_dur_ms, na.rm = TRUE),
    avg_fix_dur_ms = mean(avg_fix_dur_ms, na.rm = TRUE)
  )

# create dataset for study normed and logged values by subject
study_subject_norm_df <- myData %>%
  group_by(group,subject) %>%
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
    placement_error_cm = mean(placement_error_cm, na.rm = TRUE),
    abs_x_error_cm = mean(abs_x_error_cm, na.rm = TRUE),
    abs_y_error_cm = mean(abs_y_error_cm, na.rm = TRUE),
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
    placement_error_cm_log = mean(placement_error_cm_log, na.rm = TRUE),
    abs_x_error_cm_log = mean(abs_x_error_cm_log, na.rm = TRUE),
    abs_y_error_cm_log = mean(abs_y_error_cm_log, na.rm = TRUE),
    fix_num = mean(fix_num, na.rm = TRUE), 
    total_fix_dur_ms = mean(total_fix_dur_ms, na.rm = TRUE),
    avg_fix_dur_ms = mean(avg_fix_dur_ms, na.rm = TRUE)
  )

# create dataset for retrieval normed and logged values by subject
retrieval_subject_norm_df <- myData %>%
  group_by(group,subject) %>%
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
    placement_error_cm = mean(placement_error_cm, na.rm = TRUE),
    abs_x_error_cm = mean(abs_x_error_cm, na.rm = TRUE),
    abs_y_error_cm = mean(abs_y_error_cm, na.rm = TRUE),
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
    placement_error_cm_log = mean(placement_error_cm_log, na.rm = TRUE),
    abs_x_error_cm_log = mean(abs_x_error_cm_log, na.rm = TRUE),
    abs_y_error_cm_log = mean(abs_y_error_cm_log, na.rm = TRUE),
    fix_num = mean(fix_num, na.rm = TRUE), 
    total_fix_dur_ms = mean(total_fix_dur_ms, na.rm = TRUE),
    avg_fix_dur_ms = mean(avg_fix_dur_ms, na.rm = TRUE)
  )

# put the study subject grouped data in long format
study_subject_norm_long <- study_subject_norm_df %>%
  gather(key = "trial", value = "norm_fixation_mean", s.landmarks_norm, s.same_object_norm, s.DOSW_norm, s.wall_norm, s.DODW_norm, s.cart_norm, s.other_norm,
         s.obj_to_lm_norm, s.lm_to_obj_norm, s.obj_to_so_norm, s.obj_to_diffObj_norm, s.lm_to_lm_norm, s.lm_obj_lm_norm, s.objects_norm,
         s.landmarks_norm_log, s.same_object_norm_log, s.DOSW_norm_log, s.wall_norm_log, s.DODW_norm_log, s.cart_norm_log, s.other_norm_log, 
         s.obj_to_lm_norm_log, s.lm_to_obj_norm_log, s.obj_to_so_norm_log, s.obj_to_diffObj_norm_log, s.lm_to_lm_norm_log, s.lm_obj_lm_norm_log, s.objects_norm_log) %>%
  convert_as_factor(group,subject,trial)
  
# put the retrieval subject grouped data in long format
retrieval_subject_norm_long <- retrieval_subject_norm_df %>%
  gather(key = "trial", value = "norm_fixation_mean", r.landmarks_norm, r.same_object_norm, r.DOSW_norm, r.wall_norm, r.DODW_norm, r.cart_norm, r.other_norm,
         r.obj_to_lm_norm, r.lm_to_obj_norm, r.obj_to_so_norm, r.obj_to_diffObj_norm, r.lm_to_lm_norm, r.lm_obj_lm_norm, r.objects_norm, 
         r.landmarks_norm_log, r.same_object_norm_log, r.DOSW_norm_log, r.wall_norm_log, r.DODW_norm_log, r.cart_norm_log, r.other_norm_log, 
         r.obj_to_lm_norm_log, r.lm_to_obj_norm_log, r.obj_to_so_norm_log, r.obj_to_diffObj_norm_log, r.lm_to_lm_norm_log, r.lm_obj_lm_norm_log, r.objects_norm_log) %>%
  convert_as_factor(group,subject,trial)

# filter data into norm and norm_log values
study_only_norm_long <- study_subject_norm_long[1:882, ]
study_only_norm_log_long <- study_subject_norm_long[883:1764, ]

retrieval_only_norm_long <- retrieval_subject_norm_long[1:882, ] 
retreival_only_norm_log_long <- retrieval_subject_norm_long[883:1764, ] 

# make group for only LM, Wall, Other, and Objects categories during study and retrieval separately
study_lm_wall_other <- study_only_norm_long[c(1:63, 190:252, 379:441, 820:882), ]
study_lm_wall_other_log <- study_only_norm_log_long[c(1:63, 190:252, 379:441, 820:882), ]

retrieval_lm_wall_other <- retrieval_only_norm_long[c(1:63, 190:252, 379:441, 820:882), ]
retrieval_lm_wall_other_log <- retreival_only_norm_log_long[c(1:63, 190:252, 379:441, 820:882), ]

# make a group for successive fixations: obj_diffObj, lm_lm, lm_obj_lm,
study_successive_fix <- study_only_norm_long[631:819, ]
retrieval_successive_fix <- retrieval_only_norm_long[631:819, ] # fixed for OAs

### check for skew - significant values noted, otherwise not significant
shapiro.test(subject_df$s.landmarks_norm) 
shapiro.test(subject_df$s.same_object_norm_log)
shapiro.test(subject_df$s.DOSW_norm_log)
shapiro.test(subject_df$s.wall_norm_log)
shapiro.test(subject_df$s.DODW_norm_log)
shapiro.test(subject_df$s.cart_norm_log)
shapiro.test(subject_df$s.other_norm_log) 
shapiro.test(subject_df$s.obj_to_lm_norm_log)
shapiro.test(subject_df$s.lm_to_obj_norm_log) 
shapiro.test(subject_df$s.obj_to_so_norm_log) 
shapiro.test(subject_df$s.obj_to_diffObj_norm_log)
shapiro.test(subject_df$s.lm_to_lm_norm_log) 
shapiro.test(subject_df$s.lm_obj_lm_norm_log)
shapiro.test(subject_df$s.objects_norm_log)

shapiro.test(subject_df$r.landmarks_norm)
shapiro.test(subject_df$r.same_object_norm_log)
shapiro.test(subject_df$r.DOSW_norm_log)
shapiro.test(subject_df$r.wall_norm_log)
shapiro.test(subject_df$r.DODW_norm_log)
shapiro.test(subject_df$r.cart_norm_log)
shapiro.test(subject_df$r.other_norm_log)
shapiro.test(subject_df$r.obj_to_lm_norm_log)
shapiro.test(subject_df$r.lm_to_obj_norm_log)
shapiro.test(subject_df$r.obj_to_so_norm_log)
shapiro.test(subject_df$r.obj_to_diffObj_norm_log)
shapiro.test(subject_df$r.lm_to_lm_norm_log)
shapiro.test(subject_df$r.lm_obj_lm_norm_log)
shapiro.test(subject_df$r.objects_norm_log)

### test study against test in each category 
t.test(subject_df$s.landmarks_norm_log, subject_df$r.landmarks_norm_log, paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.same_object_norm_log, subject_df$r.same_object_norm_log, paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.DOSW_norm_log , subject_df$r.DOSW_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.wall_norm_log , subject_df$r.wall_norm_log , paired = TRUE, alternative = "two.sided") # not sig
t.test(subject_df$s.DODW_norm_log , subject_df$r.DODW_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.other_norm_log , subject_df$r.other_norm_log , paired = TRUE, alternative = "two.sided") 
t.test(subject_df$s.obj_to_lm_norm_log , subject_df$r.obj_to_lm_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.lm_to_obj_norm_log , subject_df$r.lm_to_obj_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.obj_to_so_norm_log , subject_df$r.obj_to_so_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.obj_to_diffObj_norm_log , subject_df$r.obj_to_diffObj_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.lm_to_lm_norm_log , subject_df$r.lm_to_lm_norm_log , paired = TRUE, alternative = "two.sided") 
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
                       color = "group",
                       title = "Normalized Fixation Number per Category during Study", 
                       xlab = "", ylab = "Mean of Normalized Fixations") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), plot.title = element_text(hjust = 0.5))
study_bxp

# make a boxplot of just the study categories - norm log values
study_log_bxp <- ggboxplot(study_only_norm_log_long, x = "trial", y = "norm_fixation_mean", add = "point", 
                           color = "group",
                           title = "Log Normalized Fixation Number per Category during Study",
                           xlab = "", ylab = "Mean of Log Normalized Fixations") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), plot.title = element_text(hjust = 0.5))
study_log_bxp # same pattern as just normalized

# make a boxplot of just the retrieval categories - norm values
retrieval_bxp <- ggboxplot(retrieval_only_norm_long, x = "trial", y = "norm_fixation_mean", add = "point", 
                           color = "group",
                           title = "Normalized Fixation Number per Category during Retrieval",
                           xlab = "", ylab = "Mean of Normalized Fixations") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), plot.title = element_text(hjust = 0.5))
retrieval_bxp

# make a boxplot of just the retrieval categories - norm log values
retrieval_log_bxp <- ggboxplot(retreival_only_norm_log_long, x = "trial", y = "norm_fixation_mean", add = "point", 
                               color = "group",
                               title = "Log Normalized Fixation Number per Category during Retrieval", 
                               xlab = "", ylab = "Mean of Log Normalized Fixations") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), plot.title = element_text(hjust = 0.5))
retrieval_log_bxp

# make plots for OAs and YAs


# big ANOVAs for study and retrieval - fix these later
study_only_norm_long$group_subj <- paste0(study_only_norm_long$group, study_only_norm_long$subject)
study_only_norm_long <- as.data.frame(study_only_norm_long)
study_only_norm_log_long$group_subj <- paste0(study_only_norm_log_long$group, study_only_norm_log_long$subject)
study_only_norm_log_long <- as.data.frame(study_only_norm_log_long)

study_norm_aov <- anova_test(data = study_only_norm_long, dv = norm_fixation_mean, wid = group_subj, within = trial, between = group)
get_anova_table(study_norm_aov) # won't work

study_norm_pwc <- pairwise.t.test(study_only_norm_long$norm_fixation_mean, study_only_norm_long$trial, p.adj = "bonferroni", paired = TRUE)
study_norm_pwc

study_norm_log_aov <- anova_test(data = study_only_norm_log_long, dv = norm_fixation_mean, wid = group_subj, within = trial)
get_anova_table(study_norm_log_aov) # won't work

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

# make separated data frames for the two groups
oa_data <- subject_df %>%
  filter(group == "OA")

ya_data <- subject_df %>%
  filter(group == "YA")

# oa correlations with performance - literally nothing is sig
cor.test(oa_data$s.landmarks_norm_log, oa_data$placement_error_cm_log, method = "pearson")
cor.test(oa_data$s.same_object_norm_log, oa_data$placement_error_cm_log, method = "pearson")
cor.test(oa_data$s.DOSW_norm_log, oa_data$placement_error_cm_log, method = "pearson")
cor.test(oa_data$s.wall_norm_log, oa_data$placement_error_cm_log, method = "pearson")
cor.test(oa_data$s.DODW_norm_log, oa_data$placement_error_cm_log, method = "pearson")
cor.test(oa_data$s.cart_norm_log, oa_data$placement_error_cm_log, method = "pearson")
cor.test(oa_data$s.other_norm_log, oa_data$placement_error_cm_log, method = "pearson")
cor.test(oa_data$s.obj_to_lm_norm_log, oa_data$placement_error_cm_log, method = "pearson")
cor.test(oa_data$s.lm_to_obj_norm_log, oa_data$placement_error_cm_log, method = "pearson")
cor.test(oa_data$s.obj_to_so_norm_log, oa_data$placement_error_cm_log, method = "pearson")
cor.test(oa_data$s.obj_to_diffObj_norm_log, oa_data$placement_error_cm_log, method = "pearson")
cor.test(oa_data$s.lm_to_lm_norm_log, oa_data$placement_error_cm_log, method = "pearson")
cor.test(oa_data$s.lm_obj_lm_norm_log, oa_data$placement_error_cm_log, method = "pearson")
cor.test(oa_data$s.objects_norm_log, oa_data$placement_error_cm_log, method = "pearson")

cor.test(oa_data$r.same_object_norm_log, oa_data$placement_error_cm_log, method = "pearson")
cor.test(oa_data$r.DOSW_norm_log, oa_data$placement_error_cm_log, method = "pearson")
cor.test(oa_data$r.wall_norm_log, oa_data$placement_error_cm_log, method = "pearson")
cor.test(oa_data$r.DODW_norm_log, oa_data$placement_error_cm_log, method = "pearson")
cor.test(oa_data$r.other_norm_log, oa_data$placement_error_cm_log, method = "pearson")
cor.test(oa_data$r.obj_to_lm_norm_log, oa_data$placement_error_cm_log, method = "pearson") 
cor.test(oa_data$r.lm_to_obj_norm_log, oa_data$placement_error_cm_log, method = "pearson") 
cor.test(oa_data$r.obj_to_so_norm_log, oa_data$placement_error_cm_log, method = "pearson")
cor.test(oa_data$r.obj_to_diffObj_norm_log, oa_data$placement_error_cm_log, method = "pearson")
cor.test(oa_data$r.lm_to_lm_norm_log, oa_data$placement_error_cm_log, method = "pearson")
cor.test(oa_data$r.lm_obj_lm_norm_log, oa_data$placement_error_cm_log, method = "pearson")
cor.test(oa_data$r.objects_norm_log, oa_data$placement_error_cm_log, method = "pearson")

cor.test(oa_data$total_fix_dur_ms, oa_data$placement_error_cm_log, method = "pearson")
cor.test(oa_data$avg_fix_dur_ms, oa_data$placement_error_cm_log, method = "pearson")
cor.test(oa_data$fix_num, oa_data$placement_error_cm_log, method = "pearson")

# ya correlations with performance
cor.test(ya_data$s.landmarks_norm_log, ya_data$placement_error_cm_log, method = "pearson")
cor.test(ya_data$s.same_object_norm_log, ya_data$placement_error_cm_log, method = "pearson") # sig
cor.test(ya_data$s.DOSW_norm_log, ya_data$placement_error_cm_log, method = "pearson")
cor.test(ya_data$s.wall_norm_log, ya_data$placement_error_cm_log, method = "pearson")
cor.test(ya_data$s.DODW_norm_log, ya_data$placement_error_cm_log, method = "pearson")
cor.test(ya_data$s.cart_norm_log, ya_data$placement_error_cm_log, method = "pearson")
cor.test(ya_data$s.other_norm_log, ya_data$placement_error_cm_log, method = "pearson")
cor.test(ya_data$s.obj_to_lm_norm_log, ya_data$placement_error_cm_log, method = "pearson")
cor.test(ya_data$s.lm_to_obj_norm_log, ya_data$placement_error_cm_log, method = "pearson")
cor.test(ya_data$s.obj_to_so_norm_log, ya_data$placement_error_cm_log, method = "pearson") # sig
cor.test(ya_data$s.obj_to_diffObj_norm_log, ya_data$placement_error_cm_log, method = "pearson")
cor.test(ya_data$s.lm_to_lm_norm_log, ya_data$placement_error_cm_log, method = "pearson")
cor.test(ya_data$s.lm_obj_lm_norm_log, ya_data$placement_error_cm_log, method = "pearson")
cor.test(ya_data$s.objects_norm_log, ya_data$placement_error_cm_log, method = "pearson")

cor.test(ya_data$r.same_object_norm_log, ya_data$placement_error_cm_log, method = "pearson") # sig
cor.test(ya_data$r.DOSW_norm_log, ya_data$placement_error_cm_log, method = "pearson")
cor.test(ya_data$r.wall_norm_log, ya_data$placement_error_cm_log, method = "pearson")
cor.test(ya_data$r.DODW_norm_log, ya_data$placement_error_cm_log, method = "pearson")
cor.test(ya_data$r.other_norm_log, ya_data$placement_error_cm_log, method = "pearson")
cor.test(ya_data$r.obj_to_lm_norm_log, ya_data$placement_error_cm_log, method = "pearson") 
cor.test(ya_data$r.lm_to_obj_norm_log, ya_data$placement_error_cm_log, method = "pearson") 
cor.test(ya_data$r.obj_to_so_norm_log, ya_data$placement_error_cm_log, method = "pearson") # sig
cor.test(ya_data$r.obj_to_diffObj_norm_log, ya_data$placement_error_cm_log, method = "pearson")
cor.test(ya_data$r.lm_to_lm_norm_log, ya_data$placement_error_cm_log, method = "pearson")
cor.test(ya_data$r.lm_obj_lm_norm_log, ya_data$placement_error_cm_log, method = "pearson")
cor.test(ya_data$r.objects_norm_log, ya_data$placement_error_cm_log, method = "pearson") # sig

cor.test(ya_data$total_fix_dur_ms, ya_data$placement_error_cm_log, method = "pearson") # marginal
cor.test(ya_data$avg_fix_dur_ms, ya_data$placement_error_cm_log, method = "pearson") # sig
cor.test(ya_data$fix_num, ya_data$placement_error_cm_log, method = "pearson") # not sig

# all together correlations with performance 
cor.test(subject_df$s.landmarks_norm_log, subject_df$placement_error_cm_log, method = "pearson") # sig
cor.test(subject_df$s.same_object_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.DOSW_norm_log, subject_df$placement_error_cm_log, method = "pearson") # sig
cor.test(subject_df$s.wall_norm_log, subject_df$placement_error_cm_log, method = "pearson") # sig
cor.test(subject_df$s.DODW_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.cart_norm_log, subject_df$placement_error_cm_log, method = "pearson") # sig
cor.test(subject_df$s.other_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.obj_to_lm_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.lm_to_obj_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.obj_to_so_norm_log, subject_df$placement_error_cm_log, method = "pearson") # sig
cor.test(subject_df$s.obj_to_diffObj_norm_log, subject_df$placement_error_cm_log, method = "pearson") # sig
cor.test(subject_df$s.lm_to_lm_norm_log, subject_df$placement_error_cm_log, method = "pearson") # sig
cor.test(subject_df$s.lm_obj_lm_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$s.objects_norm_log, subject_df$placement_error_cm_log, method = "pearson") # sig

cor.test(subject_df$r.same_object_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$r.DOSW_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$r.wall_norm_log, subject_df$placement_error_cm_log, method = "pearson") # sig
cor.test(subject_df$r.DODW_norm_log, subject_df$placement_error_cm_log, method = "pearson") # barely just sig
cor.test(subject_df$r.other_norm_log, subject_df$placement_error_cm_log, method = "pearson") # sig
cor.test(subject_df$r.obj_to_lm_norm_log, subject_df$placement_error_cm_log, method = "pearson") # sig
cor.test(subject_df$r.lm_to_obj_norm_log, subject_df$placement_error_cm_log, method = "pearson")  # sig
cor.test(subject_df$r.obj_to_so_norm_log, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$r.obj_to_diffObj_norm_log, subject_df$placement_error_cm_log, method = "pearson") # sig
cor.test(subject_df$r.lm_to_lm_norm_log, subject_df$placement_error_cm_log, method = "pearson") # sig
cor.test(subject_df$r.lm_obj_lm_norm_log, subject_df$placement_error_cm_log, method = "pearson")# sig
cor.test(subject_df$r.objects_norm_log, subject_df$placement_error_cm_log, method = "pearson")

cor.test(subject_df$total_fix_dur_ms, subject_df$placement_error_cm_log, method = "pearson") # sig
cor.test(subject_df$avg_fix_dur_ms, subject_df$placement_error_cm_log, method = "pearson")
cor.test(subject_df$fix_num, subject_df$placement_error_cm_log, method = "pearson") # sig

plot(subject_df$fix_num, subject_df$placement_error_cm_log)

subject_df <- as.data.frame(subject_df)

plot(oa_data$fix_num, oa_data$placement_error_cm_log)
plot(ya_data$fix_num, ya_data$placement_error_cm_log)

plot(subject_df$total_fix_dur_ms, subject_df$placement_error_cm_log) + stat_cor(method = "pearson", label.x = 600, label.y = 3.8)
plot(subject_df$avg_fix_dur_ms, subject_df$placement_error_cm_log)


x1 <- oa_data$avg_fix_dur_ms
y1 <- oa_data$placement_error_cm_log

x2 <- ya_data$avg_fix_dur_ms
y2 <- ya_data$placement_error_cm_log

# FIGURE FOR MANUSCRIPT

avg_fix_dur <-ggplot(oa_data, aes( x=x1, y=y1 )) +
  geom_point()+
  stat_cor(method = "pearson", label.x = 600, label.y = 4.5) +
  theme_classic() + xlab("Average duration of fixations (ms)") +
  ylab("Mean Placement Error (log cm)") + 
  geom_smooth(method = 'lm')
#jpeg("OA_avg_fix_dur.jpeg", width = 5, height = 5, units = 'in', res = 500)
avg_fix_dur
#dev.off()

x <- oa_data$total_fix_dur_ms
y<- oa_data$placement_error_cm_log

tot_fix_dur <-ggplot(oa_data, aes( x=x, y=y ))+
  geom_point()+
  stat_cor(method = "pearson", label.x = 1200, label.y = 4.4) +
  theme_classic() + xlab("Total duration of fixations (ms)") +
  ylab("Mean Placement Error (log cm)") + 
  geom_smooth(method = 'lm')
#jpeg("OA_total_fix_dur.jpeg", width = 5, height = 5, units = 'in', res = 500)
tot_fix_dur
#dev.off()

x <- oa_data$fix_num
y<- oa_data$placement_error_cm_log

fix_num_plot <-ggplot(oa_data, aes( x=x, y=y ))+
  geom_point()+
  stat_cor(method = "pearson", label.x = 2, label.y = 4.4) +
  theme_classic() + xlab("Average Number of Fixations") +
  ylab("Mean Placement Error (log cm)") + 
  geom_smooth(method = 'lm')
#jpeg("OA_avg_fix_num.jpeg", width = 5, height = 5, units = 'in', res = 500)
fix_num_plot
#dev.off()


# big regression with all sig ones to see which explains more variance with time to first fixation

big_reg_norm <- lm(formula = placement_error_cm_log ~ group + s.landmarks_norm_log + s.objects_norm_log + s.DOSW_norm_log + s.wall_norm_log + s.DODW_norm_log +
                s.other_norm_log + s.lm_obj_lm_norm_log + s.obj_to_so_norm_log + s.obj_to_diffObj_norm_log + s.lm_to_lm_norm_log +
                r.landmarks_norm_log + r.objects_norm_log + r.DOSW_norm_log + r.wall_norm_log + r.DODW_norm_log + r.other_norm_log +
                r.lm_obj_lm_norm_log + r.obj_to_so_norm_log + r.obj_to_diffObj_norm_log + r.lm_to_lm_norm_log +
                fix_num + total_fix_dur_ms + avg_fix_dur_ms, data = subject_df)
summary(big_reg_norm) # sig terms: r.landmarks_norm_log, r.lm_obj_lm, r.lm_to_lm

s_reg_norm <- lm(formula = placement_error_cm_log ~ group + s.landmarks_norm_log + s.objects_norm_log + s.DOSW_norm_log + s.wall_norm_log + s.DODW_norm_log +
                     s.other_norm_log + s.lm_obj_lm_norm_log + s.obj_to_so_norm_log + s.obj_to_diffObj_norm_log + s.lm_to_lm_norm_log +
                     fix_num + total_fix_dur_ms + avg_fix_dur_ms, data = subject_df)
summary(s_reg_norm) # overall sig but only intercept is sig

big_reg_terms <- subject_df %>%
  dplyr::select("s.landmarks_norm_log", "s.objects_norm_log", "s.DOSW_norm_log", "s.DODW_norm_log",
                "s.lm_obj_lm_norm_log", "s.obj_to_so_norm_log", "s.obj_to_diffObj_norm_log", "s.lm_to_lm_norm_log",
                "fix_num", "total_fix_dur_ms", "avg_fix_dur_ms")
### more regressions with less terms
# first, take out all retrieval terms
# then, take out the terms that don't really matter (wall, other, etc.)
# then, take out all multicollinear terms (above .5)

cor(big_reg_terms, use = "complete.obs")
pairs(big_reg_terms)

small_reg_terms <- subject_df %>%
  dplyr::select("s.landmarks_norm_log", "s.DOSW_norm_log", "s.DODW_norm_log", "avg_fix_dur_ms")

cor(small_reg_terms, use = "complete.obs")
pairs(small_reg_terms)

# this is the correct regression
small_reg_norm <- lm(formula = placement_error_cm_log ~ group + s.landmarks_norm_log + s.DOSW_norm_log + s.DODW_norm_log + avg_fix_dur_ms, data = subject_df)
summary(small_reg_norm) # overall sig and only group is sig

### conceptual ANOVAs and t-tests and figures

# ANOVA for LM, wall, objects, and other for study and retrieval separately - get working later
study_lm_wall_other_log$group_subj <- paste0(study_lm_wall_other_log$group, study_lm_wall_other_log$subject)
retrieval_lm_wall_other_log$group_subj <- paste0(retrieval_lm_wall_other_log$group, retrieval_lm_wall_other_log$subject)

study_lmWallOther_aov <- anova_test(data = study_lm_wall_other_log, dv = norm_fixation_mean, wid = group_subj, within = trial, between = group)
get_anova_table(study_lmWallOther_aov) 

study_lmWallOther_pwc <- pairwise.t.test(study_lm_wall_other_log$norm_fixation_mean, study_lm_wall_other_log$trial, p.adj = "bonferroni", paired = TRUE)
study_lmWallOther_pwc 

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

# FIGURE FOR MANUSCRIPT - Graph OA and YA object, landmark, wall, and other
# Ensure levels are in the order I want
singlefixData$trial <- factor(singlefixData$trial, levels = c("objects_norm_log", "landmarks_norm_log", "wall_norm_log", "other_norm_log"))

oaya_lm_obj_wall_oth_plot <- ggplot(singlefixData, aes(x = trial, y = norm_fixation_mean, fill = group)) +
  geom_boxplot(outliers = FALSE) + geom_jitter(position = position_jitterdodge()) +
  stat_summary(aes(group = group), fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  labs(x = "", y = "Mean Log Norm Fixations", fill = "Group") +
  scale_x_discrete(breaks=c("landmarks_norm_log", "wall_norm_log", "other_norm_log", "objects_norm_log"),
                   labels=c("Landmarks", "Wall", "Other", "Object")) +
  scale_fill_discrete(name = "Group", labels = c("Older Adult", "Younger Adult"), type = c("#619CFF", "#00BA38")) +
  theme_classic() +
  facet_wrap(vars(trial_type)) +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

#jpeg("D:/Nav Stress Data/dissertation/pics/oaya_lm_obj_wall_oth_plot.jpeg", width = 10, height = 6, units = 'in', res = 500)
oaya_lm_obj_wall_oth_plot
#dev.off()

# ANOVA
singlefixData <- as.data.frame(singlefixData)
singlefixData$group_subj <- paste0(singlefixData$group, singlefixData$subject)

res.aov <- anova_test(data = singlefixData, dv = norm_fixation_mean, wid = group_subj, 
                      within = c(trial, trial_type), between = group)
get_anova_table(res.aov) # everything is sig

# make smaller dataframe for t-test graph
dosw_dodw_study <- study_subject_norm_long[c(1009:1071,1135:1197),]
dosw_dodw_retrieval <- retrieval_subject_norm_long[c(1009:1071,1135:1197),]

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
# Ensure 'Same Wall' is on the left and 'Different Wall' on the right
dosw_dodw_data$trial <- factor(dosw_dodw_data$trial, levels = c("DOSW_norm_log", "DODW_norm_log"))

dosw_dodw_plot <- ggplot(dosw_dodw_data, aes(x = trial, y = norm_fixation_mean, fill = group)) +
  geom_boxplot(outliers = FALSE) + geom_jitter(position = position_jitterdodge()) +
  stat_summary(aes(group = group), fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  labs(x = "", y = "Mean Log Norm Fixations", fill = "Group") +
  scale_x_discrete(breaks = c("DOSW_norm_log", "DODW_norm_log"),
                   labels = c("Same Wall", "Different Wall")) +
  scale_fill_discrete(name = "Group", labels = c("Older Adult", "Younger Adult"), type = c("#619CFF", "#00BA38")) +
  theme_classic() +
  facet_wrap(vars(trial_type)) +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))


#jpeg("D:/Nav Stress Data/dissertation/pics/dosw_dodw_plot.jpeg", width = 10, height = 6, units = 'in', res = 500)
dosw_dodw_plot
#dev.off()

# ANOVA
dosw_dodw_data <- as.data.frame(dosw_dodw_data)
dosw_dodw_data$group_subj <- paste0(dosw_dodw_data$group, dosw_dodw_data$subject)

wall.aov <- anova_test(data = dosw_dodw_data, dv = norm_fixation_mean, wid = group_subj, 
                      within = c(trial, trial_type), between = group)
get_anova_table(wall.aov) # everything is sig


study_fixfix_aov <- anova_test(data = study_successive_fix, dv = norm_fixation_mean, wid = subject, within = trial)
get_anova_table(study_fixfix_aov) # sig

study_fixfix_pwc <- pairwise.t.test(study_successive_fix$norm_fixation_mean, study_successive_fix$trial, p.adj = "bonferroni", paired = TRUE)
study_fixfix_pwc # all sig diff from each other except obj->lm and lm->obj

t.test(subject_df$s.lm_to_lm_norm , subject_df$s.lm_obj_lm_norm , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.obj_to_diffObj_norm , subject_df$s.lm_obj_lm_norm , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.obj_to_diffObj_norm , subject_df$s.lm_to_lm_norm , paired = TRUE, alternative = "two.sided")



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
fixfixData$trial <- factor(fixfixData$trial, levels = c("obj_to_diffObj_norm", "lm_obj_lm_norm", "lm_to_lm_norm"))

successive_fix_plot <- ggplot(fixfixData, aes(x = trial, y = norm_fixation_mean, fill = group)) +
  geom_boxplot(outliers = FALSE) + geom_jitter(position = position_jitterdodge()) +
  stat_summary(aes(group = group), fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  labs(x = "", y = "Mean Log Norm Fixations", fill = "Group") +
  scale_x_discrete(breaks = c("obj_to_diffObj_norm", "lm_obj_lm_norm", "lm_to_lm_norm"),
                   labels = c("Object to Object", "Landmark to Object", "Landmark to Landmark")) +
  scale_fill_discrete(name = "Group", labels = c("Older Adult", "Younger Adult"), type = c("#619CFF", "#00BA38")) +
  theme_classic() +
  facet_wrap(vars(trial_type)) +
  theme(axis.text.x = element_text(size = 13, angle = 15, vjust = 1, hjust = 1), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1))


#jpeg("D:/Nav Stress Data/dissertation/pics/successive_fix_plot.jpeg", width = 10, height = 6, units = 'in', res = 500)
successive_fix_plot
#dev.off()

# another ANOVA
fixfixData <- as.data.frame(fixfixData)
fixfixData$group_subj <- paste0(fixfixData$group, fixfixData$subject)

fixfix.aov <- anova_test(data = fixfixData, dv = norm_fixation_mean, wid = group_subj, 
                       within = c(trial, trial_type), between = group)
get_anova_table(fixfix.aov) # everything is sig

# figuring out average duration for encoding and retrieval
duration_data <- myData %>%
  dplyr::select("group","subject", "trial", "s.duration", "r.duration", "fix_num", "total_fix_dur_ms", "avg_fix_dur_ms") %>%
  group_by(group, subject) %>%
  summarize(
    s.mean = mean(s.duration, na.rm = TRUE),
    r.mean = mean(r.duration, na.rm = TRUE),
    avg_fix_num = mean(fix_num, na.rm = TRUE),
    total_fix_dur = mean(total_fix_dur_ms, na.rm = TRUE),
    avg_fix_dur = mean(avg_fix_dur_ms, na.rm = TRUE)
  )

duration_means <- duration_data %>%
  group_by(group) %>%
  summarize(
    count = n(),
    mean_study_dur = mean(s.mean, na.rm = TRUE),
    sd_study_dur = sd(s.mean, na.rm = TRUE),
    mean_retrieval_dur = mean(r.mean, na.rm = TRUE), 
    sd_retrieval_dur = sd(r.mean, na.rm = TRUE),
    mean_fix_num = mean(avg_fix_num, na.rm = TRUE), 
    sd_fix_num = sd(avg_fix_num, na.rm = TRUE),
    mean_total_fix_dur = mean(total_fix_dur, na.rm = TRUE), 
    sd_total_fix_dur = sd(total_fix_dur, na.rm = TRUE),
    mean_avg_fix_dur = mean(avg_fix_dur, na.rm = TRUE), 
    sd_avg_fix_dur = sd(avg_fix_dur, na.rm = TRUE),
  )

# Tests that show both groups had the same study and retrieval times
t.test(s.mean ~ group, data = duration_data) # not sig
t.test(r.mean ~ group, data = duration_data) # not sig


