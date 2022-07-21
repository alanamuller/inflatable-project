# Data file used for the manuscript results section
# Alana Muller
# amuller@arizona.edu
# 2022-07-11

rm(list = ls())

library(readxl)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyverse)

# work computer uses E but laptop uses D, change accordingly
setwd("D:/Nav_1stYr_project_data/GazeCode data")

# Read in data
inputData <- read_excel("D:/Nav_1stYr_project_data/GazeCode data/R_outputs/manuscript_data_N29_gazecode_byTrial_output_paste.xlsx")
inputData <- as.data.frame(inputData)
str(inputData) # check the structure of the data

myData <- inputData

i <- c(1:55)
myData [ , i] <- apply(myData, 2, function(x) as.numeric(x))
myData$subject <- as.factor(myData$subject)
myData$trial <- as.factor(myData$trial)
str(myData)

myData$s.landmarks_norm <- myData$s.landmarks/myData$s.duration
myData$s.same_object_norm <- myData$s.same_object/myData$s.duration
myData$s.DOSW_norm <- myData$s.DOSW/myData$s.duration
myData$s.other_norm <- myData$s.other/myData$s.duration
myData$s.DODW_norm <- myData$s.DODW/myData$s.duration
myData$s.obj_to_lm_norm <- myData$s.obj_to_lm/myData$s.duration
myData$s.lm_to_obj_norm <- myData$s.lm_to_obj/myData$s.duration
myData$s.obj_to_so_norm <- myData$s.obj_to_so/myData$s.duration
myData$s.obj_to_diffObj_norm <- myData$s.obj_to_diffObj/myData$s.duration
myData$s.lm_to_lm_norm <- myData$s.lm_to_lm/myData$s.duration

myData$r.landmarks_norm <- myData$r.landmarks/myData$r.duration
myData$r.same_object_norm <- myData$r.same_object/myData$r.duration
myData$r.DOSW_norm <- myData$r.DOSW/myData$r.duration
myData$r.other_norm <- myData$r.other/myData$r.duration
myData$r.DODW_norm <- myData$r.DODW/myData$r.duration
myData$r.obj_to_lm_norm <- myData$r.obj_to_lm/myData$r.duration
myData$r.lm_to_obj_norm <- myData$r.lm_to_obj/myData$r.duration
myData$r.obj_to_so_norm <- myData$r.obj_to_so/myData$r.duration
myData$r.obj_to_diffObj_norm <- myData$r.obj_to_diffObj/myData$r.duration
myData$r.lm_to_lm_norm <- myData$r.lm_to_lm/myData$r.duration


myData$s.landmarks_norm_log <- log(myData$s.landmarks_norm+1)
myData$s.same_object_norm_log <- log(myData$s.same_object_norm+1)
myData$s.DOSW_norm_log <- log(myData$s.DOSW_norm+1)
myData$s.other_norm_log <- log(myData$s.other_norm+1)
myData$s.DODW_norm_log <- log(myData$s.DODW_norm+1)
myData$s.obj_to_lm_norm_log <- log(myData$s.obj_to_lm_norm+1)
myData$s.lm_to_obj_norm_log <- log(myData$s.lm_to_obj_norm+1)
myData$s.obj_to_so_norm_log <- log(myData$s.obj_to_so_norm+1)
myData$s.obj_to_diffObj_norm_log <- log(myData$s.obj_to_diffObj_norm+1)
myData$s.lm_to_lm_norm_log <- log(myData$s.lm_to_lm_norm+1)

myData$r.landmarks_norm_log <- log(myData$r.landmarks_norm+1)
myData$r.same_object_norm_log <- log(myData$r.same_object_norm+1)
myData$r.DOSW_norm_log <- log(myData$r.DOSW_norm+1)
myData$r.other_norm_log <- log(myData$r.other_norm+1)
myData$r.DODW_norm_log <- log(myData$r.DODW_norm+1)
myData$r.obj_to_lm_norm_log <- log(myData$r.obj_to_lm_norm+1)
myData$r.lm_to_obj_norm_log <- log(myData$r.lm_to_obj_norm+1)
myData$r.obj_to_so_norm_log <- log(myData$r.obj_to_so_norm+1)
myData$r.obj_to_diffObj_norm_log <- log(myData$r.obj_to_diffObj_norm+1)
myData$r.lm_to_lm_norm_log <- log(myData$r.lm_to_lm_norm+1)

# create a dataset that groups by subject
subject_df <- myData %>%
  group_by(subject) %>%
  summarize(
    s.landmarks_norm_log = mean(s.landmarks_norm_log),
    s.same_object_norm_log = mean(s.same_object_norm_log),
    s.DOSW_norm_log = mean(s.DOSW_norm_log),
    s.other_norm_log = mean(s.other_norm_log),
    s.DODW_norm_log = mean(s.DODW_norm_log),
    s.obj_to_lm_norm_log = mean(s.obj_to_lm_norm_log),
    s.lm_to_obj_norm_log = mean(s.lm_to_obj_norm_log),
    s.obj_to_so_norm_log = mean(s.obj_to_so_norm_log),
    s.obj_to_diffObj_norm_log = mean(s.obj_to_diffObj_norm_log),
    s.lm_to_lm_norm_log = mean(s.lm_to_lm_norm_log),
    r.landmarks_norm_log = mean(r.landmarks_norm_log),
    r.same_object_norm_log = mean(r.same_object_norm_log),
    r.DOSW_norm_log = mean(r.DOSW_norm_log),
    r.other_norm_log = mean(r.other_norm_log),
    r.DODW_norm_log = mean(r.DODW_norm_log),
    r.obj_to_lm_norm_log = mean(r.obj_to_lm_norm_log),
    r.lm_to_obj_norm_log = mean(r.lm_to_obj_norm_log),
    r.obj_to_so_norm_log = mean(r.obj_to_so_norm_log),
    r.obj_to_diffObj_norm_log = mean(r.obj_to_diffObj_norm_log),
    r.lm_to_lm_norm_log = mean(r.lm_to_lm_norm_log)
  )


### test study against test in each category
t.test(subject_df$s.landmarks_norm_log, subject_df$r.landmarks_norm_log, paired = TRUE, alternative = "two.sided")
mean(subject_df$s.landmarks_norm_log, na.rm = TRUE)
mean(subject_df$r.landmarks_norm_log, na.rm = TRUE)

t.test(subject_df$s.same_object_norm_log, subject_df$r.same_object_norm_log, paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.DOSW_norm_log , subject_df$r.DOSW_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.other_norm_log , subject_df$r.other_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.DODW_norm_log , subject_df$r.DODW_norm_log , paired = TRUE, alternative = "two.sided") # not sig
t.test(subject_df$s.obj_to_lm_norm_log , subject_df$r.obj_to_lm_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.lm_to_obj_norm_log , subject_df$r.lm_to_obj_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.obj_to_so_norm_log , subject_df$r.obj_to_so_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.obj_to_diffObj_norm_log , subject_df$r.obj_to_diffObj_norm_log , paired = TRUE, alternative = "two.sided")
t.test(subject_df$s.lm_to_lm_norm_log , subject_df$r.lm_to_lm_norm_log , paired = TRUE, alternative = "two.sided") # not sig


ggpaired(subject_df, cond1 = "s.landmarks_norm_log", cond2 = "r.landmarks_norm_log")
ggpaired(subject_df, cond1 = "s.same_object_norm_log", cond2 = "r.same_object_norm_log" )
ggpaired(subject_df, cond1 = "s.DOSW_norm_log", cond2 = "r.DOSW_norm_log")
ggpaired(subject_df, cond1 = "s.other_norm_log", cond2 = "r.other_norm_log")
ggpaired(subject_df, cond1 = "s.DODW_norm_log", cond2 = "r.DODW_norm_log")
ggpaired(subject_df, cond1 = "s.obj_to_lm_norm_log", cond2 = "r.obj_to_lm_norm_log")
ggpaired(subject_df, cond1 = "s.lm_to_obj_norm_log", cond2 = "r.lm_to_obj_norm_log")
ggpaired(subject_df, cond1 = "s.obj_to_so_norm_log", cond2 = "r.obj_to_so_norm_log")
ggpaired(subject_df, cond1 = "s.obj_to_diffObj_norm_log", cond2 = "r.obj_to_diffObj_norm_log")
ggpaired(subject_df, cond1 = "s.lm_to_lm_norm_log", cond2 = "r.lm_to_lm_norm_log")








