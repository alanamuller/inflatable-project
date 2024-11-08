# Calculate error for putting objects back on the wall - data entered manually

library(ggplot2)
library(reshape)
library(readxl)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(rstatix)

rm(list = ls())

setwd("E:/Nav_1stYr_project_data")

# Read in data
myData <- read_excel("manuscript_data_OA.xlsx")
str(myData)

myData <- as.data.frame(myData)
myData$subject <- as.factor(myData$subject)
myData$trial <- as.factor(myData$trial)
myData$wall_side <- as.factor(myData$wall_side)
myData$object <- as.factor(myData$object)
myData$object_size <- as.factor(myData$object_size)
myData$object_material <- as.factor(myData$object_material)
myData$object_aliveORnot <- as.factor(myData$object_aliveORnot)
myData$walk_noWalk <- as.factor(myData$walk_noWalk)
myData$same_diff <- as.factor(myData$same_diff)
myData$start_wall <- as.factor(myData$start_wall)
myData$height <- as.factor(myData$height)
myData$width <- as.factor(myData$width)
myData$next_to_landmark <- as.factor(myData$next_to_landmark)
myData$which_landmark <- as.factor(myData$which_landmark)
myData$`cart (took/left/half)`<- as.factor(myData$`cart (took/left/half)`)
myData$Gender <- as.factor(myData$Gender)
myData$Video_game_exp <- as.factor(myData$Video_game_exp)
myData$Fixated_or_not <- as.factor(myData$Fixated_or_not)

myData$order_removed <- as.numeric(myData$order_removed)
myData$order_replaced <- as.numeric(myData$order_replaced)
myData$order_looked <- as.numeric(myData$order_looked)
myData$x_study <- as.numeric(myData$x_study)
myData$y_study <- as.numeric(myData$y_study)
myData$x_replace <- as.numeric(myData$x_replace)
myData$y_replace <- as.numeric(myData$y_replace)
myData$fix_num <- as.numeric(myData$fix_num)
myData$total_fix_dur_ms <- as.numeric(myData$total_fix_dur_ms)
myData$avg_fix_dur_ms <- as.numeric(myData$avg_fix_dur_ms)

# Make the origin (0,0) at the bottom left - only need to change the y-axis because the origin for matlab and photoshop pixel coordinates is the top left
# So essentially flip y-axis
myData$y_study <- (myData$y_study*-1) + 1080
myData$y_replace <- (myData$y_replace*-1) + 1080

# Now rezero data so that the origin (0,0) is in the middle
myData$x_study <- myData$x_study - 960
myData$x_replace <- myData$x_replace - 960
myData$y_study <- myData$y_study - 540
myData$y_replace <- myData$y_replace - 540

myData$x_error <- myData$x_replace - myData$x_study
myData$y_error <- myData$y_replace - myData$y_study
myData$placement_error <- sqrt ( (myData$x_error^2) + (myData$y_error^2) )

# change pixels into real-world coordinates in cm
# x: 1 pixel = 0.3907 cm
# y: 1 pixel = 0.3905 cm
myData$x_error_cm <- myData$x_error*0.3907
myData$y_error_cm <- myData$y_error*0.3905
myData$placement_error_cm <- sqrt ( (myData$x_error_cm^2) + (myData$y_error_cm^2) )

# absolute x and y error
myData$abs_x_error_cm <- abs(myData$x_error_cm)
myData$abs_y_error_cm <- abs(myData$y_error_cm)

# Save file
write.csv(myData, "E:/Nav_1stYr_project_data/manuscript_data_OA_preprocessed.csv", row.names = FALSE)



