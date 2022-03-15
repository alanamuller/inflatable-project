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
myData <- read_excel("s004_to_merge.xlsx")
str(myData)

myData <- as.data.frame(myData)
myData$subject <- as.factor(myData$subject)
myData$trial <- as.factor(myData$trial)
myData$wall_side <- as.factor(myData$wall_side)
myData$object <- as.factor(myData$object)
myData$object_size <- as.factor(myData$object_size)
myData$object_material <- as.factor(myData$object_material)
myData$walk_noWalk <- as.factor(myData$walk_noWalk)
myData$same_diff <- as.factor(myData$same_diff)
myData$start_wall <- as.factor(myData$start_wall)
myData$height <- as.factor(myData$height)
myData$width <- as.factor(myData$width)
#myData$next_to_landmark <- as.factor(myData$next_to_landmark)
#myData$which_landmark <- as.factor(myData$which_landmark)

myData$x_study <- as.numeric(myData$x_study)
myData$y_study <- as.numeric(myData$y_study)
myData$x_replace <- as.numeric(myData$x_replace)
myData$y_replace <- as.numeric(myData$y_replace)

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

# Save file
write.csv(myData, "E:/Nav_1stYr_project_data/nav_room_error_data_s004.csv", row.names = FALSE)



