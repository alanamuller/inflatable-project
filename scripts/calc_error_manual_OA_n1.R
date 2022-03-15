library(ggp+lot2)
library(reshape)
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

rm(list = ls())

setwd("E:/Nav_1stYr_project_data")

# Read in data
myData <- read_csv("E:/Nav_1stYr_project_data/OA_nav_room_error_data_N1.csv")
str(myData)

# fix the data frame to have the correct properties
myData <- as.data.frame(myData)
myData$subject <- as.factor(myData$subject)
myData$trial <- as.factor(myData$trial)
myData$wall_side <- as.factor(myData$wall_side)
myData$object <- as.factor(myData$object)
#myData$object_size <- as.factor(myData$object_size)
#myData$object_aliveORnot <- as.factor(myData$object_aliveORnot)
#myData$object_material <- as.factor(myData$object_material)
myData$walk_noWalk <- as.factor(myData$walk_noWalk)
myData$same_diff <- as.factor(myData$same_diff)
myData$start_wall <- as.factor(myData$start_wall)
myData$height <- as.factor(myData$height)
myData$width <- as.factor(myData$width)
myData$next_to_landmark <- as.factor(myData$next_to_landmark)
myData$which_landmark <- as.factor(myData$which_landmark)
#myData$Fixated_or_not <- as.factor(myData$Fixated_or_not)

myData <- unite(myData, move_view, c(walk_noWalk, same_diff), remove = FALSE)
myData$move_view <- as.factor(myData$move_view)

# Data is skewed to the right
hist(myData$placement_error_cm)
shapiro.test(myData$placement_error_cm) # very sig

# Trying a log transformation
myData$placement_error_cm_log <- log10(myData$placement_error_cm)
shapiro.test(myData$placement_error_cm_log) # p = .1036
hist(myData$placement_error_cm_log)

# Trying a sqrt transformation --- let's use this one
myData$placement_error_cm_sqrt <- sqrt(myData$placement_error_cm)
shapiro.test(myData$placement_error_cm_sqrt) # p = .4906
hist(myData$placement_error_cm_sqrt)

##################################################################
# No outliers
mean_placement_error_cm <- mean(myData$placement_error_cm)
sd_data <- sd(myData$placement_error_cm)

data_NO <- myData %>%
  filter(placement_error_cm < mean_placement_error_cm + (3*sd_data) & placement_error_cm > mean_placement_error_cm - (3*sd_data))

##################################################################

bxp <- ggboxplot(
  myData, x = "walk_noWalk", y = "placement_error_cm_sqrt", color = "same_diff", add = "jitter"
)
bxp

no_outlier <- myData %>%
  filter(placement_error_cm < 100)

bxp <- ggboxplot(
  data_NO, x = "walk_noWalk", y = "placement_error_cm", color = "same_diff", add = "jitter",
  xlab = "Movement Condition", ylab = "Placement Error (cm)") +   
  scale_y_continuous(breaks = seq(0,80,20)) + scale_x_discrete(breaks=c("no walk", "walk"), labels=c("Stationary", "Walk")) +
  labs(color = "Viewpoint") + scale_color_discrete(labels = c("Different", "Same"))
bxp

walk_view_table <- myData %>%
  select(subject, walk_noWalk, same_diff, placement_error_cm_sqrt)

aov_data <- walk_view_table %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    count = n(),
    mean = mean(placement_error_cm_sqrt, na.rm = TRUE),
    sd = sd(placement_error_cm_sqrt, na.rm = TRUE),
  )

# outliers
walk_view_table %>%
  group_by(same_diff, walk_noWalk) %>%
  identify_outliers(placement_error_cm_sqrt) # one, walk, diff, 11.4

# normality
walk_view_table %>%
  group_by(same_diff, walk_noWalk) %>%
  shapiro_test(placement_error_cm_sqrt) # walk, diff p = .023

ggqqplot(walk_view_table, "placement_error_cm_sqrt", ggtheme = theme_bw()) +
  facet_grid(walk_noWalk ~ same_diff, labeller = "label_both")

# 2-way ANOVA walk view - not sig (not repeated measures because I need more participants first)
results <- aov(placement_error_cm_sqrt ~ walk_noWalk*same_diff, data = walk_view_table)
summary(results)

# t-test for landmarks
landmark_data <- myData%>%
  select("next_to_landmark", "placement_error_cm_sqrt")

yes_landmark <- landmark_data %>%
  filter(next_to_landmark == "y")
yes_landmark <- yes_landmark %>%
  select("placement_error_cm_sqrt")

no_landmark <- landmark_data %>%
  filter(next_to_landmark == "n")
no_landmark <- no_landmark %>%
  select("placement_error_cm_sqrt")

# t-test for if memory is better for items next to landmarks: not sig, p = .5138
t.test(yes_landmark, no_landmark, paired = FALSE, var.equal = TRUE)

landmark_table <- landmark_data %>%
  group_by(next_to_landmark) %>%
  summarize(
    mean = mean(placement_error_cm_sqrt),
    sd = sd(placement_error_cm_sqrt)
  )

bxp <- ggboxplot(
  myData, x = "next_to_landmark", y = "placement_error_cm", add = "jitter"
) + scale_y_continuous(breaks = seq(0,140,20))
bxp














