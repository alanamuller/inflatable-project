library(ggplot2)
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
library(BayesFactor)
library(PairedData)

rm(list = ls())

setwd("E:/Nav_1stYr_project_data")

############################### Prepping the data ###############################

# Read in data
myData <- read_csv("E:/Nav_1stYr_project_data/partialOA_nav_room_error_processed.csv")
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
#myData$order_removed <- as.factor(myData$order_removed)
#myData$oreder_replaced <- as.factor(myData$order_replaced)
myData$next_to_landmark <- as.factor(myData$next_to_landmark)
myData$which_landmark <- as.factor(myData$which_landmark)
#myData$Fixated_or_not <- as.factor(myData$Fixated_or_not)

myData <- unite(myData, move_view, c(walk_noWalk, same_diff), remove = FALSE)
myData$move_view <- as.factor(myData$move_view)

# uncomment this to save manuscript-quality pics to this folder
setwd("C:/Users/amuller/Desktop/Alana/UA/HSCL/First-Year Project/Older Adults/prelim_pics")

### Data is skewed to the right
jpeg("right_skew.jpeg", width = 7, height = 6, units = 'in', res = 500)
hist(myData$placement_error_cm, main = "Histogram of Placement Error (cm)", xlab = "Placement Error (cm)")
dev.off()

shapiro.test(myData$placement_error_cm) # very sig

# Trying a log transformation -- use log
myData$placement_error_cm_log <- log10(myData$placement_error_cm)
shapiro.test(myData$placement_error_cm_log) # p = .1036
jpeg("more_normal.jpeg", width = 7, height = 6, units = 'in', res = 500)
hist(myData$placement_error_cm_log, main = "Histogram of Placement Error (log cm)", xlab = "Placement Error (log cm)")
dev.off()

### x log data
myData$abs_x_error_cm_log <- log10(myData$abs_x_error_cm + 1)

### y log data
myData$abs_y_error_cm_log <- log10(myData$abs_y_error_cm + 1)

# Trying a sqrt transformation - not as good as log
myData$placement_error_cm_sqrt <- sqrt(myData$placement_error_cm)
shapiro.test(myData$placement_error_cm_sqrt)
hist(myData$placement_error_cm_sqrt)

# Outliers for placement error cm log - 75 outliers so far
mean_placement_error_cm_log <- mean(myData$placement_error_cm_log, na.rm = TRUE)
sd_data <- sd(myData$placement_error_cm_log, na.rm = TRUE)

# dataset with no placement_error_cm_log
data_NO <- myData %>%
  filter(placement_error_cm_log < mean_placement_error_cm_log + (3*sd_data) & placement_error_cm_log > mean_placement_error_cm_log - (3*sd_data))

# checking normality of the four conditions with placement_error_cm_log
walk_view_table <- myData %>%
  dplyr::select(subject, walk_noWalk, same_diff, placement_error_cm_log)

ggqqplot(walk_view_table, "placement_error_cm_log", ggtheme = theme_bw()) +
  facet_grid(walk_noWalk ~ same_diff, labeller = "label_both")

# dataset with no abs_x_error_log outliers
mean_absx_error_cm_log <- mean(myData$abs_x_error_cm_log, na.rm = TRUE)
absx_sd_data <- sd(myData$abs_x_error_cm_log, na.rm = TRUE)

data_NO_x <- myData %>%
  filter(abs_x_error_cm_log < mean_absx_error_cm_log + (3*absx_sd_data) & abs_x_error_cm_log > mean_absx_error_cm_log - (3*absx_sd_data))

# dataset with no abs_y_error_log outliers
mean_absy_error_cm_log <- mean(myData$abs_y_error_cm_log, na.rm = TRUE)
absy_sd_data <- sd(myData$abs_y_error_cm_log, na.rm = TRUE)

data_NO_y <- myData %>%
  filter(abs_y_error_cm_log < mean_absy_error_cm_log + (3*absy_sd_data) & abs_y_error_cm_log > mean_absy_error_cm_log - (3*absy_sd_data))

##################################################################




############################## 2-way repeated-measures ANOVA walk view ##############################
aov_data <- data_NO %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    mean = mean(placement_error_cm_log, na.rm = TRUE),
  )
aov_data <- as_tibble(aov_data)

aov_data$trial_type <- paste(aov_data$walk_noWalk, aov_data$same_diff, sep="_")

# summary stats used in 2way rep ANOVA
aov_means <- data_NO %>%
  group_by(walk_noWalk, same_diff) %>%
  get_summary_stats(placement_error_cm_log, type = "mean_sd")

bxp <- ggboxplot(
  aov_data, x = "walk_noWalk", y = "mean", 
  color = "same_diff", add = "jitter",
  xlab = "Movement Condition", ylab = "Placement Error (log cm)",
  legend = "right", legend.title = "Viewpoint") + 
  scale_x_discrete(breaks=c("no walk", "walk"), labels=c("Stationary", "Walk")) +
  scale_color_discrete(labels = c("Different", "Same"))
jpeg("movement_viewpoint_bxp.jpeg", width = 7, height = 6, units = 'in', res = 500)
bxp
dev.off()

# these are two ways to do a 2x2 repeated measures ANOVA
results_2way <- aov(mean ~ walk_noWalk*same_diff + Error(subject/(walk_noWalk*same_diff)), data = aov_data)
summary(results_2way) # nothing sig

withinTest <- anova_test(data = aov_data, dv = mean, wid = subject,
                         within = c(walk_noWalk, same_diff))
get_anova_table(withinTest) # nothing sig

# Bayes factor for this ANOVA
aov_data <- as.data.frame(aov_data)
bayes_rm <- anovaBF(mean ~ walk_noWalk*same_diff + subject, data = aov_data, whichRandom = "subject")
bayes_rm
plot(bayes_rm)

### 2-way ANOVA with abs x error
aov_x_data <- data_NO_x %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    mean = mean(abs_x_error_cm_log, na.rm = TRUE),
  )
aov_x_data <- as_tibble(aov_x_data)

bxp <- ggboxplot(
  aov_x_data, x = "walk_noWalk", y = "mean", 
  color = "same_diff", add = "jitter", 
  xlab = "Movement Condition", ylab = "X-Axis Placement Error (log cm)",
  legend = "right", legend.title = "Viewpoint") + 
  scale_x_discrete(breaks=c("no walk", "walk"), labels=c("Stationary", "Walk")) +
  scale_color_discrete(labels = c("Different", "Same"))
jpeg("movement_viewpoint_x_bxp.jpeg", width = 7, height = 6, units = 'in', res = 500)
bxp
dev.off()

# 2x2 repeated measures ANOVA - walk_noWalk is sig
results_x_2way <- aov(mean ~ walk_noWalk*same_diff + Error(subject/(walk_noWalk*same_diff)), data = aov_x_data)
summary(results_x_2way) # nothing is sig, no main effects, no interaction effect

# Bayes factor for this ANOVA
aovx_data <- as.data.frame(aov_x_data)
bayes_rm <- anovaBF(mean ~ walk_noWalk*same_diff + subject, data = aovx_data, whichRandom = "subject")
bayes_rm
plot(bayes_rm)

### 2-way ANOVA with abs y error
aov_y_data <- data_NO_y %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    mean = mean(abs_y_error_cm_log, na.rm = TRUE),
  )
aov_y_data <- as_tibble(aov_y_data)

bxp <- ggboxplot(
  aov_y_data, x = "walk_noWalk", y = "mean", 
  color = "same_diff", add = "jitter", 
  xlab = "Movement Condition", ylab = "Y-Axis Placement Error (log cm)",
  legend = "right", legend.title = "Viewpoint") + 
  scale_x_discrete(breaks=c("no walk", "walk"), labels=c("Stationary", "Walk")) +
  scale_color_discrete(labels = c("Different", "Same"))
jpeg("movement_viewpoint_y_bxp.jpeg", width = 7, height = 6, units = 'in', res = 500)
bxp
dev.off()

# 2x2 repeated measures ANOVA - nothing is sig
results_y_2way <- aov(mean ~ walk_noWalk*same_diff + Error(subject/(walk_noWalk*same_diff)), data = aov_y_data)
summary(results_y_2way) 

# Bayes factor for this ANOVA
aovy_data <- as.data.frame(aov_y_data)
bayes_rm <- anovaBF(mean ~ walk_noWalk*same_diff + subject, data = aov_y_data, whichRandom = "subject")
bayes_rm
plot(bayes_rm)

##### abs value of x coordinate vs y coordinate accuracy - paired t-test
xy_data <- myData %>%
  group_by(subject) %>%
  summarise(
    x_cm_mean = mean((abs_x_error_cm_log), na.rm = TRUE),
    y_cm_mean = mean((abs_y_error_cm_log), na.rm = TRUE)
  )
t.test(xy_data$x_cm_mean, xy_data$y_cm_mean, paired = TRUE) # p = .007048

# for the graph
xy_data_long <- xy_data %>%
  gather(error_type, mean_error, x_cm_mean, y_cm_mean)

x_mean <- subset(xy_data_long, error_type == "x_cm_mean", mean_error, drop = TRUE)
y_mean <- subset(xy_data_long, error_type == "y_cm_mean", mean_error, drop = TRUE)

pd <- paired(x_mean, y_mean)

jpeg("paired_ttest_xy.jpeg", width = 7, height = 6, units = 'in', res = 500)
plot(pd, type = "profile") + theme_classic() + ylab("Placement Error (log cm)") + 
  scale_x_discrete(breaks=c("x_mean", "y_mean"), labels=c("Horizontal \n (x-values)", "Vertical \n (y-values)")) + 
  theme(axis.text=element_text(size=10, color = 'black')) 
dev.off()

##################################################################




############################## Landmarks analysis ##############################

##### Landmark and placement accuracy

landmark_data <- data_NO %>%
  dplyr::select(c("subject", "next_to_landmark", "placement_error_cm_log"))

landmark_ttest <- landmark_data %>%
  group_by(subject, next_to_landmark) %>%
  summarise(
    mean = mean(placement_error_cm_log, na.rm = TRUE)
  )

# possible violin plot
testpic <- ggplot(landmark_ttest, aes(x = next_to_landmark, y = mean)) + geom_violin(draw_quantiles = c(0.25, 0.50, 0.75)) + 
  theme_classic() + stat_summary(fun = "mean", geom = "crossbar", color = "red")
testpic

# FIGURE FOR MANUSCRIPT
landmark_ttest$next_to_landmark <- factor(landmark_ttest$next_to_landmark, levels = c("y", "n"))
landmark_near <- ggboxplot(landmark_ttest, x = "next_to_landmark", y = "mean", group = "subject", color = "black", size = 0.25,
                           add = "jitter") +
  xlab("") +
  ylab("Mean Error (log cm)") +
  theme(legend.position = "none") +
  scale_x_discrete(breaks=c("y", "n"), labels=c("Next to Landmark", "Not Next to Landmark")) +
  theme(plot.title = element_text(hjust = 0.5))
jpeg("landmark.jpeg", width = 7, height = 6, units = 'in', res = 500)
landmark_near
dev.off()

landmark_ttest_wide <- spread(landmark_ttest, key = next_to_landmark, value = mean)

t.test(landmark_ttest_wide$y, landmark_ttest_wide$n, paired = TRUE) # not sig










