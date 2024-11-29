# Data file used for the manuscript results section for older adults
# Alana Muller
# amuller@arizona.edu
# 2024-08-29

library(ggplot2)
library(reshape)
library(reshape2)
library(readxl)
library(ggpubr)
library(tidyverse)
library(rstatix)
library(mosaic)
library(PerformanceAnalytics)
library(rcompanion)
library(PMCMRplus)
library(officer)
library(tidyr)
library(PairedData)
library(dplyr)
library(BayesFactor)
library(here)
library(lme4)

rm(list = ls())

# work computer uses E but laptop uses D, change accordingly
setwd("D:/Nav_1stYr_project_data")

# Read in data
inputData <- read.csv("D:/Nav_1stYr_project_data/manuscript_data_OA_preprocessed.csv")
inputData <- as.data.frame(inputData)
str(inputData) # check the structure of the data

# Make a copy of inputData that I'll use for analysis
myData <- inputData

# Make rows factors
i <- c(1:12,16:23,33,40,41)
myData [i] <- lapply(myData[i], factor)

# Make rows numbers
i <- c(13:15,24:32,34:39)
myData [i] <- lapply(myData[i], as.numeric)

# make abs value columns for x_error_cm and y_error_cm
myData$abs_x_error_cm <- abs(myData$x_error_cm)
myData$abs_y_error_cm <- abs(myData$y_error_cm)

# Make sure the data looks ok now before proceeding
str(myData)

# Placement error (cm) is skewed to the right
hist(myData$placement_error_cm)

# Using smaller bins to see the spread of data better at the lower region
hist(myData$placement_error_cm, breaks = 50)

### Data transformations - they were not successful at making the data normal but log is the best

# Log transformation to make the placement error data more normal
myData$placement_error_cm_log <- log10(myData$placement_error_cm +1)
hist(myData$placement_error_cm_log)
shapiro.test(myData$placement_error_cm_log) # still not normal but looks much better
ggqqplot(myData$placement_error_cm_log)

# cube transformation - not better than log
myData$placement_error_cm_cuberoot <- (myData$placement_error_cm)^(1/3)
hist(myData$placement_error_cm_cuberoot)
shapiro.test(myData$placement_error_cm_cuberoot) # still not normal, log is better
ggqqplot(myData$placement_error_cm_cuberoot)

### Transform abs value of x_error_cm and y_error_cm
myData$abs_x_error_cm_log <- log10(myData$abs_x_error_cm+1)
myData$abs_y_error_cm_log <- log10(myData$abs_y_error_cm+1)

# Make a dataframe for YAs
yaData <- myData %>%
  filter(group == "YA")


# Make a dataframe for OAs
myData <- myData %>%
  filter(group == "OA")

### Get rid of outliers of placement_error_cm past 3 SD of transformed data for OAs
mean_placement_error_cm_log <- mean(myData$placement_error_cm_log, na.rm = TRUE)
sd_placement_error_cm_log <- sd(myData$placement_error_cm_log, na.rm = TRUE)

# 9 outliers identified 3 SD away from the mean
outliers_placement_error_cm <- myData %>%
  filter(placement_error_cm_log > mean_placement_error_cm_log + (3*sd_placement_error_cm_log) | 
         placement_error_cm_log < mean_placement_error_cm_log - (3*sd_placement_error_cm_log))

# Make a dataset with No Outliers (NO for short) and also gets rid of NAs in placement_error_cm_log
myData_NO <- myData %>% 
  filter(placement_error_cm_log < mean_placement_error_cm_log + (3*sd_placement_error_cm_log), 
           placement_error_cm_log > mean_placement_error_cm_log - (3*sd_placement_error_cm_log) )

# Get rid of outliers for YAs
mean_ya_error_log <- mean(yaData$placement_error_cm_log, na.rm = TRUE)
sd_ya_error_log <- sd(yaData$placement_error_cm_log, na.rm = TRUE)

# Make a dataset with No Outliers for YAs
yaData_NO <- yaData %>%
  filter(placement_error_cm_log < mean_ya_error_log + (3*sd_ya_error_log) |
           placement_error_cm_log - (3*sd_ya_error_log))

# Do these counts before log transforming
hist(myData$placement_error_cm, breaks = 25)
resp_less_5 <- myData %>%
  filter(placement_error_cm <= 5) # 81 observations
resp_less_10 <- myData %>%
  filter(placement_error_cm <= 10) # 254 observations
resp_less_20 <- myData %>%
  filter(placement_error_cm <= 20) # 708 observations
resp_less_30 <- myData %>%
  filter(placement_error_cm <= 30) # 1119 observations
resp_20_50 <- myData_NO %>%
  filter(placement_error_cm > 20 & placement_error_cm <= 50) # 994 observations
resp_greater_50 <- myData_NO %>%
  filter(placement_error_cm >= 50) # 794 observations
resp_less_0 <- myData_NO %>%
  filter(placement_error_cm <= 0) # 0 observations
resp_less_1 <- myData_NO %>%
  filter(placement_error_cm <= 1) # 0 observations
resp_less_2 <- myData_NO %>%
  filter(placement_error_cm <= 2) # 11 observations
resp_2_3 <- myData_NO %>%
  filter(placement_error_cm > 2 & placement_error_cm <= 3) # 19 observations
resp_3_4 <- myData_NO %>%
  filter(placement_error_cm > 3 & placement_error_cm <= 4) # 20 observations
resp_4_5 <- myData_NO %>%
  filter(placement_error_cm > 4 & placement_error_cm <= 5) # 31 observations
resp_5_6 <- myData_NO %>%
  filter(placement_error_cm > 5 & placement_error_cm <= 6) # 39 observations
resp_6_7 <- myData_NO %>%
  filter(placement_error_cm > 6 & placement_error_cm <= 7) # 23 observations
resp_7_8 <- myData_NO %>%
  filter(placement_error_cm > 7 & placement_error_cm <= 8) # 34 observations
resp_8_9 <- myData_NO %>%
  filter(placement_error_cm > 8 & placement_error_cm <= 9) # 41 observations
resp_9_10 <- myData_NO %>%
  filter(placement_error_cm > 9 & placement_error_cm <= 10) # 36 observations

resp_5_10 <- myData_NO %>%
  filter(placement_error_cm > 5 & placement_error_cm <= 10) # 173 observations
resp_10_20 <- myData_NO %>%
  filter(placement_error_cm > 10 & placement_error_cm <= 20) # 454 observations

# How participants performed in cm
# Average over trials first
subj_trial_cm_data <- myData_NO %>%
  group_by(subject, trial) %>%
  summarize(
    count = n(),
    placement_error_cm = mean(placement_error_cm)
  )
# Average over subject
subj_cm_data <- subj_trial_cm_data %>%
  group_by(subject) %>%
  summarise(
    count = n(), 
    placement_error_cm_mean = mean(placement_error_cm)
  )
# plot error by subject
plot(x = subj_cm_data$subject, y = subj_cm_data$placement_error_cm_mean)

subj_mean <- mean(subj_cm_data$placement_error_cm_mean) # 48.7 cm
subj_sd <- sd(subj_cm_data$placement_error_cm_mean) # 19.5 cm
min(subj_cm_data$placement_error_cm_mean) # 24.4
max(subj_cm_data$placement_error_cm_mean) # 113.7

# take out participant 16 because it's an outlier
subj_cm_data_NO <- subj_cm_data %>%
  filter(placement_error_cm_mean < subj_mean + (3*subj_sd))

mean(subj_cm_data_NO$placement_error_cm_mean)
sd(subj_cm_data_NO$placement_error_cm_mean)
min(subj_cm_data_NO$placement_error_cm_mean)
max(subj_cm_data_NO$placement_error_cm_mean)

# Check normality assumption on NO data - still not normal but looks better
hist(myData_NO$placement_error_cm_log)
ggqqplot(myData_NO$placement_error_cm_log)
shapiro.test(myData_NO$placement_error_cm_log) 


# Get rid of outliers from abs value of x_error_cm
mean_x_error_cm_log <- mean(myData$abs_x_error_cm_log, na.rm = TRUE)
sd_x_error_cm_log <- sd(myData$abs_x_error_cm_log, na.rm = TRUE)

# 0 outliers found
outliers_x_error_cm_log <- myData %>%
  filter(abs_x_error_cm_log > mean_x_error_cm_log + (3*sd_x_error_cm_log) | 
         abs_x_error_cm_log < mean_x_error_cm_log - (3*sd_x_error_cm_log))

# Make a dataset without outliers for x_error_cm
myData_NO_x <- myData %>%
  filter(abs_x_error_cm_log < mean_x_error_cm_log + (3*sd_x_error_cm_log), 
         abs_x_error_cm_log > mean_x_error_cm_log - (3*sd_x_error_cm_log))

### Get rid of outliers from abs value of y_error_cm
mean_y_error_cm_log <- mean(myData$abs_y_error_cm_log, na.rm = TRUE)
sd_y_error_cm_log <- sd(myData$abs_y_error_cm_log, na.rm = TRUE)

# 0 outliers found
outliers_y_error_cm_log <- myData %>%
  filter(abs_y_error_cm_log > mean_y_error_cm_log + (3*sd_y_error_cm_log) | 
           abs_y_error_cm_log < mean_y_error_cm_log - (3*sd_y_error_cm_log))

# Make a dataset without outliers for y_error_cm
myData_NO_y <- myData %>%
  filter(abs_y_error_cm_log < mean_y_error_cm_log + (3*sd_y_error_cm_log), 
           abs_y_error_cm_log > mean_y_error_cm_log - (3*sd_y_error_cm_log))


################### Parametric Analyses ###################  

##### set up for 2-way repeated-measures ANOVA walk view
aov_data <- myData_NO %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    mean = mean(placement_error_cm_log, na.rm = TRUE),
    sd = sd(placement_error_cm_log, na.rm = TRUE)
  )
aov_data <- as_tibble(aov_data)

aov_data$trial_type <- paste(aov_data$walk_noWalk, aov_data$same_diff, sep="_")

# summary stats used in 2way rep ANOVA
aov_means <- myData_NO %>%
  group_by(walk_noWalk, same_diff) %>%
  get_summary_stats(placement_error_cm_log, type = "mean_sd")

# Plot with only older adults

bxp <- ggboxplot(
  aov_data, x = "walk_noWalk", y = "mean", 
  color = "same_diff", add = "jitter",
  xlab = "Movement Condition", ylab = "Placement Error (log cm)",
  legend = "right", legend.title = "Viewpoint") + 
  scale_x_discrete(breaks=c("no walk", "walk"), labels=c("Stationary", "Walk")) +
  scale_color_discrete(labels = c("Different", "Same")) +
  stat_summary(aes(group = same_diff), fun = mean, geom = "point", shape = 18, size = 3, color = "black", position = position_dodge(0.75))
#jpeg("movement_viewpoint_bxp_OA_mean.jpeg", width = 7, height = 6, units = 'in', res = 500)
bxp
#dev.off()

###### 2x2 repeated measures ANOVA for mean - not sig
withinTest <- anova_test(data = aov_data, dv = mean, wid = subject,
                         within = c(walk_noWalk, same_diff))
get_anova_table(withinTest) # nothing is sig

# Bayes factor for this ANOVA
aov_data <- as.data.frame(aov_data)
bayes_rm <- anovaBF(mean ~ walk_noWalk*same_diff + subject, data = aov_data, whichRandom = "subject")
bayes_rm
plot(bayes_rm)

# graph for sd aka precision
bxp <- ggboxplot(
  aov_data, x = "walk_noWalk", y = "sd", 
  color = "same_diff", add = "jitter",
  xlab = "Movement Condition", ylab = "Placement Error SD (log cm)",
  legend = "right", legend.title = "Viewpoint") + 
  scale_x_discrete(breaks=c("no walk", "walk"), labels=c("Stationary", "Walk")) +
  scale_color_discrete(labels = c("Different", "Same")) +
  stat_summary(aes(group = same_diff), fun = mean, geom = "point", shape = 18, size = 3, color = "black", position = position_dodge(0.75))
#jpeg("movement_viewpoint_bxp_OA_sd.jpeg", width = 7, height = 6, units = 'in', res = 500)
bxp
#dev.off()

##### # 2x2 repeated measures ANOVA for sd
withinTest <- anova_test(data = aov_data, dv = sd, wid = subject,
                         within = c(walk_noWalk, same_diff))
get_anova_table(withinTest) # walk is sig

# Bayes factor for this ANOVA
aov_data <- as.data.frame(aov_data)
bayes_rm <- anovaBF(sd ~ walk_noWalk*same_diff + subject, data = aov_data, whichRandom = "subject")
bayes_rm
plot(bayes_rm)


# connecting lines - NOT USED but saved for example of how to do it so I don't forget
ggline(aov_data, x = "trial_type", y = "mean", group = "subject", color = "black", size = 0.25,
                     add = "boxplot") +
  xlab("Movement Condition") +
  ylab("Placement Error (log cm)") +
  theme(legend.position = "none") +
  scale_x_discrete(breaks=c("no walk_diff", "no walk_same", "walk_diff", "walk_same"),
                 labels=c("Stationary \n Different Viewpoint", "Stationary \n Same Viewpoint", "Walk \n Different Viewpoint", "Walk \n Same Viewpoint")) +
  theme(plot.title = element_text(hjust = 0.5))


# Combine OA and YA dataframes
oayaData <- rbind(myData_NO, yaData_NO)

##### Comparing YAs and OAs
# Generalized linear mixed effects model
fit.lmer <- lmer(placement_error_cm_log ~ group + walk_noWalk + same_diff + (1 | subject), data = oayaData)
summary(fit.lmer)

plot(fit.lmer)
qqnorm(resid(fit.lmer))
qqline(resid(fit.lmer))

##### Plots with YAs and OAs
aov_data2 <- oayaData %>%
  group_by(subject, walk_noWalk, same_diff, group) %>%
  summarize(
    mean = mean(placement_error_cm_log, na.rm = TRUE),
    sd = sd(placement_error_cm_log, na.rm = TRUE)
  )
aov_data2 <- as_tibble(aov_data2)

# summary stats used in 2way rep ANOVA
aov_means2 <- oayaData %>%
  group_by(walk_noWalk, same_diff, group) %>%
  get_summary_stats(placement_error_cm_log, type = "mean_sd")

# FIGURE FOR MANUSCRIPT

bxp <- ggboxplot(
  aov_data2, x = "walk_noWalk", y = "mean", 
  color = "same_diff", add = "jitter",
  xlab = "Movement Condition", ylab = "Placement Error (log cm)",
  legend = "right", legend.title = "Viewpoint") + 
  scale_x_discrete(breaks=c("no walk", "walk"), labels=c("Stationary", "Walk")) +
  scale_color_discrete(labels = c("Different", "Same")) +
  stat_summary(aes(group = same_diff), fun = mean, geom = "point", shape = 18, size = 3, color = "black", position = position_dodge(0.75)) +
  facet_wrap(~group, labeller = as_labeller(c("OA" = "Older Adult", "YA" = "Younger Adult"))) + 
  theme(
    strip.text = element_text(size = 12),   # Change facet label font size
    axis.text = element_text(size = 12),    # Change axis text size
    axis.title = element_text(size = 14),   # Change axis title size
    legend.text = element_text(size = 12),   # Change legend text size
    panel.border = element_rect(color = "black", fill = NA, size = .75) # Add border around each facet
  )
#jpeg("D:/Nav Stress Data/dissertation/pics/movement_viewpoint_bxp_OAYA_mean.jpeg", width = 8, height = 6, units = 'in', res = 500)
bxp
#dev.off() 

bxp <- ggboxplot(
  aov_data2, x = "walk_noWalk", y = "sd", 
  color = "same_diff", add = "jitter",
  xlab = "Movement Condition", ylab = "Placement Error SD (log cm)",
  legend = "right", legend.title = "Viewpoint") + 
  scale_x_discrete(breaks=c("no walk", "walk"), labels=c("Stationary", "Walk")) +
  scale_color_discrete(labels = c("Different", "Same")) +
  stat_summary(aes(group = same_diff), fun = mean, geom = "point", shape = 18, size = 3, color = "black", position = position_dodge(0.75)) +
  facet_wrap(~group, labeller = as_labeller(c("OA" = "Older Adult", "YA" = "Younger Adult"))) + 
  theme(
    strip.text = element_text(size = 12),   # Change facet label font size
    axis.text = element_text(size = 12),    # Change axis text size
    axis.title = element_text(size = 14),   # Change axis title size
    legend.text = element_text(size = 12),   # Change legend text size
    panel.border = element_rect(color = "black", fill = NA, size = .75) # Add border around each facet
  )
#jpeg("movement_viewpoint_bxp_OAYA_sd.jpeg", width = 8, height = 6, units = 'in', res = 500)
bxp
#dev.off()

# try a 2x2x2 ANOVA
willItBlend <- anova_test(data = aov_data2, dv = mean, wid = subject, 
                       within = c(walk_noWalk, same_diff), between = group)
get_anova_table(willItBlend) # main effect of group - for manuscript

# Bayes factor for this ANOVA
aov_data2 <- as.data.frame(aov_data2)
#bayes_rm <- anovaBF(mean ~ walk_noWalk*same_diff*group + subject, data = aov_data2, whichRandom = "subject")
bayes_rm
plot(bayes_rm)

willItBlendSD <- anova_test(data = aov_data2, dv = sd, wid = subject, 
                          within = c(walk_noWalk, same_diff), between = group)
get_anova_table(willItBlendSD)

### exclude trials when people took the cart AND put back in the same order and halfs
### all other trials can stay, just don't want people retracing their steps

cart <- oayaData %>%
  group_by(subject,trial,cart..took.left.half.,objects_put_back_order..same.not_same.) %>%
  summarize(
    count = n(),
    mean = mean(placement_error_cm_log), 
    sd = sd(placement_error_cm_log)
  )

# make groups of each pair of conditions to see how many of each there are
cart_took_same <- oayaData %>%
  filter(cart..took.left.half.== "took" & objects_put_back_order..same.not_same. == "same") # truly retracing their steps

cart_took_notSame <- oayaData %>%
  filter(cart..took.left.half.== "took" & objects_put_back_order..same.not_same. == "not_same") # keep in analysis

cart_left_same <- oayaData %>%
  filter(cart..took.left.half.== "left" & objects_put_back_order..same.not_same. == "same") # still could be retracing their steps by reviewing what they saw at encoding

cart_left_notSame <- oayaData %>%
  filter(cart..took.left.half.== "left" & objects_put_back_order..same.not_same. == "not_same") # keep in analysis

cart_half_same <- oayaData %>%
  filter(cart..took.left.half.== "half" & objects_put_back_order..same.not_same. == "same") # still retracing their steps

cart_half_notSame <- oayaData %>%
  filter(cart..took.left.half.== "half" & objects_put_back_order..same.not_same. == "not_same") # keep in analysis 

# put the data together that I want (took, not same; left, not same; half, not same)
cart_data <- oayaData %>%
  filter(cart..took.left.half.== "took" & objects_put_back_order..same.not_same. == "not_same" |
         cart..took.left.half.== "left" & objects_put_back_order..same.not_same. == "not_same" |
         cart..took.left.half.== "half" & objects_put_back_order..same.not_same. == "not_same")

##### only include people that left the cart
left_cart <- oayaData %>%
  filter(cart..took.left.half. == "left")
took_cart <- oayaData %>%
  filter(cart..took.left.half. == "took")
group_took <- took_cart %>%
  group_by(subject) %>%
  summarize (
    count = n()
  )

# without the took cart group
aov_cart_data <- left_cart %>%
  group_by(subject, walk_noWalk, same_diff, group) %>%
  summarize(
    mean = mean(placement_error_cm_log, na.rm = TRUE),
    sd = sd(placement_error_cm_log, na.rm = TRUE)
  )
aov_cart_data <- as_tibble(aov_cart_data)

bxp <- ggboxplot(
  aov_cart_data, x = "walk_noWalk", y = "mean", 
  color = "same_diff", add = "jitter", 
  xlab = "Movement Condition", ylab = "Placement Error (log cm)",
  legend = "right", legend.title = "Viewpoint") + 
  scale_x_discrete(breaks=c("no walk", "walk"), labels=c("Stationary", "Walk")) +
  scale_color_discrete(labels = c("Different", "Same")) +
  stat_summary(aes(group = same_diff), fun = mean, geom = "point", shape = 18, size = 3, color = "black", position = position_dodge(0.75)) +
  facet_wrap(~group, labeller = as_labeller(c("OA" = "Older Adult", "YA" = "Younger Adult"))) + 
  theme(
    strip.text = element_text(size = 12),   # Change facet label font size
    axis.text = element_text(size = 12),    # Change axis text size
    axis.title = element_text(size = 14),   # Change axis title size
    legend.text = element_text(size = 12),   # Change legend text size
    panel.border = element_rect(color = "black", fill = NA, size = .75) # Add border around each facet
  )
bxp

# only the took cart group
aov_cart_data_took <- took_cart %>%
  group_by(subject, walk_noWalk, same_diff, group) %>%
  summarize(
    mean = mean(placement_error_cm_log, na.rm = TRUE),
    sd = sd(placement_error_cm_log, na.rm = TRUE)
  )
aov_cart_data_took <- as_tibble(aov_cart_data_took)

bxp <- ggboxplot(
  aov_cart_data_took, x = "walk_noWalk", y = "mean", 
  color = "same_diff", add = "jitter", 
  xlab = "Movement Condition", ylab = "Placement Error (log cm)",
  legend = "right", legend.title = "Viewpoint") + 
  scale_x_discrete(breaks=c("no walk", "walk"), labels=c("Stationary", "Walk")) +
  scale_color_discrete(labels = c("Different", "Same")) +
  stat_summary(aes(group = same_diff), fun = mean, geom = "point", shape = 18, size = 3, color = "black", position = position_dodge(0.75)) +
  facet_wrap(~group, labeller = as_labeller(c("OA" = "Older Adult", "YA" = "Younger Adult"))) + 
  theme(
    strip.text = element_text(size = 12),   # Change facet label font size
    axis.text = element_text(size = 12),    # Change axis text size
    axis.title = element_text(size = 14),   # Change axis title size
    legend.text = element_text(size = 12),   # Change legend text size
    panel.border = element_rect(color = "black", fill = NA, size = .75) # Add border around each facet
  )
bxp

##### # 2x2 repeated measures ANOVA cart for mean
# without the took cart group
withinTest <- anova_test(data = aov_cart_data, dv = mean, wid = subject,
                         within = c(walk_noWalk, same_diff), between = group)
get_anova_table(withinTest) # no change, group still sig

# Bayes factor for this ANOVA
aov_cart_data <- as.data.frame(aov_cart_data)
#bayes_rm <- anovaBF(mean ~ walk_noWalk*same_diff*group + subject, data = aov_cart_data, whichRandom = "subject")
bayes_rm
plot(bayes_rm)

# only the took cart group
withinTest <- anova_test(data = aov_cart_data_took, dv = mean, wid = subject,
                         within = c(walk_noWalk, same_diff), between = group)
get_anova_table(withinTest) # no change, group still sig

# Bayes factor for this ANOVA
# without the took cart group
aov_data <- as.data.frame(aov_cart_data)
#bayes_rm <- anovaBF(mean ~ walk_noWalk*same_diff + subject, data = aov_cart_data, whichRandom = "subject")
bayes_rm
plot(bayes_rm)

# only the took cart group
aov_data_took <- as.data.frame(aov_cart_data_took)
bayes_rm <- anovaBF(mean ~ walk_noWalk*same_diff + subject, data = aov_cart_data_took, whichRandom = "subject")
bayes_rm
plot(bayes_rm)

##### mixed repeated ANOVA



##### # 2x2 repeated measures ANOVA cart for sd
withinTest <- anova_test(data = aov_cart_data, dv = sd, wid = subject,
                         within = c(walk_noWalk, same_diff))
get_anova_table(withinTest) # walk is still sig

# Bayes factor for this ANOVA
aov_data <- as.data.frame(aov_cart_data)
bayes_rm <- anovaBF(sd ~ walk_noWalk*same_diff + subject, data = aov_cart_data, whichRandom = "subject")
bayes_rm
plot(bayes_rm)

### exclude trials when people put objects back in the same order as taken off the walls
cart_not_samePutBackOrder <- myData_NO %>%
  filter(objects_put_back_order..same.not_same. != "same") # only keep trials in which objects were not put back in same order

cart_samePutBackOrder <- myData_NO %>%
  filter(objects_put_back_order..same.not_same. == "same")

cart_same_counts <- myData_NO %>%
  filter(objects_put_back_order..same.not_same. == "same") %>%
  group_by(subject, trial) %>%
  summarize(
    count = n(),
    mean = mean(placement_error_cm_log), 
    sd = sd(placement_error_cm_log)
  ) # sanity check to make sure I know how many trials are part of this "same put back order" group

cart_not_same_counts <- myData_NO %>%
  filter(objects_put_back_order..same.not_same. != "same") %>%
  group_by(subject, trial) %>%
  summarize(
    count = n(),
    mean = mean(placement_error_cm_log), 
    sd = sd(placement_error_cm_log)
  ) # sanity check to make sure I know how many trials are part of this "not same put back order" group

cart_not_samePutBackOrder_data <- cart_not_samePutBackOrder %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    mean = mean(placement_error_cm_log, na.rm = TRUE),
  )
cart_not_samePutBackOrder_data <- as_tibble(cart_not_samePutBackOrder_data)

bxp <- ggboxplot(
  cart_not_samePutBackOrder_data, x = "walk_noWalk", y = "mean", 
  color = "same_diff", add = "jitter", 
  xlab = "Movement Condition", ylab = "Placement Error (log cm)",
  legend = "right", legend.title = "Viewpoint") + 
  scale_x_discrete(breaks=c("no walk", "walk"), labels=c("Stationary", "Walk")) +
  scale_color_discrete(labels = c("Different", "Same"))
bxp

aov_sameSeqExclude <- anova_test(data = cart_not_samePutBackOrder_data, dv = mean, wid = subject,
                              within = c(walk_noWalk, same_diff))
get_anova_table(aov_sameSeqExclude) # not sig but p = 0.08 for walk_noWalk

# this 2-way repeated measures anova takes out the incomplete cases
cart_withinTest <- anova_test(data = aov_cart_data, dv = mean, wid = subject,
                              within = c(walk_noWalk, same_diff))
get_anova_table(cart_withinTest) # not sig but p = 0.08 for walk_noWalk

# Bayes factor for this ANOVA
aov_cart_data <- as.data.frame(aov_cart_data)
bayes_rm <- anovaBF(mean ~ walk_noWalk*same_diff + subject, data = aov_cart_data, whichRandom = "subject")
bayes_rm
plot(bayes_rm)

################### x and y error ANOVAS

### 2-way ANOVA with abs x error
aov_x_data <- myData_NO_x %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    mean = mean(abs_x_error_cm_log, na.rm = TRUE),
    sd = sd(abs_x_error_cm_log, na.rm = TRUE)
  )
aov_x_data <- as_tibble(aov_x_data)

bxp <- ggboxplot(
  aov_x_data, x = "walk_noWalk", y = "mean", 
  color = "same_diff", add = "jitter", 
  xlab = "Movement Condition", ylab = "Placement Error (log cm)",
  legend = "right", legend.title = "Viewpoint") + 
  scale_x_discrete(breaks=c("no walk", "walk"), labels=c("Stationary", "Walk")) +
  scale_color_discrete(labels = c("Different", "Same"))
bxp

# 2x2 repeated measures ANOVA mean and sd
results_x_2way <- anova_test(data = aov_x_data, dv = mean, wid = subject,
                              within = c(walk_noWalk, same_diff))
get_anova_table(results_x_2way) # walk is sig

results_x_2waySD <- anova_test(data = aov_x_data, dv = sd, wid = subject,
                             within = c(walk_noWalk, same_diff))
get_anova_table(results_x_2waySD) # nothing sig

### 2-way ANOVA with abs y error
aov_y_data <- myData_NO_y %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    mean = mean(abs_y_error_cm_log, na.rm = TRUE),
    sd = sd(abs_y_error_cm_log, na.rm = TRUE)
  )
aov_y_data <- as_tibble(aov_y_data)

bxp <- ggboxplot(
  aov_y_data, x = "walk_noWalk", y = "mean", 
  color = "same_diff", add = "jitter", 
  xlab = "Movement Condition", ylab = "Placement Error (log cm)",
  legend = "right", legend.title = "Viewpoint") + 
  scale_x_discrete(breaks=c("no walk", "walk"), labels=c("Stationary", "Walk")) +
  scale_color_discrete(labels = c("Different", "Same"))
bxp

# 2x2 repeated measures ANOVA mean and sd
results_y_2way <- anova_test(data = aov_y_data, dv = mean, wid = subject,
                             within = c(walk_noWalk, same_diff))
get_anova_table(results_y_2way) # nothing sig

results_y_2waySD <- anova_test(data = aov_y_data, dv = sd, wid = subject,
                               within = c(walk_noWalk, same_diff))
get_anova_table(results_y_2waySD) # nothing sig


##### abs value of x coordinate vs y coordinate accuracy - paired t-test
xy_data <- myData %>%
  group_by(subject) %>%
  summarise(
    x_cm_mean = mean((abs_x_error_cm_log), na.rm = TRUE),
    y_cm_mean = mean((abs_y_error_cm_log), na.rm = TRUE)
  )
t.test(xy_data$x_cm_mean, xy_data$y_cm_mean, paired = TRUE) # p = .1.958e-05

# for the graph
xy_data_long <- xy_data %>%
  gather(error_type, mean_error, x_cm_mean, y_cm_mean)

x_mean <- subset(xy_data_long, error_type == "x_cm_mean", mean_error, drop = TRUE)
y_mean <- subset(xy_data_long, error_type == "y_cm_mean", mean_error, drop = TRUE)

pd <- paired(x_mean, y_mean)

#jpeg("horizonal_vertical_values.jpeg", width = 3, height = 3, units = 'in', res = 300)
plot(pd, type = "profile") + theme_classic() + ylab("Mean Error (log cm)") + 
  scale_x_discrete(breaks=c("x_mean", "y_mean"), labels=c("Horizontal \n (x-values)", "Vertical \n (y-values)")) + 
  theme(axis.text=element_text(size=10, color = 'black')) 
#dev.off()

mean(xy_data$x_cm_mean) # 1.30
sd(xy_data$x_cm_mean) # 0.17

mean(xy_data$y_cm_mean) # 1.19
sd(xy_data$y_cm_mean) # 0.12

# untransformed
(10^mean(xy_data$x_cm_mean))-1 # 18.78
(10^sd(xy_data$x_cm_mean))-1 # 0.48

(10^mean(xy_data$y_cm_mean))-1 # 14.44
(10^sd(xy_data$y_cm_mean))-1 # 0.31


# I don't think this is the way to scale it. Doesn't make sense.
xy_data$x_cm_mean_scaled <- xy_data$x_cm_mean/6
xy_data$y_cm_mean_scaled <- xy_data$y_cm_mean/2.5

t.test(xy_data$x_cm_mean_scaled, xy_data$y_cm_mean_scaled, paired = TRUE)

##### Landmark and placement accuracy

landmark_data <- myData_NO %>%
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

# for mean
landmark_ttest$next_to_landmark <- factor(landmark_ttest$next_to_landmark, levels = c("y", "n"))
landmark_near <- ggboxplot(landmark_ttest, x = "next_to_landmark", y = "mean", group = "subject", color = "black", size = 0.25,
                     add = "jitter") +
  xlab("") +
  ylab("Mean Error (log cm)") +
  theme(legend.position = "none") +
  scale_x_discrete(breaks=c("y", "n"), labels=c("Next to Landmark", "Not Next to Landmark")) +
  theme(plot.title = element_text(hjust = 0.5))  +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75))
#jpeg("landmark_near_OAmean.jpeg", width = 7, height = 6, units = 'in', res = 500)
landmark_near
#dev.off()

landmark_ttest_wide <- spread(landmark_ttest, key = next_to_landmark, value = mean)

t.test(landmark_ttest_wide$y, landmark_ttest_wide$n, paired = TRUE) # not sig, p = .987

yes_mean <- subset(landmark_ttest, next_to_landmark == "y", mean, drop = TRUE)
no_mean <- subset(landmark_ttest, next_to_landmark == "n", mean, drop = TRUE)

pd <- paired(yes_mean, no_mean)

#jpeg("landmark.jpeg", width = 3, height = 3, units = 'in', res = 300)
plot(pd, type = "profile") + theme_classic() + ylab("Mean Error (log cm)") + 
  scale_x_discrete(breaks=c("yes_mean", "no_mean"), labels=c("Next to \n Landmark", "Not Next to \n Landmark")) + 
  theme(axis.text = element_text(size = 10, color = 'black')) 
#dev.off()

# for SD
landmark_ttest_sd <- landmark_data %>%
  group_by(subject, next_to_landmark) %>%
  summarise(
    sd = sd(placement_error_cm_log, na.rm = TRUE)
  )

landmark_ttest_sd$next_to_landmark <- factor(landmark_ttest_sd$next_to_landmark, levels = c("y", "n"))
landmark_near_sd <- ggboxplot(landmark_ttest_sd, x = "next_to_landmark", y = "sd", group = "subject", color = "black", size = 0.25,
                           add = "jitter") +
  xlab("") +
  ylab("Mean Standard Deviation (log cm)") +
  theme(legend.position = "none") +
  scale_x_discrete(breaks=c("y", "n"), labels=c("Next to Landmark", "Not Next to Landmark")) +
  theme(plot.title = element_text(hjust = 0.5))  +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75))
#jpeg("landmark_near_OAsd.jpeg", width = 7, height = 6, units = 'in', res = 500)
landmark_near_sd
#dev.off()

landmark_ttest_wide_sd <- spread(landmark_ttest_sd, key = next_to_landmark, value = sd)

t.test(landmark_ttest_wide_sd$y, landmark_ttest_wide_sd$n, paired = TRUE) # sig, p = .0002

yes_mean <- subset(landmark_ttest_sd, next_to_landmark == "y", mean, drop = TRUE)
no_mean <- subset(landmark_ttest_sd, next_to_landmark == "n", mean, drop = TRUE)

pd <- paired(yes_mean, no_mean)

#jpeg("landmark.jpeg", width = 3, height = 3, units = 'in', res = 300)
plot(pd, type = "profile") + theme_classic() + ylab("Mean Error (log cm)") + 
  scale_x_discrete(breaks=c("yes_mean", "no_mean"), labels=c("Next to \n Landmark", "Not Next to \n Landmark")) + 
  theme(axis.text = element_text(size = 10, color = 'black')) 
#dev.off()

##### Landmarks with YAs and OAs on the same plot
landmark_oaya <- oayaData %>%
  dplyr::select(c("subject", "next_to_landmark", "placement_error_cm_log", "group"))

landmark_oaya_stats <- landmark_oaya %>%
  group_by(subject, next_to_landmark, group) %>%
  summarise(
    mean = mean(placement_error_cm_log, na.rm = TRUE), 
    sd = sd(placement_error_cm_log, na.rm = TRUE)
  )

# FIGURE FOR MANUSCRIPT
landmark_oaya_stats$next_to_landmark <- factor(landmark_oaya_stats$next_to_landmark, levels = c("y", "n"))
landmark_oaya_plot <- ggboxplot(landmark_oaya_stats, x = "next_to_landmark", y = "mean", group = "subject", color = "black", size = 0.25,
                           add = "jitter", facet.by = "group") +
  xlab("") +
  ylab("Mean Error (log cm)") +
  theme(legend.position = "none") +
  scale_x_discrete(breaks=c("y", "n"), labels=c("Next to Landmark", "Not Next to Landmark")) +
  theme(plot.title = element_text(hjust = 0.5))  +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  facet_wrap(~group, labeller = as_labeller(c("OA" = "Older Adult", "YA" = "Younger Adult"))) +
  theme(
    strip.text = element_text(size = 12),   # Change facet label font size
    axis.text = element_text(size = 12),    # Change axis text size
    axis.title = element_text(size = 14),   # Change axis title size
    legend.text = element_text(size = 12),   # Change legend text size
    panel.border = element_rect(color = "black", fill = NA, size = .75) # Add border around each facet
  )
#jpeg("D:/Nav Stress Data/dissertation/pics/landmark_oaya_plot.jpeg", width = 8, height = 6, units = 'in', res = 500)
landmark_oaya_plot
#dev.off()


landmark_oaya_stats <- as.tibble(landmark_oaya_stats)

landmarks_aov <- anova_test(data = landmark_oaya_stats, dv = mean, wid = subject,
                             within = next_to_landmark, between = group)
get_anova_table(landmarks_aov) # they're all sig

# Bayes factor for this ANOVA
landmark_oaya_stats <- as.data.frame(landmark_oaya_stats)
bayes_rm <- anovaBF(mean ~ next_to_landmark*group + subject, data = landmark_oaya_stats, whichRandom = "subject")
bayes_rm
plot(bayes_rm)


TukeyHSD(landmarks_aov, which = "next_to_landmark")
# post-hoc tests
# simple main effect of group
one.way_group <- landmark_oaya_stats %>%
  group_by(next_to_landmark) %>%
  anova_test(dv = mean, wid = subject, between = group) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way_group

# pairwise comparison between group levels
pwc <- landmark_oaya_stats %>%
  group_by(next_to_landmark) %>%
  pairwise_t_test(mean ~ group, p.adjust.method = "bonferroni")
pwc

# simple main effect of landmark
one.way_landmark <- landmark_oaya_stats %>%
  group_by(group) %>%
  anova_test(dv = mean, wid = subject, within = next_to_landmark) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way_landmark

pwc2 <- landmark_oaya_stats %>%
  group_by(group) %>%
  pairwise_t_test(
    mean ~ next_to_landmark, paired = TRUE, 
    p.adjust.method = "bonferroni"
  ) 
pwc2

### now for SD
landmark_oaya_plot_sd <- ggboxplot(landmark_oaya_stats, x = "group", y = "sd", group = "subject", color = "black", size = 0.25,
                           add = "jitter", facet.by = "next_to_landmark") +
  xlab("") +
  ylab("Mean Error Standard Deviation (log cm)") +
  theme(legend.position = "none") +
  scale_x_discrete(breaks=c("OA", "YA"), labels=c("Older Adult", "Younger Adult")) +
  theme(plot.title = element_text(hjust = 0.5))  +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75)) +
  facet_wrap(~next_to_landmark, labeller = as_labeller(c("y" = "Next to Landmark", 
                                                         "n" = "Not Next to Landmark")))
#jpeg("landmark_near_OAmean.jpeg", width = 7, height = 6, units = 'in', res = 500)
landmark_oaya_plot_sd
#dev.off()

landmarks_aov_sd <- anova_test(data = landmark_oaya_stats, dv = sd, wid = subject,
                             within = next_to_landmark, between = group)
get_anova_table(landmarks_aov_sd) # next_to_landmark is sig only

# post-hoc tests
# simple main effect of group
one.way_group <- landmark_oaya_stats %>%
  group_by(next_to_landmark) %>%
  anova_test(dv = mean, wid = subject, between = group) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way_group

# pairwise comparison between group levels
pwc <- landmark_oaya_stats %>%
  group_by(next_to_landmark) %>%
  pairwise_t_test(mean ~ group, p.adjust.method = "bonferroni")
pwc

# simple main effect of landmark
one.way_landmark <- landmark_oaya_stats %>%
  group_by(group) %>%
  anova_test(dv = mean, wid = subject, within = next_to_landmark) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way_landmark

pwc2 <- landmark_oaya_stats %>%
  group_by(group) %>%
  pairwise_t_test(
    mean ~ next_to_landmark, paired = TRUE, 
    p.adjust.method = "bonferroni"
  ) 
pwc2 <- ggboxplot(landmark_oaya_stats, x = "next_to_landmark", y = "sd", group = "subject", color = "black", size = 0.25,
                                add = "jitter", facet.by = "group") +
  xlab("") +
  ylab("Mean Error Standard Deviation (log cm)") +
  theme(legend.position = "none") +
  scale_x_discrete(breaks=c("y", "n"), labels=c("Next to Landmark", "Not Next to Landmark")) +
  theme(plot.title = element_text(hjust = 0.5))  +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red", position = position_dodge(0.75))
#jpeg("landmark_near_OAmean.jpeg", width = 7, height = 6, units = 'in', res = 500)
landmark_oaya_plot
#dev.off()
landmark_oaya_stats <- as.tibble(landmark_oaya_stats)

landmarks_aov <- anova_test(data = landmark_oaya_stats, dv = mean, wid = subject,
                            within = next_to_landmark, between = group)
get_anova_table(landmarks_aov) # they're all sig

# post-hoc tests
# simple main effect of group
one.way_group <- landmark_oaya_stats %>%
  group_by(next_to_landmark) %>%
  anova_test(dv = mean, wid = subject, between = group) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way_group

# pairwise comparison between group levels
pwc <- landmark_oaya_stats %>%
  group_by(next_to_landmark) %>%
  pairwise_t_test(mean ~ group, p.adjust.method = "bonferroni")
pwc

# simple main effect of landmark
one.way_landmark <- landmark_oaya_stats %>%
  group_by(group) %>%
  anova_test(dv = mean, wid = subject, within = next_to_landmark) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way_landmark

pwc2 <- landmark_oaya_stats %>%
  group_by(group) %>%
  pairwise_t_test(
    mean ~ next_to_landmark, paired = TRUE, 
    p.adjust.method = "bonferroni"
  ) 
pwc2


########## Control analyses

wall_aov <- oayaData %>%
  group_by(group, subject, wall_side, start_wall) %>%
  summarize(
    mean = mean(placement_error_cm_log), 
    sd = sd(placement_error_cm_log)
  )
wall_aov <- as.tibble(wall_aov)

wall_bxp <- ggboxplot(
  wall_aov, x = "wall_side", y = "mean", 
  color = "start_wall", add = "jitter",
  xlab = "Wall Side", ylab = "Placement Error (log cm)") +
  stat_summary(aes(group = start_wall), fun = mean, geom = "point", shape = 18, size = 3, color = "black", position = position_dodge(0.75)) +
  facet_wrap(~group)
#jpeg("movement_viewpoint_bxp_OAYA_mean.jpeg", width = 7, height = 6, units = 'in', res = 500)
wall_bxp
#dev.off() 

# 2x2 repeated measures ANOVA mean and sd
wall_2way <- anova_test(data = wall_aov, dv = mean, wid = subject,
                             within = c(wall_side, start_wall), between = group)
get_anova_table(wall_2way) # group, group:wall_side, and wall_side:start_wall are sig

# post-hoc tests
# simple main effect of group
one.way_group <- wall_aov %>%
  group_by(wall_side) %>%
  anova_test(dv = mean, wid = subject, between = group) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way_group

# pairwise comparison between group levels - same as above because there's only two groups
pwc <- wall_aov %>%
  group_by(wall_side) %>%
  pairwise_t_test(mean ~ group, p.adjust.method = "bonferroni")
pwc

# need to clean the data so there's no duplicates for wall_side
# Summarize the data to remove duplicates by averaging the 'mean' values
wall_aov_clean <- wall_aov %>%
  group_by(subject, group, wall_side) %>%
  summarize(mean = mean(mean, na.rm = TRUE), .groups = "drop")

one.way_ws <- wall_aov_clean %>%
  group_by(group) %>%
  anova_test(dv = mean, wid = subject, within = wall_side) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way_ws

pwc2 <- wall_aov_clean %>%
  group_by(group) %>%
  pairwise_t_test(
    mean ~ wall_side, paired = TRUE, 
    p.adjust.method = "bonferroni"
  ) 
pwc2















