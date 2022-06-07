# Data file used for the manuscript results section
# Alana Muller
# amuller@email.arizona.edu
# 2022-03-23

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
inputData <- read_excel("E:/Nav_1stYr_project_data/manuscript_data_N30.xlsx")
inputData <- as.data.frame(inputData)
str(inputData) # check the structure of the data

# Make a copy of inputData that I'll use for analysis
myData <- inputData

# Make rows 1:12 and 16:19 factors
myData$subject <- as.factor(myData$subject)
myData$trial <- as.factor(myData$trial)
myData$wall_side <- as.factor(myData$wall_side)
myData$object <- as.factor(myData$object)
myData$object_size <- as.factor(myData$object_size)
myData$object_aliveORnot <- as.factor(myData$object_aliveORnot)
myData$object_material <- as.factor(myData$object_material)
myData$walk_noWalk <- as.factor(myData$walk_noWalk)
myData$same_diff <- as.factor(myData$same_diff)
myData$start_wall <- as.factor(myData$start_wall)
myData$height <- as.factor(myData$height)
myData$width <- as.factor(myData$width)
myData$next_to_landmark <- as.factor(myData$next_to_landmark)
myData$which_landmark <- as.factor(myData$which_landmark)
myData$`objects_put_back_order (same/not_same)`<- as.factor(myData$`objects_put_back_order (same/not_same)`)
myData$`cart (took/left/half)`<- as.factor(myData$`cart (took/left/half)`)

# Make rows 20:29 numbers
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

# Make sure the data looks ok now before proceeding
str(myData)

# Placement error (cm) is skewed to the right
hist(myData$placement_error_cm)

### Data transformations - they were not successful at making the data normal but log is the best

# Log transformation to make the placement error data more normal
myData$placement_error_cm_log <- log10(myData$placement_error_cm)
hist(myData$placement_error_cm_log)
shapiro.test(myData$placement_error_cm_log) # still not normal
ggqqplot(myData$placement_error_cm_log)

myData$placement_error_cm_cuberoot <- (myData$placement_error_cm)^(1/3)
hist(myData$placement_error_cm_cuberoot)
shapiro.test(myData$placement_error_cm_cuberoot) # still not normal, log is better
ggqqplot(myData$placement_error_cm_cuberoot)

### Get rid of outliers of placement_error_cm past 3 SD of transformed data
mean_placement_error_cm_log <- mean(myData$placement_error_cm_log, na.rm = TRUE)
sd_placement_error_cm_log <- sd(myData$placement_error_cm_log, na.rm = TRUE)

# 13 outliers identified 3 SD away from the mean
outliers_placement_error_cm <- myData %>%
  filter(placement_error_cm_log > mean_placement_error_cm_log + (3*sd_placement_error_cm_log) | 
         placement_error_cm_log < mean_placement_error_cm_log - (3*sd_placement_error_cm_log))

# Make a dataset with No Outliers (NO for short) and there are also no more NAs in placement_error_cm_log
myData_NO <- myData %>% 
  filter(placement_error_cm_log < mean_placement_error_cm_log + (3*sd_placement_error_cm_log), 
           placement_error_cm_log > mean_placement_error_cm_log - (3*sd_placement_error_cm_log) )

# Check normality assumption on NO data - still not normal but looks better
hist(myData_NO$placement_error_cm_log)
ggqqplot(myData_NO$placement_error_cm_log)
shapiro.test(myData_NO$placement_error_cm_log) # p = 1.228e-06

### Transform abs value of x_error_cm and y_error_cm
myData$abs_x_error_cm_log <- log10(myData$abs_x_error_cm+1)
myData$abs_y_error_cm_log <- log10(myData$abs_y_error_cm+1)

# Get rid of outliers from abs value of x_error_cm
mean_x_error_cm_log <- mean(myData$abs_x_error_cm_log, na.rm = TRUE)
sd_x_error_cm_log <- sd(myData$abs_x_error_cm_log, na.rm = TRUE)

# 2 outliers found
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

# Make a dataset without outliers for x_error_cm
myData_NO_y <- myData %>%
  filter(abs_y_error_cm_log < mean_y_error_cm_log + (3*sd_y_error_cm_log), 
           abs_y_error_cm_log > mean_y_error_cm_log - (3*sd_y_error_cm_log))


################### Parametric Analyses ###################  

##### 2-way repeated-measures ANOVA walk view - not sig
aov_data <- myData_NO %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    mean = mean(placement_error_cm_log, na.rm = TRUE),
  )
aov_data <- as_tibble(aov_data)

bxp <- ggboxplot(
  aov_data, x = "walk_noWalk", y = "mean", 
  color = "same_diff", add = "jitter", 
  xlab = "Movement Condition", ylab = "Placement Error (log cm)",
  legend = "right", legend.title = "Viewpoint") + 
  scale_x_discrete(breaks=c("no walk", "walk"), labels=c("Stationary", "Walk")) +
  scale_color_discrete(labels = c("Different", "Same"))
#jpeg("movement_viewpoint_anova.jpeg", width = 3.5, height = 3, units = 'in', res = 300)
bxp
#dev.off()

# these are two ways to do a 2x2 repeated measures ANOVA
results_2way <- aov(mean ~ walk_noWalk*same_diff + Error(subject/(walk_noWalk*same_diff)), data = aov_data)
summary(results_2way) # nothing is sig, no main effects, no interaction effect

withinTest <- anova_test(data = aov_data, dv = mean, wid = subject,
                         within = c(walk_noWalk, same_diff))
get_anova_table(withinTest) # nothing is sig

### exclude trials when people took the cart AND put back in the same order and halfs
### all other trials can stay, just don't want people retracing their steps

cart <- myData_NO %>%
  group_by(subject,trial, `cart (took/left/half)`, `objects_put_back_order (same/not_same)`) %>%
  summarize(
    count = n(),
    mean = mean(placement_error_cm_log), 
    sd = sd(placement_error_cm_log)
  )

# make groups of each pair of conditions to see how many of each there are
cart_took_same <- myData_NO %>%
  filter(`cart (took/left/half)`== "took" & `objects_put_back_order (same/not_same)` == "same")

cart_took_notSame <- myData_NO %>%
  filter(`cart (took/left/half)`== "took" & `objects_put_back_order (same/not_same)` == "not_same")

cart_left_same <- myData_NO %>%
  filter(`cart (took/left/half)`== "left" & `objects_put_back_order (same/not_same)` == "same")

cart_left_notSame <- myData_NO %>%
  filter(`cart (took/left/half)`== "left" & `objects_put_back_order (same/not_same)` == "not_same")

cart_half_same <- myData_NO %>%
  filter(`cart (took/left/half)`== "half" & `objects_put_back_order (same/not_same)` == "same")

cart_half_notSame <- myData_NO %>%
  filter(`cart (took/left/half)`== "half" & `objects_put_back_order (same/not_same)` == "not_same")

# put the data together that I want (took, not same; left, not same; left, same)
cart_data <- myData_NO %>%
  filter(`cart (took/left/half)`== "took" & `objects_put_back_order (same/not_same)` == "not_same" |
         `cart (took/left/half)`== "left" & `objects_put_back_order (same/not_same)` == "not_same" |
         `cart (took/left/half)`== "left" & `objects_put_back_order (same/not_same)` == "same" |
         `cart (took/left/half)`== "half" & `objects_put_back_order (same/not_same)` == "not_same")

aov_cart_data <- cart_data %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    mean = mean(placement_error_cm_log, na.rm = TRUE),
  )
aov_cart_data <- as_tibble(aov_cart_data)

bxp <- ggboxplot(
  aov_cart_data, x = "walk_noWalk", y = "mean", 
  color = "same_diff", add = "jitter", 
  xlab = "Movement Condition", ylab = "Placement Error (log cm)",
  legend = "right", legend.title = "Viewpoint") + 
  scale_x_discrete(breaks=c("no walk", "walk"), labels=c("Stationary", "Walk")) +
  scale_color_discrete(labels = c("Different", "Same"))
bxp

# this 2-way repeated measures anova takes out the incomplete cases
cart_withinTest <- anova_test(data = aov_cart_data, dv = mean, wid = subject,
                              within = c(walk_noWalk, same_diff))
get_anova_table(cart_withinTest) # not sig but p = 0.08 for walk_noWalk


### 2-way ANOVA with abs x error
aov_x_data <- myData_NO_x %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    mean = mean(abs_x_error_cm_log, na.rm = TRUE),
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

# 2x2 repeated measures ANOVA - not sig
results_x_2way <- aov(mean ~ walk_noWalk*same_diff + Error(subject/(walk_noWalk*same_diff)), data = aov_x_data)
summary(results_x_2way) # nothing is sig, no main effects, no interaction effect

### 2-way ANOVA with abs y error
aov_y_data <- myData_NO_y %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    mean = mean(abs_y_error_cm_log, na.rm = TRUE),
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


# 2x2 repeated measures ANOVA - only movement condition was sig p = 0.046 but very small numerical diff and wasn't there for non log transformed data
results_y_2way <- aov(mean ~ walk_noWalk*same_diff + Error(subject/(walk_noWalk*same_diff)), data = aov_y_data)
summary(results_y_2way) # movement sig p = 0.046


##### abs value of x coordinate vs y coordinate accuracy - paired t-test
xy_data <- myData %>%
  group_by(subject) %>%
  summarise(
    x_cm_mean = mean((abs_x_error_cm_log), na.rm = TRUE),
    y_cm_mean = mean((abs_y_error_cm_log), na.rm = TRUE)
  )
t.test(xy_data$x_cm_mean, xy_data$y_cm_mean, paired = TRUE) # p = .001178

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

mean(xy_data$x_cm_mean) # 1.10
sd(xy_data$x_cm_mean) # 0.15

mean(xy_data$y_cm_mean) # 1.04
sd(xy_data$y_cm_mean) # 0.12

# untransformed
(10^mean(xy_data$x_cm_mean))-1 # 11.51
(10^sd(xy_data$x_cm_mean))-1 # 0.43

(10^mean(xy_data$y_cm_mean))-1 # 9.85
(10^sd(xy_data$y_cm_mean))-1 # 0.31


# I don't think this is the way to scale it. Doesn't make sense.
xy_data$x_cm_mean_scaled <- xy_data$x_cm_mean/6
xy_data$y_cm_mean_scaled <- xy_data$y_cm_mean/2.5

t.test(xy_data$x_cm_mean_scaled, xy_data$y_cm_mean_scaled, paired = TRUE)

##### Landmark and placement accuracy

landmark_data <- myData_NO %>%
  select(c("subject", "next_to_landmark", "placement_error_cm_log"))

landmark_ttest <- landmark_data %>%
  group_by(subject, next_to_landmark) %>%
  summarise(
    mean = mean(placement_error_cm_log, na.rm = TRUE)
  )

landmark_ttest_long <- spread(landmark_ttest, key = next_to_landmark, value = mean)

t.test(landmark_ttest_long$y, landmark_ttest_long$n, paired = TRUE) # sig, p = .002

yes_mean <- subset(landmark_ttest, next_to_landmark == "y", mean, drop = TRUE)
no_mean <- subset(landmark_ttest, next_to_landmark == "n", mean, drop = TRUE)

pd <- paired(yes_mean, no_mean)

jpeg("landmark.jpeg", width = 3, height = 3, units = 'in', res = 300)
plot(pd, type = "profile") + theme_classic() + ylab("Mean Error (log cm)") + 
  scale_x_discrete(breaks=c("yes_mean", "no_mean"), labels=c("Next to \n Landmark", "Not Next to \n Landmark")) + 
  theme(axis.text = element_text(size = 10, color = 'black')) 
dev.off()

mean(landmark_ttest_long$y) # 1.26
sd(landmark_ttest_long$y) # 0.20

mean(landmark_ttest_long$n) # 1.33
sd(landmark_ttest_long$n) # 0.13

# untransformed
(10^mean(landmark_ttest_long$y)) # 18.27
(10^sd(landmark_ttest_long$y)) # 1.57

(10^mean(landmark_ttest_long$n)) # 21.54
(10^sd(landmark_ttest_long$n)) # 1.36

################### Non-Parametric Analyses ###################  

# create columns in the data for absolute value of x and y error
myData$abs_x_error_cm <- abs(myData$x_error_cm)
myData$abs_y_error_cm <- abs(myData$y_error_cm)

### rep measures npar ANOVA - main analysis - all data
library(nparLD)

# data grouped by subject, movement condition, and viewpoint condition
nparData <- myData %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    median = median(placement_error_cm, na.rm = TRUE),
  )

ex.f2 <- ld.f2(y = nparData$median, 
               time1 = nparData$walk_noWalk, 
               time2 = nparData$same_diff,
               subject = nparData$subject,
               time1.name = "Movement", 
               time2.name = "Viewpoint", description = TRUE,
               time1.order = c("walk", "no walk") ,
               time2.order = c("same", "diff"))

# ANOVA-type statistic
ex.f2$ANOVA.test # nothing sig

### rep measures npar ANOVA - excluding "same" for objects_put_back_order
### analysis represents people that did not simply retrace their steps
npar_noSame <- myData %>%
  filter(`objects_put_back_order (same/not_same)` != "same")

npar_noSameData <- npar_noSame %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    median = median(placement_error_cm)
  )

# taking out subjects that don't have data in all 4 conditions (14 subjects - yikes)
npar_n16 <- npar_noSameData %>%
  filter(subject != 2 & subject != 4 & subject != 6 & subject != 9 & subject != 12 & subject != 15 & subject != 16 
         & subject != 20 & subject != 23 & subject != 25 & subject != 26 & subject != 27 & subject != 28 & subject != 29)

ex.f2_n16 <- ld.f2(y = npar_n16$median, 
               time1 = npar_n16$walk_noWalk, 
               time2 = npar_n16$same_diff,
               subject = npar_n16$subject,
               time1.name = "Movement", 
               time2.name = "Viewpoint", description = TRUE,
               time1.order = c("walk", "no walk") ,
               time2.order = c("same", "diff"))

# ANOVA-type statistic
ex.f2_n16$ANOVA.test # nothing sig

# objects put back order: same vs not same for each subject - npar paired t-test
sameVnotSame <- myData %>%
  group_by(subject, `objects_put_back_order (same/not_same)`) %>%
  summarize(
    median = median(placement_error_cm, na.rm = TRUE)
  )

sameVnotSame <- spread(sameVnotSame, `objects_put_back_order (same/not_same)`, median)
sameVnotSame <- na.omit(sameVnotSame) # Ss 6, 11, 15, 16, 22, 23, 25, 26 excluded bc they didn't have data for both conditions

# actual test: npar paired samples t-test
wilcox.test(sameVnotSame$not_same, sameVnotSame$same, paired = TRUE) # p = .8987

##### horizontal vs vertical placement error (x and y values)

# most broad test: all x_error vs all y_error, paired
wilcox.test(myData$x_error_cm, myData$y_error_cm, paired = TRUE) # p = .003, but I don't think this is the right grouping

xy_graph <- myData %>%
  select(c("x_error_cm", "y_error_cm"))
xy_graph <- gather(xy_graph, key = "type_error", value = "median", 1:2)
ggboxplot(xy_graph, x = "type_error", y = "median", add = "jitter",
          color = "type_error", ylab = "Placement Error (cm)", 
          xlab = "Type of Error") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), legend.position = "none")

wilcox.test(myData$x_error_cm, myData$y_error_cm, paired = TRUE) # p < .003
median(myData$x_error_cm, na.rm = TRUE) # -0.3907
median(myData$y_error_cm, na.rm = TRUE) # 1.1715

### get a value for each person

subj_xy_data <- myData %>%
  group_by(subject) %>%
  summarize(
    x_cm_median = median(x_error_cm, na.rm = TRUE),
    y_cm_median = median(y_error_cm, na.rm = TRUE)
  )

wilcox.test(subj_xy_data$x_cm_median, subj_xy_data$y_cm_median, paired = TRUE) # not sig, p = .05845

### get a median value for each trial and then for each subject
npar_xy_data <- myData %>%
  group_by(subject, trial) %>%
  summarize(
    x_cm_median = median(x_error_cm, na.rm = TRUE),
    y_cm_median = median(y_error_cm, na.rm = TRUE)
  )

npar_xy_data <- npar_xy_data %>%
  group_by(subject) %>%
  summarize(
    x_cm_median = median(x_cm_median, na.rm = TRUE), 
    y_cm_median = median(y_cm_median, na.rm = TRUE)
  )

wilcox.test(npar_xy_data$x_cm_median, npar_xy_data$y_cm_median, paired = TRUE) # not sig, p = .1294

npar_xy_graph <- gather(npar_xy_data, key = "type_error", value = "median", "x_cm_median":"y_cm_median")
ggboxplot(npar_xy_graph, x = "type_error", y = "median", add = "jitter",
          color = "type_error", ylab = "Placement Error (cm)", 
          xlab = "Type of Error") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), legend.position = "none")

### using the absolute value of x_error_cm and y_error_cm

# most broad test: all abs_x_error vs all abs_y_error, paired
wilcox.test(myData$abs_x_error_cm, myData$abs_y_error_cm, paired = TRUE) # p < .001
median(myData$abs_x_error_cm, na.rm = TRUE) # 11.3303
median(myData$abs_y_error_cm, na.rm = TRUE) # 10.5435

abs_xy_graph <- myData %>%
  select(c("abs_x_error_cm", "abs_y_error_cm"))
abs_xy_graph <- gather(abs_xy_graph, key = "type_error", value = "median", 1:2)
ggboxplot(abs_xy_graph, x = "type_error", y = "median", add = "jitter",
          color = "type_error", ylab = "Placement Error (cm)", 
          xlab = "Type of Error") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), legend.position = "none")

# get a median for each trial and then for each subject
abs_xy_data <- myData %>%
  group_by(subject, trial) %>%
  summarize(
    x_cm_median = median(abs_x_error_cm, na.rm = TRUE),
    y_cm_median = median(abs_y_error_cm, na.rm = TRUE)
  )

abs_xy_data <- abs_xy_data %>%
  group_by(subject) %>%
  summarize(
    x_cm_median = median(x_cm_median, na.rm = TRUE), 
    y_cm_median = median(y_cm_median, na.rm = TRUE)
  )

wilcox.test(abs_xy_data$x_cm_median, abs_xy_data$y_cm_median, paired = TRUE) # not sig, p = .1519

abs_xy_graph <- gather(abs_xy_data, key = "type_error", value = "median", "x_cm_median":"y_cm_median")
ggboxplot(abs_xy_graph, x = "type_error", y = "median", add = "jitter",
          color = "type_error", ylab = "Placement Error (cm)", 
          xlab = "Type of Error") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), legend.position = "none")


plot(abs_xy_data$x_cm_median, abs_xy_data$y_cm_median)
cor.test(abs_xy_data$x_cm_median, abs_xy_data$y_cm_median, method = "spearman")

### npar rep-measure ANOVA for x values

x_nparData <- myData %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    median = median(x_error_cm, na.rm = TRUE),
  )

ex.f2_x <- ld.f2(y = x_nparData$median, 
               time1 = x_nparData$walk_noWalk, 
               time2 = x_nparData$same_diff,
               subject = x_nparData$subject,
               time1.name = "Movement", 
               time2.name = "Viewpoint", description = TRUE,
               time1.order = c("walk", "no walk") ,
               time2.order = c("same", "diff"))

# ANOVA-type statistic
ex.f2_x$ANOVA.test # nothing sig

# abs value of x error
abs_x_nparData <- myData %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    median = median(abs_x_error_cm, na.rm = TRUE),
  )

ex.f2_abs_x <- ld.f2(y = abs_x_nparData$median, 
                 time1 = abs_x_nparData$walk_noWalk, 
                 time2 = abs_x_nparData$same_diff,
                 subject = abs_x_nparData$subject,
                 time1.name = "Movement", 
                 time2.name = "Viewpoint", description = TRUE,
                 time1.order = c("walk", "no walk") ,
                 time2.order = c("same", "diff"))

# ANOVA-type statistic
ex.f2_abs_x$ANOVA.test # nothing sig

### npar rep-measure ANOVA for y values
y_nparData <- myData %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    median = median(y_error_cm, na.rm = TRUE),
  )

ex.f2_y <- ld.f2(y = y_nparData$median, 
                 time1 = y_nparData$walk_noWalk, 
                 time2 = y_nparData$same_diff,
                 subject = y_nparData$subject,
                 time1.name = "Movement", 
                 time2.name = "Viewpoint", description = TRUE,
                 time1.order = c("walk", "no walk") ,
                 time2.order = c("same", "diff"))

# ANOVA-type statistic
ex.f2_y$ANOVA.test # movement main effect is sig, p = .0282

# abs value of y error
abs_y_nparData <- myData %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    median = median(abs_y_error_cm, na.rm = TRUE),
  )

ex.f2_abs_y <- ld.f2(y = abs_y_nparData$median, 
                     time1 = abs_y_nparData$walk_noWalk, 
                     time2 = abs_y_nparData$same_diff,
                     subject = abs_y_nparData$subject,
                     time1.name = "Movement", 
                     time2.name = "Viewpoint", description = TRUE,
                     time1.order = c("walk", "no walk") ,
                     time2.order = c("same", "diff"))

# ANOVA-type statistic
ex.f2_abs_y$ANOVA.test # nothing sig

################### Preliminary Eye Tracking Data ####################


# placement_error_cm
ggscatter(myData_NO, x = "Total_duration_of_fixations", y = "placement_error_cm_log", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.coeff.args = list(method = "pearson", label.x = 4000, label.sep = "\n"), 
          xlab = "Total duration of fixations", ylab = "Placement Error (log cm)") # sig

# filter out 0 duration of fixations and too long total fixation time
duration_filter <- myData_NO %>%
  filter(myData_NO$Total_duration_of_fixations < 4000 & Total_duration_of_fixations != 0)

#jpeg("eye_total_duration.jpeg", width = 4, height = 3, units = 'in', res = 300)
ggscatter(duration_filter, x = "Total_duration_of_fixations", y = "placement_error_cm_log", add = "reg.line", conf.int = TRUE, size = 1,
          cor.coef = TRUE, cor.coeff.args = list(method = "pearson", label.x = 3050, label.y = 2.3, label.sep = "\n"), 
          xlab = "Total duration of fixations", ylab = "Placement Error (log cm)") # sig
#dev.off()

ggscatter(myData_NO, x = "Number_of_fixations", y = "placement_error_cm_log", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.coeff.args = list(method = "pearson", label.x = 10, label.sep = "\n"), 
          xlab = "Number of fixations", ylab = "Placement Error (log cm)") # sig
fixation_filter <- myData_NO %>%
  filter(myData_NO$Number_of_fixations < 11 & myData_NO$Number_of_fixations > 0)

#jpeg("eye_fixation_number.jpeg", width = 4, height = 3, units = 'in', res = 300)
ggscatter(fixation_filter, x = "Number_of_fixations", y = "placement_error_cm_log", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.coeff.args = list(method = "pearson", label.x = 7.5, label.y = 2.3, label.sep = "\n"), 
          xlab = "Number of fixations", ylab = "Placement Error (log cm)") # sig
#dev.off()
