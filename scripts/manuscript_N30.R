# Data file used for the manuscript results section

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

# Make sure the data looks ok now before proceeding
str(myData)

# Placement error (cm) is skewed to the right
hist(myData$placement_error_cm)

# Log transformation to make the placement error data more normal

myData$placement_error_cm_log <- log(myData$placement_error_cm)
hist(myData$placement_error_cm_log)

### Get rid of outliers past 3 SD of transformed data
mean_placement_error_cm_log <- mean(myData$placement_error_cm_log, na.rm = TRUE)
sd_placement_error_cm_log <- sd(myData$placement_error_cm_log, na.rm = TRUE)

# 23 outliers identified beyond 3 SD away from the mean
outliers <- myData %>%
  filter(placement_error_cm_log > mean_placement_error_cm_log + (3*sd_placement_error_cm_log))

# Make a dataset with No Outliers (NO for short) and there are also no more NAs in this column
myData_NO <- myData %>%
  filter(placement_error_cm_log < mean_placement_error_cm_log + (3*sd_placement_error_cm_log))

# Check normality assumption - still not normal but not as bad
hist(myData_NO$placement_error_cm_log)
ggqqplot(myData_NO$placement_error_cm_log)
shapiro.test(myData_NO$placement_error_cm_log)


################### Parametric Analyses ###################  

##### 2-way repeated-measures ANOVA walk view - not sig

bxp <- ggboxplot(
  myData_NO, x = "walk_noWalk", y = "placement_error_cm_log", 
  color = "same_diff"
)
bxp

aov_data <- myData_NO %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    mean = mean(placement_error_cm_log, na.rm = TRUE),
  )
aov_data <- as_tibble(aov_data)

# these are two ways to do a 2x2 repeated measures ANOVA
results_2way <- aov(mean ~ walk_noWalk*same_diff + Error(subject/(walk_noWalk*same_diff)), data = aov_data)
summary(results_2way) # nothing is sig, no main effects, no interaction effect

withinTest <- anova_test(data = aov_data, dv = mean, wid = subject,
                         within = c(walk_noWalk, same_diff))
get_anova_table(withinTest) # nothing is sig

### how many people took the cart and put items back in the same order?
# it'll take a little more to figure this out, some people did, some didn't

cart <- myData_NO %>%
  group_by(subject,trial, `cart (took/left/half)`, `objects_put_back_order (same/not_same)`) %>%
  summarize(
    count = n(),
    mean = mean(placement_error_cm_log), 
    sd = sd(placement_error_cm_log)
  )

cart_data <- myData_NO %>%
  filter(`cart (took/left/half)`== "left" & `objects_put_back_order (same/not_same)` == "not_same")

bxp <- ggboxplot(
  cart_data, x = "walk_noWalk", y = "placement_error_cm_log", 
  color = "same_diff"
)
bxp

aov_cart_data <- cart_data %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    mean = mean(placement_error_cm_log, na.rm = TRUE),
  )
aov_cart_data <- as_tibble(aov_cart_data)

# this 2-way repeated measures anova takes out the incomplete cases
# only 17 Ss left
cart_withinTest <- anova_test(data = aov_cart_data, dv = mean, wid = subject,
                              within = c(walk_noWalk, same_diff))
get_anova_table(cart_withinTest) # not sig but p = 0.08 for walk_noWalk

# exclude all object order put back == same (more flexible that analysis above) - not sig

cart_flex <- myData_NO %>%
  filter(`objects_put_back_order (same/not_same)`!= "same")

aov_cart_flex <- cart_flex %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    mean = mean(placement_error_cm_log, na.rm = TRUE)
  )
aov_cart_flex <- as_tibble(aov_cart_flex)

cart_flex_within_test <- anova_test(data = aov_cart_flex, dv = mean, wid = subject,
                                    within = c(walk_noWalk, same_diff))
get_anova_table(cart_flex_within_test)


##### x coordinate vs y coordinate accuracy - paired t-test - not sig but not the right test

xy_data <- myData_NO %>%
  group_by(subject) %>%
  summarise(
    x_cm_mean = mean(x_error_cm),
    y_cm_mean = mean(y_error_cm)
  )
t.test(xy_data$x_cm_mean, xy_data$y_cm_mean, paired = TRUE)

xy_data_long <- xy_data %>%
  gather(error_type, mean_error, x_cm_mean, y_cm_mean)

x_mean <- subset(xy_data_long, error_type == "x_cm_mean", mean_error, drop = TRUE)
y_mean <- subset(xy_data_long, error_type == "y_cm_mean", mean_error, drop = TRUE)

pd <- paired(x_mean, y_mean)
plot(pd, type = "profile") + theme_bw()





################### Non-Parametric Analyses ###################  
