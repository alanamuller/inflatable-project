# Nav Stress Prelim Behavioral Analyses

library(openxlsx)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(rstatix)

rm(list = ls())

##### Read in file with subject, condition, and city data
setwd("E:/Nav Stress Data/Participant_data/") # set working directory

myData <- read.xlsx("navTrialsLog.xlsx") # read in file

# Make some columns factors
myData$subjectID <- as.factor(myData$subjectID)
myData$block <- as.factor(myData$block)
myData$trial <- as.factor(myData$trial)
myData$condition <- as.factor(myData$condition)
myData$city <- as.factor(myData$city)
myData$trial_type <- as.factor(myData$trial_type)

########## Histograms
hist(myData$Navigate_excessPath)
hist(myData$Navigate_duration)

### Get rid of outliers in excess path and duration

mean_excess <- mean(myData$Navigate_excessPath)
sd_excess <- sd(myData$Navigate_excessPath)

mean_duration <- mean(myData$Navigate_duration)
sd_duration <- sd(myData$Navigate_duration)

cleanData <- subset(myData, myData$Navigate_excessPath > mean_excess - sd_excess*2.5 & myData$Navigate_excessPath < mean_excess + sd_excess*2.5)
cleanData <- subset(cleanData, cleanData$Navigate_duration > mean_duration - sd_duration*2.5 & cleanData$Navigate_duration < mean_duration + sd_duration*2.5)

hist(cleanData$Navigate_excessPath)
hist(cleanData$Navigate_duration)

### Data has a positive skew - log transform
# There are negative numbers in the Navigate_excessPath column, find them
neg_nums <- cleanData$Navigate_excessPath[cleanData$Navigate_excessPath < 0]
# Delete the negative nums
cleanData <- subset(cleanData, cleanData$Navigate_excessPath > 0)
# Log transform
cleanData$log_excessPath <- log(cleanData$Navigate_excessPath)
hist(cleanData$log_excessPath) # so normal now wow
ggqqplot(cleanData$log_excessPath)

# Log transform duration
cleanData$log_duration <- log(cleanData$Navigate_duration)
hist(cleanData$log_duration) # better but still not great
ggqqplot(cleanData$log_duration)

ggboxplot(cleanData, x = "subjectID", y = "log_excessPath", facet.by = "trial")

ggqqplot(log(myData$Navigate_excessPath))

# Collapse the data over trials
blockCondition <- cleanData %>%
  group_by(subjectID, condition, block) %>%
  summarize(mean_log_excess = mean(log_excessPath, na.rm = TRUE), 
            sd_log_excess = sd(log_excessPath, na.rm = TRUE),
            mean_log_dur = mean(log_duration, na.rm = TRUE), 
            sd_log_dur = sd(log_duration, na.rm = TRUE))

blockCondition <- as.data.frame(blockCondition) # make into a dataframe because anova doesn't like tibble

blockTrialType <- cleanData %>%
  group_by(subjectID, trial_type, block) %>%
  summarize(mean_log_excess = mean(log_excessPath, na.rm = TRUE), 
            sd_log_excess = sd(log_excessPath, na.rm = TRUE),
            mean_log_dur = mean(log_duration, na.rm = TRUE), 
            sd_log_dur = sd(log_duration, na.rm = TRUE))

blockTrialType <- as.data.frame(blockTrialType) # make into a dataframe because anova doesn't like tibble

########## Some quick anova's
# anova - log excess path by block and condition
res.aov_excess <- anova_test(data = blockCondition, dv = mean_log_excess, wid = subjectID, within = c(condition, block))
get_anova_table(res.aov_excess)

# anova - log duration by block and condition
res.aov_dur <- anova_test(data = blockCondition, dv = mean_log_dur, wid = subjectID, within = c(condition, block))
get_anova_table(res.aov_dur)

# anova - log excess path by block and trial type
res.aov_excess_tt <- anova_test(data = blockTrialType, dv = mean_log_excess, wid = subjectID, within = c(trial_type, block))
get_anova_table(res.aov_excess_tt)

# anova - log duration by block and trial type
res.aov_dur_tt <- anova_test(data = blockTrialType, dv = mean_log_dur, wid = subjectID, within = c(trial_type, block))
get_anova_table(res.aov_dur_tt)

# anova - trial type

##### box plots to visualize data
# Log excess path
bxp_excess <- ggboxplot(
  cleanData, x = "condition", y = "log_excessPath", 
  color = "block", add = "jitter")
bxp_excess

# Log duration
bxp_dur <- ggboxplot(
  cleanData, x = "condition", y = "log_duration", 
  color = "block", add = "jitter")
bxp_dur

# Trial type
bxp_trialType_excess <- ggboxplot(
  cleanData, x = "trial_type", y = "log_excessPath", 
  color = "block", add = "jitter")
bxp_trialType_excess

bxp_trialType_dur <- ggboxplot(
  cleanData, x = "trial_type", y = "log_duration", 
  color = "block", add = "jitter")
bxp_trialType_dur



