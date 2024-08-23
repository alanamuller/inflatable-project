# Overlap Stats

##### Navigation Trials Overlap Stats
# Did people previously learned routes or new ones to get to their destination?

# Try ARTools for nonparametric factorial ANOVA

library(openxlsx)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(ARTool)
library(openxlsx)

rm(list = ls())

##### Read in data
setwd("E:/Nav Stress Data/") # set working directory
# setwd("C:/Users/amuller/Desktop/Alana/UA/HSCL/Stress shortcuts") # for developing

################################# look at nav trials data

originalData <- read.csv("combined_navTrialsLogData.csv") # keep a copy of original data
navTrialsData <- originalData # work with this copy of the data

sotData <- read.xlsx("SOT_data.xlsx") # read in the SOT data

# make the correct variables factors
navTrialsData$subjectID <- as.factor(navTrialsData$subjectID)
navTrialsData$block <- as.factor(navTrialsData$block)
navTrialsData$trial <- as.factor(navTrialsData$trial)
navTrialsData$target_store_num <- as.factor(navTrialsData$target_store_num)
navTrialsData$startEnd_store <- as.factor(navTrialsData$startEnd_store)
navTrialsData$condition <- as.factor(navTrialsData$condition)
navTrialsData$moreFamiliarPath <- as.factor(navTrialsData$moreFamiliarPath)
navTrialsData$city <- as.factor(navTrialsData$city)
navTrialsData$trial_type <- as.factor(navTrialsData$trial_type)

# add new columns totaling outer and inner use
navTrialsData$total_outer_use <- navTrialsData$grid_overlap_outer_seg1 + navTrialsData$grid_overlap_outer_seg2 + navTrialsData$grid_overlap_outer_seg3 + navTrialsData$grid_overlap_outer_seg4
navTrialsData$total_inner_use <- navTrialsData$grid_overlap_inner_seg1 + navTrialsData$grid_overlap_inner_seg2 + navTrialsData$grid_overlap_inner_seg3 + navTrialsData$grid_overlap_inner_seg4

# add new column for log grid_count
navTrialsData$log_grid_count <- log(navTrialsData$grid_count)

# add new columns categorizing more familiar grid type and less familiar grid type collapsing across path type
for (i in 1:nrow(navTrialsData)) {
  if (navTrialsData$moreFamiliarPath[i] == "outer") {
    navTrialsData$moreFamGridNum[i] <- navTrialsData$total_outer_use[i]
    navTrialsData$lessFamGridNum[i] <- navTrialsData$total_inner_use[i]
  } else if (navTrialsData$moreFamiliarPath[i] == "inner") {
    navTrialsData$moreFamGridNum[i] <- navTrialsData$total_inner_use[i]
    navTrialsData$lessFamGridNum[i] <- navTrialsData$total_outer_use[i]
  }
}

# Exclude the freebie trials from city 3 (when the start and target store were the same) by excluding participants who traveled less than 300 units on a trial (10 trials)
navTrialsData <- subset(navTrialsData, Navigate_actualPath > 300)
# exclude trials with too many negative excess block numbers - coding error to be fixed soon on city 3 trial 13
navTrialsData <- subset(navTrialsData, excess_block_num > -3)
# exclude trials where Ss got hopelessly lost
# find outliers in grid count and we'll call them lost
mean_gridCount <- mean(navTrialsData$grid_count)
sd_gridCount <- sd(navTrialsData$grid_count)

# create dataset with no lost trials
no_lost_navTrials <- subset(navTrialsData, grid_count < mean_gridCount+(2.5*sd_gridCount))

hist(no_lost_navTrials$grid_count) # still skewed but better
hist(log(no_lost_navTrials$grid_count))
shapiro.test(log(no_lost_navTrials$grid_count)) # still skewed, doesn't pass shapiro test

# create dataset with only lost trials
lost_trials <- which(navTrialsData$grid_count > mean_gridCount+(2.5*sd_gridCount))
length(lost_trials) # 76 lost trials

lost_Ss <- subset(navTrialsData, grid_count > mean_gridCount+(2.5*sd_gridCount))
hist(lost_Ss$grid_count)

#################### What percentage of new/old/novel paths were taken on no lost trials?

# gather total inner and outer use and novel grids
# name the columns you want to gather, the other columns will remain there
gathered_columns <- c("novel_grids", "total_outer_use", "total_inner_use")

# gather the data for grid numbers
longNav <- gather(no_lost_navTrials, key = grid_type, value = grid_number, gathered_columns, factor_key = TRUE)

# one data point per person per grid type
nav_summary <- longNav %>%
  group_by(subjectID, grid_type, moreFamiliarPath, condition) %>%
  summarize(
    count = n(),
    mean_grid_number = mean(grid_number, na.rm = TRUE), 
    sd_grid_number = sd(grid_number, na.rm = TRUE),
    median_grid_number = median(grid_number, na.rm = TRUE),
    IQR_grid_number = IQR(grid_number, na.rm = TRUE)
  )

# check for outliers
medianGrid <- median(nav_summary$median_grid_number)
iqrGrid <- IQR(nav_summary$IQR_grid_number)

upperGridRange <- medianGrid + (2.5*iqrGrid)

NO_nav_summary <- subset(nav_summary, median_grid_number < upperGridRange)

# graph for iNAV
wrap_labels <- c("Inner More Familiar", "Outer More Familiar")
names(wrap_labels) <- c("inner", "outer")
tick_labels <- c("Novel Grids", "Outer Grids", "Inner Grids")

navTrialsMedian <- ggplot(NO_nav_summary, aes(x = grid_type, y = median_grid_number, fill = condition)) + 
  geom_boxplot(outliers = FALSE) + geom_jitter(position = position_jitterdodge()) +
  labs(x = "Grid Type", y = "Median Grid Number", fill = "Condition") +
  scale_x_discrete(labels = tick_labels) + 
  scale_fill_discrete(name = "Condition", labels = c("Cold Pressor", "Control", "Fire Environment"), type = c("deep sky blue", "lime green", "salmon")) +
  facet_wrap(vars(moreFamiliarPath), labeller = labeller(moreFamiliarPath = wrap_labels)) +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13))
#jpeg("C:/Users/amuller/Desktop/Alana/UA/HSCL/Conferences/iNAV 2024/pics/navTrialsMedian.jpeg", width = 10, height = 5.75, units = 'in', res = 500)
navTrialsMedian
#dev.off()

# graph for variance for iNAV
wrap_labels <- c("Inner More Familiar", "Outer More Familiar")
names(wrap_labels) <- c("inner", "outer")
tick_labels <- c("Novel Grids", "Outer Grids", "Inner Grids")

navTrialsIQR <- ggplot(NO_nav_summary, aes(x = grid_type, y = IQR_grid_number, fill = condition)) + 
  geom_boxplot(outliers = FALSE) + geom_jitter(position = position_jitterdodge()) +
  labs(x = "Grid Type", y = "Interquartile Range", fill = "Condition") +
  scale_x_discrete(labels = tick_labels) + 
  scale_fill_discrete(name = "Condition", labels = c("Cold Pressor", "Control", "Fire Environment"), type = c("deep sky blue", "lime green", "salmon")) +
  facet_wrap(vars(moreFamiliarPath), labeller = labeller(moreFamiliarPath = wrap_labels)) +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13))
#jpeg("C:/Users/amuller/Desktop/Alana/UA/HSCL/Conferences/iNAV 2024/pics/navTrialsIQR.jpeg", width = 10, height = 5.75, units = 'in', res = 500)
navTrialsIQR
#dev.off()


# check normality
NO_nav_summary %>%
  group_by(condition, grid_type, moreFamiliarPath) %>%
  shapiro_test(median_grid_number) # 10 are sig not normal but 8 are ok

# ARTool analysis - need some help here
n <- art(median_grid_number ~ condition*grid_type*moreFamiliarPath + Error(condition), data = NO_nav_summary)
summary(n) # still may not be appropriate
anova(n) # lots of sig

# try regular anova - same as ARTool
res.aov <- aov(median_grid_number ~ condition*grid_type*moreFamiliarPath, data = NO_nav_summary)
summary(res.aov)

# one data point per person per grid type
nav_summary2 <- longNav %>%
  group_by(subjectID, grid_type, condition) %>%
  summarize(
    count = n(),
    mean_grid_number = mean(grid_number, na.rm = TRUE), 
    sd_grid_number = sd(grid_number, na.rm = TRUE),
    median_grid_number = median(grid_number, na.rm = TRUE),
    IQR_grid_number = IQR(grid_number, na.rm = TRUE)
  )

ggplot(nav_summary2, aes(x = grid_type, y = median_grid_number, fill = condition)) + 
  geom_boxplot(outliers = FALSE) +
  geom_jitter(position = position_jitterdodge())

# graph that connects each person's data points
nav_summary2 <- longNav %>%
  group_by(subjectID, grid_type, condition) %>%
  summarize(
    count = n(),
    mean_grid_number = mean(grid_number, na.rm = TRUE), 
    sd_grid_number = sd(grid_number, na.rm = TRUE),
    median_grid_number = median(grid_number, na.rm = TRUE),
    IQR_grid_number = IQR(grid_number, na.rm = TRUE)
  )

ggplot(nav_summary2, aes(x = grid_type, y = median_grid_number, fill = condition)) + 
  facet_wrap(vars(condition)) +
  geom_boxplot(outliers = FALSE, coef = 0) +
  geom_point() +
  geom_line(aes(group = subjectID))

##### do that again but with more and less familiar grid numbers

# gather more fam and less fam grid nums and novel grids
# name the columns you want to gather, the other columns will remain there
gathered_columns <- c("novel_grids", "moreFamGridNum", "lessFamGridNum")

# gather the data for grid numbers
longNav2 <- gather(no_lost_navTrials, key = grid_type, value = grid_number, gathered_columns, factor_key = TRUE)

# check normality
longNav2 %>%
  group_by(condition, grid_type) %>%
  shapiro_test(grid_number) # everything is not normal - try log transformation

longNav2$log_grid_number <- log(longNav2$grid_number + 2) # +2 because some values have -1 and 0

# recheck normality
longNav2 %>%
  group_by(condition, grid_type) %>%
  shapiro_test(log_grid_number) # still super not normal so lets do the ARTool package

# ARTool analysis
m <- art(grid_number ~ condition*grid_type + Error(city), data = longNav2)
summary(m) # says ART may not be appropriate
anova(m) # it's all sig

nav_summary2 <- longNav2 %>%
  group_by(subjectID, grid_type, condition) %>%
  summarize(
    count = n(),
    mean_grid_number = mean(grid_number, na.rm = TRUE), 
    sd_grid_number = sd(grid_number, na.rm = TRUE),
    median_grid_number = median(grid_number, na.rm = TRUE),
    IQR_grid_number = IQR(grid_number, na.rm = TRUE)
  )

# check for outliers
medianGrid <- median(nav_summary2$median_grid_number)
iqrGrid <- IQR(nav_summary2$IQR_grid_number)

upperGridRange <- medianGrid + (2.5*iqrGrid)

NO_nav_summary2 <- subset(nav_summary2, median_grid_number < upperGridRange)

# graph for iNAV
tick_labels <- c("Novel Grids", "More Familiar Path Grids", "Less Familiar Path Grids")

navPlot <- ggplot(NO_nav_summary2, aes(x = grid_type, y = median_grid_number, fill = condition)) + 
  geom_boxplot(outliers = FALSE) + geom_jitter(position = position_jitterdodge()) +
  labs(x = "Grid Type", y = "Median Grid Number", fill = "Condition") +
  scale_x_discrete(labels = tick_labels) + 
  scale_fill_discrete(name = "Condition", labels = c("Cold Pressor", "Control", "Fire Environment"), type = c("deep sky blue", "lime green", "salmon")) +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13))
#jpeg("C:/Users/amuller/Desktop/Alana/UA/HSCL/Conferences/iNAV 2024/pics/navMoreLessFam.jpeg", width = 9.5, height = 5.75, units = 'in', res = 500)
navPlot
#dev.off()
  

#################### How often did participants have the optimal path?
hist(no_lost_navTrials$excess_block_num)

optimal_trials <- which(no_lost_navTrials$excess_block_num <= 1)
length(optimal_trials) # 728 optimal trials

optimal_trials <- subset(no_lost_navTrials, excess_block_num <= 1)

# gather the data for grid numbers
optimal_nav <- gather(optimal_trials, key = grid_type, value = grid_number, gathered_columns, factor_key = TRUE)

# one data point per person per grid type
nav_summary <- optimal_nav %>%
  group_by(subjectID, condition) %>%
  summarize(
    count = n(),
    mean_grid_number = mean(grid_number, na.rm = TRUE), 
    sd_grid_number = sd(grid_number, na.rm = TRUE),
    median_grid_number = median(grid_number, na.rm = TRUE),
    IQR_grid_number = IQR(grid_number, na.rm = TRUE)
  )

# graph for Kailee

ggplot(nav_summary, aes(x = condition, y = median_grid_number)) + 
  geom_boxplot(outliers = FALSE) +
  labs(x = "Grid Type", y = "Median Grid Number", fill = "Condition")

# code a column for number of optimal paths for each Ss

optimal_lost_subj <- navTrialsData %>%
  group_by(subjectID) %>%
  summarize(
    optimal_nav_count = sum(excess_block_num <= 2), # allow for a little variation when finding stores
    lost_trials_count = sum(grid_count > mean_gridCount+(2.5*sd_gridCount))
  )

sum(optimal_lost_subj$optimal_nav_count)
sum(optimal_lost_subj$lost_trials_count)

hist(optimal_lost_subj$optimal_nav_count)
shapiro.test(optimal_lost_subj$optimal_nav_count) # it's normal

optimal_lost_subj_SOT <- merge(optimal_lost_subj, sotData, by = "subjectID") 

optimal_by_condition <- navTrialsData %>%
  group_by(subjectID, condition) %>%
  summarize(
    optimal_nav_count = sum(excess_block_num <= 2), # allow for a little variation when finding stores
    lost_trials_count = sum(grid_count > mean_gridCount+(2.5*sd_gridCount))
  )

ggplot(optimal_by_condition, aes(x = condition, y = optimal_nav_count)) + 
  geom_boxplot(outliers = FALSE, coef = 0) +
  geom_point() +
  geom_line(aes(group = subjectID))




# check angular error for outliers - 1 at 116 SOT error
mean_SOT <- mean(optimal_lost_subj_SOT$SOT_average_angular_error)
sd_SOT <- sd(optimal_lost_subj_SOT$SOT_average_angular_error)

optimal_lost_subj_SOT <- subset(optimal_lost_subj_SOT, SOT_average_angular_error < mean_SOT + (2.5*sd_SOT))

plot(optimal_lost_subj_SOT$optimal_nav_count, optimal_lost_subj_SOT$SOT_average_angular_error)
cor.test(optimal_lost_subj_SOT$optimal_nav_count, optimal_lost_subj_SOT$SOT_average_angular_error) # not sig, p = .065

plot(optimal_lost_subj_SOT$lost_trials_count, optimal_lost_subj_SOT$SOT_average_angular_error)
cor.test(optimal_lost_subj_SOT$lost_trials_count, optimal_lost_subj_SOT$SOT_average_angular_error) # dumb but sig

# maybe graph for INAV
sp <- ggscatter(optimal_lost_subj_SOT, x = "lost_trials_count", y = "SOT_average_angular_error", 
                add = "reg.line", 
                add.params = list(color = "blue", fill = "lightgray"), 
                conf.int = TRUE, 
                xlab = "Number of Lost Trials",
                ylab = "SOT Avg Angular Error") + 
  stat_cor(method = "pearson", label.x = 7, label.y = 15)

#jpeg("C:/Users/amuller/Desktop/Alana/UA/HSCL/Conferences/iNAV 2024/pics/sotLostCorr.jpeg", width = 4, height = 5, units = 'in', res = 500)
sp
#dev.off()

#################### How often did participants get lost?

# name the columns you want to gather, the other columns will remain there
gathered_columns <- c("novel_grids", "total_outer_use", "total_inner_use")

# gather the data for grid numbers
long_lost <- gather(lost_Ss, key = grid_type, value = grid_number, gathered_columns, factor_key = TRUE)

# one data point per person per grid type
nav_lost <- long_lost %>%
  group_by(subjectID, grid_type) %>%
  summarize(
    count = n(),
    mean_grid_number = mean(grid_number, na.rm = TRUE), 
    sd_grid_number = sd(grid_number, na.rm = TRUE),
    median_grid_number = median(grid_number, na.rm = TRUE),
    IQR_grid_number = IQR(grid_number, na.rm = TRUE)
  )

ggplot(nav_lost, aes(x = grid_type, y = median_grid_number)) + 
  geom_boxplot(outliers = FALSE) +
  geom_jitter(width = .1)

# Do people use the same paths forward, backward, and diagonal?

# one data point per person per grid type
trial_type_summary <- longNav %>%
  group_by(subjectID, trial_type, grid_type) %>%
  summarize(
    count = n(),
    mean_grid_number = mean(grid_number, na.rm = TRUE), 
    sd_grid_number = sd(grid_number, na.rm = TRUE),
    median_grid_number = median(grid_number, na.rm = TRUE),
    IQR_grid_number = IQR(grid_number, na.rm = TRUE)
  )

# check for outliers
medianGrid <- median(trial_type_summary$median_grid_number)
iqrGrid <- IQR(trial_type_summary$IQR_grid_number)

upperGridRange <- medianGrid + (2.5*iqrGrid)

trial_type_summary <- subset(trial_type_summary, median_grid_number < upperGridRange)

# graph for iNAV
trialType <- ggplot(trial_type_summary, aes(x = trial_type, y = median_grid_number, fill = grid_type)) + 
  geom_boxplot(outliers = FALSE) +
  geom_jitter(position = position_jitterdodge()) +
  labs(x = "Trial Type", y = "Median Grid Number", fill = "Grid Type") +
  scale_fill_discrete(name = "Condition", labels = c("Novel Grids", "More Familiar Grids", "Less Familiar Grids")) +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13)) +
  scale_x_discrete(labels = c("Backward", "Diagonal", "Forward"))
#jpeg("C:/Users/amuller/Desktop/Alana/UA/HSCL/Conferences/iNAV 2024/pics/trialType.jpeg", width = 8, height = 5.75, units = 'in', res = 500)
trialType
#dev.off()

######### excess path analysis

# Excess path analyses

# Did participants have more excess path for condition and path familiarity?
excessPathSummary <- longNav2 %>%
  group_by(subjectID, block, condition) %>%
  summarize(
    count = n(),
    mean_excess_path = mean(Navigate_excessPath),
    sd_excess_path = sd(Navigate_excessPath)
  )

excessPathPlot <- ggplot(excessPathSummary, aes(x = condition, y = mean_excess_path, fill = block)) + 
  geom_boxplot(outliers = FALSE) +
  geom_jitter(position = position_jitterdodge()) +
  labs(x = "Condition", y = "Mean Excess Path", fill = "Block") +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13), 
        strip.text = element_text(size = 13)) +
  scale_x_discrete(labels = c("Cold Pressor", "Control", "Fire Environment"))
jpeg("C:/Users/amuller/Desktop/Alana/UA/HSCL/Conferences/iNAV 2024/pics/excessPathPlot.jpeg", width = 8, height = 5.75, units = 'in', res = 500)
excessPathPlot
dev.off()

avgExcessPathSubj <- longNav2 %>%
  group_by(subjectID) %>%
  summarise(
    mean_excessPath = mean(Navigate_excessPath),
    sd_excessPath = sd(Navigate_excessPath)
  )

# Write bigMergeData to a new csv file
#write.csv(avgExcessPathSubj, "E:/Nav Stress Data/surveys/avgExcessPathSubj.csv", row.names = FALSE)















