# Clear the environment to start fresh
rm(list = ls())

library(readxl)
library(tidyverse)


# Set the directory where the Excel files are stored
setwd("E:/Nav Stress Pilot Data")

import_data <- read_excel("pilot_navTestTrials.xlsx", sheet = "Sheet 1")

# Copy of data to play with
data <- import_data

# Fix structure of the data: columns 1:11 should be factors
data <- data %>%
  mutate_at(vars(1:11), as.factor)

### Delete the trials that have a duration less than 1 because those are mistakes
# Count trials to be deleted
count <- sum(data$Navigate_duration > 1)

# Delete those rows
cd <- subset(data, Navigate_duration > 1) # cd stands for "clean data"

### Make some histograms - see the spread of the data
hist(data$Navigate_actualPath)
hist(data$Navigate_optimalPath)



### Make some graphs

# Navigation duration by block
nav_dur <- cd %>%
  group_by(subjectID, block, path_type) %>%
  summarise(
    mean_nav_dur = mean(Navigate_duration, na.rm = TRUE)
  )

# possible violin plot
testpic <- ggplot(nav_dur, aes(x = block, y = mean_nav_dur)) + geom_violin(draw_quantiles = c(0.25, 0.50, 0.75)) + 
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


