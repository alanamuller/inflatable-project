library(readxl)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(ez)
library(ggpubr)
library(rstatix)


# Set the directory where the Excel files are stored
setwd("E:/Nav Stress Pilot Data")

import_data <- read_excel("pilot_navTestTrials.xlsx", sheet = "Sheet 1")

# Copy of data to play with
df <- import_data

# Fix structure of the data: columns 1:11 should be factors
df <- df %>%
  mutate_at(vars(1:12), as.factor)

### Delete the trials that have a duration less than 1 because those are mistakes
# Count trials to be deleted
count <- sum(df$Navigate_duration < 1)

# Delete those rows
cd <- subset(df, Navigate_duration > 1) # cd stands for "clean data"

### Make some histograms - see the spread of the data
hist(df$Navigate_actualPath)
hist(df$Navigate_optimalPath)
hist(df$Navigate_excessPath)
hist(df$Navigate_duration)
hist(df$overlap_outer)
hist(df$overlap_inner)
hist(df$total_grids_trial)

# Make column for overlap percentages
cd$overlap_outer_percent <- (cd$overlap_outer/cd$total_grids)*100
cd$overlap_inner_percent <- (cd$overlap_inner/cd$total_grids)*100
cd$nonoverlap_percent <- (cd$nonoverlap/cd$total_grids)*100

# Make column for overlap outer-inner
cd$outerMinusInner <- (cd$overlap_outer_percent - cd$overlap_inner_percent)

### Make some graphs

# Navigation duration by block
nav_dur <- cd %>%
  group_by(subjectID, block, path_type) %>%
  summarise(
    mean_nav_dur = mean(Navigate_duration, na.rm = TRUE),
    sd = sd(Navigate_duration, na.rm = TRUE)
  )

### Plots by subject to get a look at how each did

# excess path
ggplot(cd, aes(x = subjectID, y = Navigate_excessPath)) +
  geom_boxplot() + 
  geom_jitter(color="gray", size=1, width = 0.15) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.position = "none") +
  scale_fill_brewer(palette = "Paired")

# nav duration
ggplot(cd, aes(x = subjectID, y = Navigate_duration)) +
  geom_boxplot() + 
  geom_jitter(color="gray", size=1, width = 0.15) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.position = "none") +
  scale_fill_brewer(palette = "Paired")

# overlap outer path
ggplot(cd, aes(x = subjectID, y = overlap_outer)) +
  geom_boxplot() + 
  geom_jitter(color="gray", size=1, width = 0.15) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.position = "none") +
  scale_fill_brewer(palette = "Paired")

# overlap inner path
ggplot(cd, aes(x = subjectID, y = overlap_inner)) +
  geom_boxplot() + 
  geom_jitter(color="gray", size=1, width = 0.15) +
  theme_classic() +
  xlab("Subject Number") +
  ylab("Mean Nav Duration (s)") + 
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.position = "none") +
  scale_fill_brewer(palette = "Paired")

##### Overlap dv analyses

# outer and inner path on same plot
overlap_df <- cd[c("subjectID", "overlap_outer_percent", "overlap_inner_percent", "nonoverlap_percent", "path_type", "first_route_learned", "outer_reps", "Navigate_excessPath")]
overlap_melt <- melt(overlap_df, id.vars =  c("subjectID", "path_type", "first_route_learned", "outer_reps", "Navigate_excessPath"), 
                     variable.name = "inner_outer_non", value.name = "overlap_percent")

ggplot(overlap_melt, aes(x = subjectID, y = overlap_percent, fill = inner_outer_non)) +
  geom_boxplot() +  theme_classic()

# split by which path taught first
plot_left <- ggplot(subset(overlap_melt, first_route_learned == "Outer"), aes(x = subjectID, y = overlap_percent, fill = inner_outer_non)) +
                      geom_boxplot() + labs(title = "Outer Route Learned First") + theme_classic() + guides(fill = FALSE)
plot_right <- ggplot(subset(overlap_melt, first_route_learned == "Inner"), aes(x = subjectID, y = overlap_percent, fill = inner_outer_non)) +
  geom_boxplot() + labs(title = "Inner Route Learned First") + theme_classic() + guides(fill = FALSE)
grid.arrange(plot_left, plot_right, ncol=2)

# split by which path taught first and how many reps it got
plot11 <- ggplot(subset(overlap_melt, first_route_learned == "Outer" & outer_reps == "2"), 
                 aes(x = subjectID, y = overlap_percent, fill = inner_outer_non)) + geom_boxplot() + labs(title = "Outer 1st, outer rep 2, inner rep 4")
plot12 <- ggplot(subset(overlap_melt, first_route_learned == "Inner" & outer_reps == "2"), 
                 aes(x = subjectID, y = overlap_percent, fill = inner_outer_non)) + geom_boxplot() + labs(title = "Inner 1st, outer rep 2, inner rep 4")
plot21 <- ggplot(subset(overlap_melt, first_route_learned == "Outer" & outer_reps == "4"), 
                 aes(x = subjectID, y = overlap_percent, fill = inner_outer_non)) + geom_boxplot() + labs(title = "Outer 1st, outer rep 4, inner rep 2")
plot22 <- ggplot(subset(overlap_melt, first_route_learned == "Inner" & outer_reps == "4"), 
                 aes(x = subjectID, y = overlap_percent, fill = inner_outer_non)) + geom_boxplot() + labs(title = "Inner 1st, outer rep 4, inner rep 2")
plot31 <- ggplot(subset(overlap_melt, first_route_learned == "Outer" & outer_reps == "3"), 
                 aes(x = subjectID, y = overlap_percent, fill = inner_outer_non)) + geom_boxplot() + labs(title = "Outer 1st, outer rep 3, inner rep 6")
plot32 <- ggplot(subset(overlap_melt, first_route_learned == "Inner" & outer_reps == "3"), 
                 aes(x = subjectID, y = overlap_percent, fill = inner_outer_non)) + geom_boxplot() + labs(title = "Inner 1st, outer rep 3, inner rep 6")
plot41 <- ggplot(subset(overlap_melt, first_route_learned == "Outer" & outer_reps == "6"), 
                 aes(x = subjectID, y = overlap_percent, fill = inner_outer_non)) + geom_boxplot() + labs(title = "Outer 1st, outer rep 6, inner rep 3")
plot42 <- ggplot(subset(overlap_melt, first_route_learned == "Inner" & outer_reps == "6"), 
                 aes(x = subjectID, y = overlap_percent, fill = inner_outer_non)) + geom_boxplot() + labs(title = "Inner 1st, outer rep 6, inner rep 3")
grid.arrange(plot11, plot12, plot21, plot22, plot31, plot32, plot41, plot42, ncol=2, nrow=4)

### Plots by path type

# plain path type all together
ggplot(overlap_melt, aes(x = path_type, y = overlap_percent, fill = inner_outer_non)) +
  geom_boxplot() + 
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) +
  scale_fill_brewer(palette = "Paired")

# split by which path was taught first
plot_left <- ggplot(subset(overlap_melt, first_route_learned == "Outer"), aes(x = path_type, y = overlap_percent, fill = inner_outer_non)) +
  geom_boxplot() + labs(title = "Outer Route Learned First") + theme_classic() + guides(fill = FALSE)
plot_right <- ggplot(subset(overlap_melt, first_route_learned == "Inner"), aes(x = path_type, y = overlap_percent, fill = inner_outer_non)) +
  geom_boxplot() + labs(title = "Inner Route Learned First") + theme_classic()
grid.arrange(plot_left, plot_right, ncol=2)

##### Navigate excess path dv analyses

# split by which path taught first and how many reps it got
plot11 <- ggplot(subset(overlap_melt, first_route_learned == "Outer" & outer_reps == "2"), 
                 aes(x = subjectID, y = Navigate_excessPath)) + geom_boxplot() + labs(title = "Outer 1st, outer rep 2, inner rep 4")
plot12 <- ggplot(subset(overlap_melt, first_route_learned == "Inner" & outer_reps == "2"), 
                 aes(x = subjectID, y = Navigate_excessPath)) + geom_boxplot() + labs(title = "Inner 1st, outer rep 2, inner rep 4")
plot21 <- ggplot(subset(overlap_melt, first_route_learned == "Outer" & outer_reps == "4"), 
                 aes(x = subjectID, y = Navigate_excessPath)) + geom_boxplot() + labs(title = "Outer 1st, outer rep 4, inner rep 2")
plot22 <- ggplot(subset(overlap_melt, first_route_learned == "Inner" & outer_reps == "4"), 
                 aes(x = subjectID, y = Navigate_excessPath)) + geom_boxplot() + labs(title = "Inner 1st, outer rep 4, inner rep 2")
plot31 <- ggplot(subset(overlap_melt, first_route_learned == "Outer" & outer_reps == "3"), 
                 aes(x = subjectID, y = Navigate_excessPath)) + geom_boxplot() + labs(title = "Outer 1st, outer rep 3, inner rep 6")
plot32 <- ggplot(subset(overlap_melt, first_route_learned == "Inner" & outer_reps == "3"), 
                 aes(x = subjectID, y = Navigate_excessPath)) + geom_boxplot() + labs(title = "Inner 1st, outer rep 3, inner rep 6")
plot41 <- ggplot(subset(overlap_melt, first_route_learned == "Outer" & outer_reps == "6"), 
                 aes(x = subjectID, y = Navigate_excessPath)) + geom_boxplot() + labs(title = "Outer 1st, outer rep 6, inner rep 3")
plot42 <- ggplot(subset(overlap_melt, first_route_learned == "Inner" & outer_reps == "6"), 
                 aes(x = subjectID, y = Navigate_excessPath)) + geom_boxplot() + labs(title = "Inner 1st, outer rep 6, inner rep 3")
grid.arrange(plot11, plot12, plot21, plot22, plot31, plot32, plot41, plot42, ncol=2, nrow=4)

# Outer and inner excess path

# Stats for a t-test
t.test_df <- cd[c("subjectID", "first_route_learned", "Navigate_excessPath")]
t.test_df <- t.test_df %>%
  group_by(subjectID, first_route_learned) %>%
  summarise(
    mean_nav_excessPath = mean(Navigate_excessPath)
  )
wide_df <- pivot_wider(t.test_df, id_cols = subjectID, names_from = first_route_learned, values_from = mean_nav_excessPath)
wide_df <- wide_df[, -1]

t.test(wide_df$Outer, wide_df$Inner) # not sig

ggplot(t.test_df, aes(x = first_route_learned, y = mean_nav_excessPath)) +
  geom_boxplot() + 
  geom_jitter(color="gray", size=1, width = 0.15) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.position = "none") +
  scale_fill_brewer(palette = "Paired")

##### Stats

### 2 way anova for nav excess path

# create dataframe for stats
excessPath_df <- cd[c("subjectID", "first_route_learned", "rep_condition","Navigate_excessPath")]
ep_df <- excessPath_df %>%
  group_by(subjectID, first_route_learned, rep_condition) %>%
  summarise(
    mean = mean(Navigate_excessPath, na.rm = TRUE)
  )

aov <- aov(mean ~ first_route_learned * rep_condition, data = ep_df)
summary(aov)

ggboxplot(ep_df, x = "first_route_learned", y = "mean", color = "rep_condition")

# anova for inner, outer, and nonoverlap
overlap_df <- cd[c("subjectID", "overlap_outer_percent", "overlap_inner_percent", "nonoverlap_percent")]
overlap_melt <- melt(overlap_df, id.vars = "subjectID", variable.name = "route_type", value.name = "overlap_percent")

overlap_aov <- overlap_melt %>%
  group_by(subjectID, route_type) %>%
  summarise(
    mean_overlap_percent = mean(overlap_percent)
  )

ggboxplot(overlap_aov, x = "route_type", y = "mean_overlap_percent")

# Perform paired one-way ANOVA
result <- aov(mean_overlap_percent ~ route_type + Error(subjectID/route_type), data = overlap_aov)
summary(result)

pwc <- overlap_melt %>%
  pairwise_t_test(
    overlap_percent ~ route_type, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc
