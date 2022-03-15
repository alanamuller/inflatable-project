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

rm(list = ls())

setwd("E:/Nav_1stYr_project_data")

# Read in data
myData <- read_csv("E:/Nav_1stYr_project_data/nav_room_error_data_cns.csv")
smallData <- read_csv("E:/Nav_1stYr_project_data/nav_room_error_small_data.csv")
str(myData)

smallData$placement_error_cm_log <- as.numeric(smallData$placement_error_cm_log)

# fix the data frame to have the correct properties
myData <- as.data.frame(myData)
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
myData$Fixated_or_not <- as.factor(myData$Fixated_or_not)

#myData$x_study <- as.numeric(myData$x_study)
#myData$y_study <- as.numeric(myData$y_study)
#myData$x_replace <- as.numeric(myData$x_replace)
#myData$y_replace <- as.numeric(myData$y_replace)
#myData$x_error <- as.numeric(myData$x_error)
#myData$y_error <- as.numeric(myData$y_error)
#myData$placement_error <- as.numeric(myData$placement_error)
#myData$x_error_cm <- as.numeric(myData$x_error_cm)
#myData$y_error_cm <- as.numeric(myData$y_error_cm)
#myData$placement_error_cm <- as.numeric(myData$placement_error_cm)


### Group of all data and subgroups of data

myData <- unite(myData, move_view, c(walk_noWalk, same_diff), remove = FALSE)
myData$move_view <- as.factor(myData$move_view)

################ Log transform placement error_cm ##################### still not normal

myData$placement_error_cm_log <- log(myData$placement_error_cm)
hist(myData$placement_error_cm_log)

ggqqplot(myData$placement_error_cm_log)
shapiro.test(myData$placement_error_cm_log)
mean_log <- mean(myData$placement_error_cm_log, na.rm = TRUE) # 3.0197
sd_log <- sd(myData$placement_error_cm_log, na.rm = TRUE)

data_log_NO <- myData %>%
  filter(placement_error_cm_log < mean_log+(sd_log*3) & placement_error_cm_log > mean_log-(sd_log*3))
hist(data_log_NO$placement_error_cm_log)
ggqqplot(data_log_NO$placement_error_cm_log)
shapiro.test(data_log_NO$placement_error_cm_log)


#################### 2-way ANOVA with walk and view ##################
bxp <- ggboxplot(
  data_log_NO, x = "walk_noWalk", y = "placement_error_cm_log", 
  color = "same_diff"
)

bxp

smallData <- na.omit(smallData)

small_data <- smallData %>%
  select(subject, walk_noWalk, same_diff, placement_error_cm_log)

aov_data <- small_data %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    count = n(),
    mean = mean(placement_error_cm_log, na.rm = TRUE),
    sd = sd(placement_error_cm_log, na.rm = TRUE),
  )
aov_data <- as.data.frame(aov_data)

aov_result <- anova_test(
  data = aov_data, dv = mean, wid = subject,
  within = c(walk_noWalk, same_diff)
)
# I'm getting the error Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) : 
#0 (non-NA) cases and I don't FUCKING know why

##################

#################### Take out outliers more than 3 st dev away

hist1 <- hist(myData$placement_error_cm, main = "Histogram of Placement Error (cm)", xlab = "Placement Error (cm)")
mean_data <- mean(myData$placement_error_cm, na.rm = TRUE)
sd_data <- sd(myData$placement_error_cm, na.rm = TRUE)

data_NO <- myData %>%
  filter(placement_error_cm < mean_data + (3*sd_data) & placement_error_cm > mean_data - (3*sd_data))

hist2 <- hist(data_NO$placement_error_cm, main = "Histogram of Placement Error (cm)", xlab = "Placement Error (cm)")

#read_pptx() %>%
#  add_slide(layout = "Title and Content", master = "Office Theme") %>%
#  ph_with(x = hist1, type = "body", height = 8.5, width = 10.5) %>%
#  print("E:/Nav_1stYr_project_data/pics.pptx")

####################

# Friedman test for placement error cm

friedmanTableCM <- data_NO %>%
  group_by(subject, move_view) %>%
  summarize(
    count = n(),
    mean = mean(placement_error_cm, na.rm = TRUE),
    sd = sd(placement_error_cm, na.rm = TRUE),
    median = median(placement_error_cm, na.rm = TRUE)
  )

friedmanTableCM <- as.data.frame(friedmanTableCM)

friedmanTableCM %>%
  group_by(move_view) %>%
  get_summary_stats(median, type = "common")

friedmanTableSubject <- friedmanTableCM %>%
  group_by(move_view) %>%
  get_summary_stats(median, type = "common")

friedman.test(median ~ move_view | subject, data = friedmanTableCM) # p = .6869

res.fried <- friedmanTableCM %>% friedman_test(median ~ move_view |subject)
res.fried 

friedmanTableCM %>%
  group_by(move_view) %>%
  identify_outliers(median) # no outliers

bxp <- ggboxplot(
  friedmanTableCM, x = "move_view", y = "median", add = "jitter", title = "Placement Error (cm)", 
  xlab = "Movement and Replacement View Conditions", ylab = "Placement Error (cm)") +
  scale_y_continuous(breaks = seq(0,60,10)) + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels=c("no walk_diff" = "No walk \n Diff", "no walk_same" = "No walk \n Same",
                              "walk_diff" = "Walk \n Diff", "walk_same" = "Walk \n Same"))
bxp

# Create table of move, view, placement error
friedman_move_view_table <- data_NO %>%
  group_by(subject, move_view) %>%
  summarize(
    count = n(),
    placement_error_cm = median(placement_error_cm, na.rm = TRUE),
  )
friedman_move_view_table <- as.data.frame(friedman_move_view_table)
friedman_move_view_table$subject <- factor(friedman_move_view_table$subject)
friedman_move_view_table$move_view <- factor(friedman_move_view_table$move_view)

# Graph: Placement error by movement and orientation with full data
ggboxplot(data_NO, x = "walk_noWalk", y = "placement_error_cm", color = "same_diff", add = "jitter") +
  xlab("Movement Condition") +
  ylab("Placement Error (cm)") +
  scale_x_discrete(labels = c("No Walk", "Walk")) +
  labs(color = "Retrieval Viewpoint") +
  scale_y_continuous(breaks = seq(0,100,10))

ggline(friedmanTableCM, x = "move_view", y = "median", color = "subject", title = "Placement error in cm (accounting for both x and y-axes)") +
  xlab("Movement and Replacement View Conditions") +
  ylab("Placement Error (cm)") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0,100,10))

# Friedman test for y error in cm

friedman_y_table_cm <- data_NO %>%
  group_by(subject, move_view) %>%
  summarize(
    count = n(),
    mean = mean(y_error_cm, na.rm = TRUE),
    sd = sd(y_error_cm, na.rm = TRUE),
    median = median(y_error_cm, na.rm = TRUE)
  )

friedman_y_table_cm <- as.data.frame(friedman_y_table_cm)

friedman_y_table_cm %>%
  group_by(move_view) %>%
  get_summary_stats(median, type = "common")

friedman.test(median ~ move_view | subject, data = friedman_y_table_cm) # p = .439

res.fried <- friedman_y_table_cm %>% friedman_test(median ~ move_view |subject)
res.fried 

frdAllPairsNemenyiTest(median ~ move_view | subject, data = friedman_y_table_cm)

friedman_y_table_cm %>%
  group_by(subject, move_view) %>%
  identify_outliers(median) # no outliers

friedman_y_table_cm %>%
  group_by(move_view) %>%
  identify_outliers(median) # 2 in P4

bxp <- ggboxplot(
  friedman_y_table_cm, x = "move_view", y = "median", add = "jitter", title = "Y-axis Error (values of 0 mean no error)", 
  xlab = "Movement and Replacement View Conditions", ylab = "Placement Error (cm)") +
  scale_y_continuous(breaks = seq(-20,60,10))
bxp

#ggline(friedman_y_table_cm, x = "move_view", y = "median", color = "subject", title = "Y-axis Error (values of 0 mean no error)", xlab = "Movement and Replacement View Conditions", ylab = "Placement Error (cm)")

wilcox.test(data_NO$x_error_cm, data_NO$y_error_cm, paired = TRUE)
friedman_xvy <- data_NO %>%
  group_by(subject, x_error_cm, y_error_cm) %>%
  summarize(
    median = median(y_error_cm, na.rm = TRUE)
  )
ggline(datamelt_xvy, x = "type_error", y = "value", color = "subject", title = "Y-axis Error (values of 0 mean no error)", xlab = "Movement and Replacement View Conditions", ylab = "Placement Error (cm)")


# Friedman test for x error cm

friedman_x_table_cm <- data_NO %>%
  group_by(subject, move_view) %>%
  summarize(
    count = n(),
    mean = mean(x_error_cm, na.rm = TRUE),
    sd = sd(x_error_cm, na.rm = TRUE),
    median = median(x_error_cm, na.rm = TRUE)
  )

friedman_x_table_cm <- as.data.frame(friedman_x_table_cm)

friedman_x_table_cm %>%
  group_by(move_view) %>%
  get_summary_stats(median, type = "common")

friedman.test(median ~ move_view | subject, data = friedman_x_table_cm) # p = .2222

res.fried <- friedman_x_table_cm %>% friedman_test(median ~ move_view |subject)
res.fried

friedman_x_table_cm %>%
  group_by(subject, move_view) %>%
  identify_outliers(median) # no outliers

friedman_x_table_cm %>%
  group_by(move_view) %>%
  identify_outliers(median) # one, subject 9, walk_same

bxp <- ggboxplot(
  friedman_x_table_cm, x = "move_view", y = "median", add = "jitter", title = "X-axis Error (values of 0 mean no error)", 
  xlab = "Movement and Replacement View Conditions", ylab = "Placement Error (cm)") +
  scale_y_continuous(breaks = seq(-20,60,10))
bxp

ggline(friedman_x_table_cm, x = "move_view", y = "median", color = "subject", title = "X-axis Error (values of 0 mean no error)") +
  theme(legend.position = "none")

##############
# If you have time, do another friedman's test for x and y error

data_xvy <- data_NO[,c("subject", "trial", "wall_side", "object", "object_size", "object_material", "walk_noWalk", "same_diff", "move_view", "x_error_cm", "y_error_cm", "placement_error_cm")]
datamelt_xvy <- melt(data_xvy, id=c("subject", "trial", "wall_side", "object", "object_size", "object_material", "walk_noWalk", "same_diff", "move_view"), value.name = "Error(cm)", variable_name = "type_error")
datamelt_xvy3 <- datamelt_xvy %>%
  filter(type_error == "x_error_cm" | type_error == "y_error_cm")

xyerrorsubject <- datamelt_xvy3 %>%
  group_by(subject, type_error) %>%
  summarize(
    count = n(),
    median = median(value, na.rm = TRUE)
  )
xyerrorsubject <- as.data.frame(xyerrorsubject)

# Placement error (cm) values
ggboxplot(xyerrorsubject, x = "type_error", y = "median", add = "jitter",
          color = "type_error", ylab = "Placement Error (cm)", 
          xlab = "Type of Error") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), legend.position = "none") +
  scale_y_continuous(breaks = seq(-100,100,20))

kruskal.test(median ~ type_error, data = xyerrorsubject) # p = 0.2217 so as a group it's sig but by subject it's not

wilcox.test(data_xvy$x_error_cm, data_xvy$y_error_cm, paired = TRUE) # p = .003
median(data_xvy$x_error_cm)
mad(data_xvy$x_error_cm, center = median(data_xvy))
median(data_xvy$y_error_cm)
sd(data_xvy$y_error_cm)

# Normalized x and y error (cm) values
ggboxplot(datamelt_xvy2, x = "type_error", y = "value", add = "jitter",
          color = "type_error", ylab = "% Wall Space Moved From Original Position", 
          xlab = "Type of Error") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), legend.position = "none") +
  scale_y_continuous(breaks = seq(-100,100,10))

kruskal.test(value ~ type_error, data = datamelt_xvy2) # p = .0008351

##############

########### Basic descriptives

######## Placement error by subject

## Graph: Error by subject
ggplot(data_NO, aes(x = subject, y = placement_error_cm)) +
  geom_boxplot() + 
  geom_jitter(color="gray", size=1, width = 0.15) +
  theme_classic() +
  xlab("Subject Number") +
  ylab("Placement Error (cm)") + 
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.position = "none") +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(breaks = seq(-100,100,10))

## Normality: Placement error by subject very skewed to right so have to do nonparametric test
ggplot(data_NO, aes(x=placement_error_cm)) +
  geom_density(aes(group=subject, color=subject, fill=subject), alpha=0.3)

## Table: Placement error by subject
group_by(data_NO, subject) %>%
  summarize(
    count = n(),
    mean = mean(placement_error_cm, na.rm = TRUE),
    sd = sd(placement_error_cm, na.rm = TRUE),
    median = median(placement_error_cm, na.rm = TRUE),
    IQR = IQR(placement_error_cm, na.rm = TRUE)
  )

kruskal.test(placement_error_cm ~ subject, data = data_NO) # p < .0001
epsilonSquared(x = data_NO$placement_error_cm, g = data_NO$subject) # 0.138
DT = dunn_test(placement_error_cm ~ subject, data = data_NO, p.adjust.method = "bonferroni")
DT # so many

######## Placement error by each trial only

## Table: Error placement by trial only
trialError <- data_NO %>%
  group_by(trial) %>%
  summarize(
    count = n(),
    median = median(placement_error_cm, na.rm = TRUE),
    sd = sd(placement_error_cm, na.rm = TRUE)
  )
trialError

## Normality: Error by trial only
ggplot(data_NO, aes(x=placement_error_cm)) +
  geom_density(aes(group=trial, color=trial, fill=trial), alpha=0.3)

## Graph: Placement error by trail number only
ggboxplot(data_NO, x = "trial", y = "placement_error_cm", add = "jitter",
          color = "trial", ylab = "Placement Error (cm)", 
          xlab = "Trial Number") +
  stat_summary(fun=median, geom="point", color="red", size=2) +
  scale_y_continuous(breaks = seq(-100,100,10)) +
  theme(legend.position = "none")

kruskal.test(placement_error_cm ~ trial, data = data_NO) # p < .0001
epsilonSquared(x = data_NO$placement_error_cm, g = data_NO$trial) # 0.0389
DT = dunn_test(placement_error_cm ~ trial, data = data_NO, p.adjust.method = "bonferroni")
DT # trial 1 was diff than every other trial than 2, it was the worst trial for accuracy

########## Placement error for each subject and each trial

## Graph: Error for each subject and each trial
ggplot(data_NO, aes(x = trial, y = placement_error_cm, fill = trial)) +
  geom_boxplot() + 
  theme_classic() +
  stat_summary(fun=mean, geom="point", color="red", size=2) +
  facet_grid(rows = vars(subject)) +
  xlab("Trial Number") +
  ylab("Placement Error (cm)") + 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14), legend.position = "none") +
  scale_fill_brewer(palette = "Paired") +
  theme(strip.text = element_text(size = 12))

## Table: Error placement for each subject and each trial
trialError_subj <- data_NO %>%
  group_by(subject, trial) %>%
  summarize(
    count = n(),
    mean = mean(placement_error_cm, na.rm = TRUE),
    sd = sd(placement_error_cm, na.rm = TRUE)
  )
View(trialError_subj)

############# Placement error by walk_noWalk only

# Normality: Placement error by move only
ggplot(data_NO, aes(x=placement_error_cm)) +
  geom_density(aes(group=walk_noWalk, color=walk_noWalk, fill=walk_noWalk), alpha=0.3)

# Table: Placement error by move only
group_by(data_NO, walk_noWalk) %>%
  summarize(
    count = n(), 
    mean = mean(placement_error_cm, na.rm = TRUE),
    sd = sd(placement_error_cm, na.rm = TRUE),
    median = median(placement_error_cm, na.rm = TRUE),
    IQR = IQR(placement_error_cm, na.rm = TRUE)
  )

# Graph: Placement error by move only
ggboxplot(data_NO, x = "walk_noWalk", y = "placement_error_cm", add = "jitter", 
          color = "walk_noWalk", ylab = "Placement Error (cm)", 
          xlab = "Movement Condition") +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.position = "none") +
  scale_fill_brewer(palette = "Paired")

# Graph: Placement error by move only broken down by subjects
ggplot(data_NO, aes(x = walk_noWalk, y = placement_error_cm, fill = subject)) +
  geom_boxplot(aes(fill=subject), position=position_dodge(.9)) + 
  theme_classic() +
  stat_summary(fun=mean, geom="point", aes(group=subject), position=position_dodge(.9), 
               color="red", size=2) +
  guides(fill = guide_legend(title = "Subject")) +
  xlab("Movement Condition") +
  ylab("Placement Error (cm)") +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.text = element_text(size = 11), legend.title = element_text(size = 12)) +
  scale_fill_brewer(palette = "Paired")

kruskal.test(placement_error_cm ~ walk_noWalk, data = data_NO) # p = .6686

####### Placement error by view only

## Table: Placement error by view only
group_by(data_NO, same_diff) %>%
  summarize(
    count = n(), 
    mean = mean(placement_error_cm, na.rm = TRUE),
    sd = sd(placement_error_cm, na.rm = TRUE),
    median = median(placement_error_cm, na.rm = TRUE),
    IQR = IQR(placement_error_cm, na.rm = TRUE)
  )

# Graph: Placement error by view only
ggboxplot(data_NO, x = "same_diff", y = "placement_error_cm", add = "jitter", 
          color = "same_diff", ylab = "Placement Error (cm)", 
          xlab = "View Condition") +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.position = "none") +
  scale_fill_brewer(palette = "Paired")

# Graph: Placement error by view only broken down by subject
ggplot(data_NO, aes(x = same_diff, y = placement_error_cm, fill = subject)) +
  geom_boxplot(aes(fill=subject), position=position_dodge(.9)) + 
  theme_classic() +
  stat_summary(fun=mean, geom="point", aes(group=subject), position=position_dodge(.9), 
               color="red", size=2) +
  guides(fill = guide_legend(title = "Subject")) +
  xlab("View Condition") +
  ylab("Placement Error (cm)") +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.text = element_text(size = 11), legend.title = element_text(size = 12)) +
  scale_fill_brewer(palette = "Paired")

kruskal.test(placement_error_cm ~ same_diff, data = data_NO) # p = .26

####### Placement error by movement and orientation - 2-way non-parametric repeated Friedman's test




## Table: Placement error by movement and orientation for each subject
move_view_subj <- data_NO %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    count = n(),
    mean = mean(placement_error_cm, na.rm = TRUE),
    sd = sd(placement_error_cm, na.rm = TRUE)
  )
View(move_view_subj)

## Table: Placement error by movement and orientation
move_view_table <- data_NO %>%
  group_by(walk_noWalk, same_diff) %>%
  summarize(
    count = n(),
    mean = mean(placement_error_cm, na.rm = TRUE),
    sd = sd(placement_error_cm, na.rm = TRUE)
  )
View(move_view_table)


# Friedman Test placement error by move and by view (nonparametric 2-way repeated measures ANOVA)
friedman_move_view_table %>%
  group_by(move_view) %>%
  get_summary_stats(placement_error_cm, type = "common")

res.fried <- friedman_move_view_table %>% friedman_test(placement_error_cm ~ move_view |subject)
res.fried # p = .472

# Normality: Placement error by movement and orientation
ggplot(data_NO, aes(x=placement_error_cm)) +
  geom_density(aes(group=move_view, color=move_view, fill=move_view), alpha=0.3)

# Normality violated
ggqqplot(data_NO, "placement_error_cm", ggtheme = theme_bw()) +
  facet_grid(walk_noWalk~same_diff, labeller = "label_both")

# Table: Placement error by movement and orientation
table(data_NO$walk_noWalk, data_NO$same_diff)
group_by(data_NO, walk_noWalk, same_diff) %>%
  summarize(
    count = n(),
    mean = mean(placement_error_cm, na.rm = TRUE),
    sd = sd(placement_error_cm, na.rm = TRUE)
  )

# Forbidden ANOVA just so I can get effect size
res.aov2 <- aov(placement_error_cm ~ walk_noWalk * same_diff, data = data_NO)
summary(res.aov2) # p = .427
TukeyHSD(res.aov2)
eta_squared(res.aov2)
epsilonSquared(x = data_NO$placement_error_cm, g = data_NO$move_view)


# several outliers
data_NO %>%
  group_by(walk_noWalk, same_diff) %>%
  identify_outliers(placement_error_cm)

# all combinations of normality are violated
data_NO %>%
  group_by(walk_noWalk, same_diff) %>%
  shapiro_test(placement_error_cm)

################################

### X-error data

## Error by subject
ggplot(data_NO, aes(x = subject, y = x_error_cm, fill = subject)) +
  geom_boxplot() + 
  theme_classic() +
  stat_summary(fun=mean, geom="point", color="red", size=2)

## Graph of trial precision -- just making sure all the trials' errors look the same
ggplot(data_NO, aes(x = trial, y = x_error_cm, fill = trial)) +
  geom_boxplot() + 
  theme_classic() +
  stat_summary(fun=mean, geom="point", color="red", size=2)

# Table of trial precision
trialError <- data_NO %>%
  group_by(trial) %>%
  summarize(
    count = n(),
    mean = mean(x_error_cm, na.rm = TRUE),
    sd = sd(x_error_cm, na.rm = TRUE)
  )
trialError

## Graph of trial precision by subject
ggplot(data_NO, aes(x = trial, y = x_error_cm, fill = subject)) +
  geom_boxplot(aes(fill=subject), position=position_dodge(.9)) + 
  theme_classic() +
  stat_summary(fun=mean, geom="point", aes(group=subject), position=position_dodge(.9), 
               color="red", size=2)

trialError_subj <- data_NO %>%
  group_by(subject, trial) %>%
  summarize(
    count = n(),
    mean = mean(x_error_cm, na.rm = TRUE),
    sd = sd(x_error_cm, na.rm = TRUE)
  )
View(trialError_subj)

# Table of movement and orientation by subject
move_view_subj <- data_NO %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    count = n(),
    mean = mean(x_error_cm, na.rm = TRUE),
    sd = sd(x_error_cm, na.rm = TRUE)
  )
View(move_view_subj)

### Y-error data

## Error by subject
ggplot(data_NO, aes(x = subject, y = y_error_cm, fill = subject)) +
  geom_boxplot() + 
  theme_classic() +
  stat_summary(fun=mean, geom="point", color="red", size=2)

## Graph of trial precision -- just making sure all the trials' errors look the same
ggplot(data_NO, aes(x = trial, y = y_error_cm, fill = trial)) +
  geom_boxplot() + 
  theme_classic() +
  stat_summary(fun=mean, geom="point", color="red", size=2)

# Table of trial precision
trialError <- data_NO %>%
  group_by(trial) %>%
  summarize(
    count = n(),
    mean = mean(y_error_cm, na.rm = TRUE),
    sd = sd(y_error_cm, na.rm = TRUE)
  )
trialError


#########

# placement error by wall side
wallsidetable <- data_NO %>%
  group_by(subject, wall_side) %>%
  summarize(
    count = n(),
    placement_error_cm = median(placement_error_cm, na.rm = TRUE)
  )

ggboxplot(wallsidetable, x = "wall_side", y = "placement_error_cm", add = "jitter",
          color = "wall_side", ylab = "Placement Error (cm)", 
          xlab = "Wall Side") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), legend.position = "none") +
  scale_x_discrete(labels = c("Double Door", "Single Door")) +
  scale_y_continuous(breaks = seq(0,100,10))


kruskal.test(placement_error_cm ~ wall_side, data = data_NO) # p = .602

friedman_walls <- data_NO %>%
  group_by(subject, wall_side) %>%
  summarize(
    count = n(),
    median = median(placement_error_cm, na.rm = TRUE)
  )
friedman.test(median ~ wall_side | subject, data = friedman_walls) # p = .3532


# placement error for wall height

ggboxplot(data_NO, x = "height", y = "placement_error_cm", add = "jitter",
          color = "height", ylab = "Placement Error (cm)", 
          xlab = "Wall Y-Position") +
  stat_summary(fun=median, geom="point", shape=20, size=5, color="red", fill="red") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), legend.position = "none") + 
  scale_x_discrete(lim = c("G","F","E","D","C","B","A")) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0,100,10))

ggplot(data_NO, aes(x = height, y = placement_error_cm, fill = subject)) +
  geom_boxplot(aes(fill=subject), position=position_dodge(.9)) + 
  theme_classic() +
  facet_grid(rows = vars(subject)) +
  guides(fill = guide_legend(title = "Subject")) +
  xlab("Wall Height") +
  ylab("Placement Error (cm)") +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.text = element_text(size = 11), legend.title = element_text(size = 12)) +
  scale_fill_brewer(palette = "Paired")

kruskal.test(placement_error_cm ~ height, data = data_NO) # p = .00433

pairwise.wilcox.test(data_NO$placement_error_cm, data_NO$height,
                     p.adjust.method = "BH") # sig diff from A: C,F; sig diff from B: F

# placement error for wall length

ggboxplot(data_NO, x = "width", y = "placement_error_cm", add = "jitter",
          color = "width", ylab = "Placement Error (cm)", 
          xlab = "Wall X-Position") +
  stat_summary(fun=median, geom="point", shape=20, size=5, color="red", fill="red") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), legend.position = "none") +
  scale_y_continuous(breaks = seq(0,100,10))

ggplot(data_NO, aes(x = width, y = placement_error_cm, fill = subject)) +
  geom_boxplot(aes(fill=subject), position=position_dodge(.9)) + 
  theme_classic() +
  stat_summary(fun=mean, geom="point", aes(group=subject), position=position_dodge(.9), 
               color="red", size=2) +
  facet_grid(rows = vars(subject)) +
  guides(fill = guide_legend(title = "Subject")) +
  xlab("Wall Width") +
  ylab("Placement Error (cm)") +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.text = element_text(size = 11), legend.title = element_text(size = 12)) +
  scale_fill_brewer(palette = "Paired")

kruskal.test(placement_error ~ width, data = data_NO) # p < .0001

pairwise.wilcox.test(data_NO$placement_error_cm, data_NO$width,
                     p.adjust.method = "BH")

# placement error next to landmarks

landmark_data <- data_NO %>%
  group_by(subject, next_to_landmark) %>%
  summarize(
    data_median = median(placement_error_cm, na.rm = TRUE)
  )


landmark_data <- spread(landmark_data, next_to_landmark, data_median)
wilcox.test(landmark_data$n, landmark_data$y, paired = TRUE) # p = .0092

landmark_data2 <- data_NO %>%
  select(subject, next_to_landmark, placement_error_cm)
landmark_data2 <- spread(landmark_data2, next_to_landmark, placement_error_cm)
median(landmark_data$n)
mad(landmark_data$n)
median(landmark_data$y)
mad(landmark_data$y)

ggboxplot(data_NO, x = "next_to_landmark", y = "placement_error_cm", add = "jitter",
          color = "next_to_landmark", ylab = "Placement Error (cm)", 
          xlab = "Next to Landmark?") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), legend.position = "none") +
  scale_y_continuous(breaks = seq(0,100,10))

kruskal.test(placement_error ~ next_to_landmark, data = data_NO) # p < .00001

friedman_landmark <- data_NO %>%
  group_by(subject, next_to_landmark) %>%
  summarize(
    count = n(),
    median = median(placement_error_cm, na.rm = TRUE)
  )
friedman.test(median ~ next_to_landmark | subject, data = friedman_landmark) # p = .00159

ggboxplot(friedman_landmark, x = "next_to_landmark", y = "median", add = "jitter",
          color = "next_to_landmark", ylab = "Placement Error (cm)", 
          xlab = "Next to Landmark?") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), legend.position = "none") +
  scale_y_continuous(breaks = seq(0,100,10))

ggline(landmark_data, x = "next_to_landmark", y = "data_median", color = "subject", title = "Placement error in cm (accounting for both x and y-axes)") +
  xlab("Movement and Replacement View Conditions") +
  ylab("Placement Error (cm)") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0,100,10))

landmarktypetable <- data_NO %>%
  group_by(subject, which_landmark) %>%
  summarize(
    count = n(),
    placement_error_cm = median(placement_error_cm, na.rm = TRUE)
  )

ggboxplot(landmarktypetable, x = "which_landmark", y = "placement_error_cm", add = "jitter",
          color = "which_landmark", ylab = "Placement Error (cm)", 
          xlab = "Wall Landmarks") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), legend.position = "none") +
  scale_y_continuous(breaks = seq(0,100,10)) + theme(axis.text.x = element_text(angle = 45))

kruskal.test(placement_error_cm ~ which_landmark, data = landmarktypetable) # p < .035

pairwise.wilcox.test(data_NO$placement_error_cm, data_NO$which_landmark,
                     p.adjust.method = "BH") # fire alarm is sig diff from everything except thermostat and light switch



ggboxplot(data_NO, x = "object", y = "placement_error_cm", 
          color = "object", ylab = "Placement Error (cm)", 
          xlab = "object") +
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), legend.position = "none")

kruskal.test(placement_error ~ object, data = data_NO) # p = .0357

# object size
sizetable <- data_NO %>%
  group_by(subject, object_size) %>%
  summarize(
    count = n(),
    placement_error_cm = median(placement_error_cm, na.rm = TRUE)
  )
sizetable <- as.data.frame(sizetable)

ggboxplot(sizetable, x = "object_size", y = "placement_error_cm", add = "jitter",
          color = "object_size", ylab = "Placement Error (cm)", 
          xlab = "Object Size") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), legend.position = "none") +
  scale_y_continuous(breaks = seq(0,100,10))

kruskal.test(placement_error ~ object_size, data = data_NO) # p = .099

friedman_size <- data_NO %>%
  group_by(subject, object_size) %>%
  summarize(
    count = n(),
    median = median(placement_error_cm, na.rm = TRUE)
  )
friedman.test(median ~ object_size | subject, data = friedman_size) # p = .279

# object material
materialtable <- data_NO %>%
  group_by(subject, object_material) %>%
  summarize(
    count = n(),
    placement_error_cm = median(placement_error_cm, na.rm = TRUE)
  )
materialtable <- as.data.frame(materialtable)

ggboxplot(materialtable, x = "object_material", y = "placement_error_cm", add = "jitter",
          color = "object_material", ylab = "Placement Error (cm)", 
          xlab = "Object Material") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), legend.position = "none") +
  scale_y_continuous(breaks = seq(0,100,10))

kruskal.test(placement_error ~ object_material, data = data_NO) # p = .001885

# alive or not
alivetable <- data_NO %>%
  group_by(subject, object_aliveORnot) %>%
  summarize(
    count = n(),
    placement_error_cm = median(placement_error_cm, na.rm = TRUE)
  )
alivetable <- as.data.frame(alivetable)

ggboxplot(alivetable, x = "object_aliveORnot", y = "placement_error_cm", add = "jitter",
          color = "object_aliveORnot", ylab = "Placement Error (cm)", 
          xlab = "Object Status: Living or Non-living") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), legend.position = "none") +
  scale_y_continuous(breaks = seq(0,100,10))

friedman_alive <- data_NO %>%
  group_by(subject, object_aliveORnot) %>%
  summarize(
    count = n(),
    median = median(placement_error_cm, na.rm = TRUE)
  )
living_median <- friedman_alive %>%
  filter(object_aliveORnot == "living")
median(living_median$median) # 24.25

nonliving_median <- friedman_alive %>%
  filter(object_aliveORnot == "non-living")
median(nonliving_median$median) # 19.79

friedman.test(median ~ object_aliveORnot | subject, data = friedman_alive) # p = .001595

# Friedman test for placement error cm by object material type

friedman_material <- data_NO %>%
  group_by(subject, object_material) %>%
  summarize(
    count = n(),
    median = median(placement_error_cm, na.rm = TRUE)
  )
friedman.test(median ~ object_material | subject, data = friedman_material) # p = .005


write.csv(data_NO, "C:/Users/Updates/Desktop/data_NO.csv", row.names = FALSE)
# most recent

################### Preliminary Eye Tracking Data ####################


# placement_error_cm
ggscatter(data_NO, x = "Total_duration_of_fixations", y = "placement_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.coeff.args = list(method = "spearman", label.x = 4000, label.sep = "\n"), xlab = "Total duration of fixations", ylab = "Placement Error (cm)") # sig

duration_filter <- data_NO %>%
  filter(data_NO$Total_duration_of_fixations < 2100)

ggscatter(duration_filter, x = "Total_duration_of_fixations", y = "placement_error_cm", add = "reg.line", conf.int = TRUE, size = 1,
          cor.coef = TRUE, cor.coeff.args = list(method = "spearman", label.x = 1500, label.y = 120, label.sep = "\n"), xlab = "Total duration of fixations", ylab = "Placement Error (cm)") # sig

ggscatter(data_NO, x = "Number_of_fixations", y = "placement_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.coeff.args = list(method = "spearman", label.x = 12, label.sep = "\n"), xlab = "Number of fixations", ylab = "Placement Error (cm)") # sig
fixation_filter <- data_NO %>%
  filter(data_NO$Number_of_fixations < 11 & data_NO$Number_of_fixations > 0)
ggscatter(fixation_filter, x = "Number_of_fixations", y = "placement_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.coeff.args = list(method = "spearman", label.x = 7, label.y = 120, label.sep = "\n"), xlab = "Number of fixations", ylab = "Placement Error (cm)") # sig



# non parametric test for number of fixation: fixated or not
fixation_test <- data_NO %>%
  group_by(subject, Fixated_or_not) %>%
  summarize(
    data_median = median(placement_error_cm, na.rm = TRUE)
  )

fixation_wide <- spread(fixation_test, Fixated_or_not, data_median)

wilcox.test(fixation_wide$no, fixation_wide$yes, paired = TRUE) # not sig, so no diff between fixated and not fixated


##### general linear model #####
mixed.lmer <- lmer(placement_error_cm_log ~ walk_noWalk + same_diff + (1|object_size) + (1|object_material) + (1|object_aliveORnot), data = data_log_NO) 
summary(mixed.lmer)

plot(mixed.lmer)
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))


ggscatter(data_NO, x = "Time_to_first_fixation", y = "placement_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman", xlab = "Time to first fixation", ylab = "Placement Error (cm)") # not sig

ggscatter(data_NO, x = "Duration_of_first_fixation", y = "placement_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE,  cor.coeff.args = list(method = "spearman", label.x = 1500, label.sep = "\n"), xlab = "Duration of irst_fixation", ylab = "Placement Error (cm)") # sig

ggscatter(data_NO, x = "Total_duration_of_whole_fixations", y = "placement_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE,  cor.coeff.args = list(method = "spearman", label.x = 4000, label.sep = "\n"), xlab = "Total duration of whole fixations", ylab = "Placement Error (cm)") # sig

ggscatter(data_NO, x = "Number_of_whole_fixations", y = "placement_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE,  cor.coeff.args = list(method = "spearman", label.x = 10, label.sep = "\n"), xlab = "Number of whole fixations", ylab = "Placement Error (cm)") # sig

ggscatter(data_NO, x = "Time_to_first_whole_fixation", y = "placement_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman", xlab = "Time to first whole fixation", ylab = "Placement Error (cm)") # not sig

ggscatter(data_NO, x = "Duration_of_first_whole_fixation", y = "placement_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman", xlab = "Duration of first whole fixation", ylab = "Placement Error (cm)") # not sig

ggscatter(data_NO, x = "Total_duration_of_Visit", y = "placement_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE,  cor.coeff.args = list(method = "spearman", label.x = 4500, label.sep = "\n"), xlab = "Total duration of Visit", ylab = "Placement Error (cm)") # sig

ggscatter(data_NO, x = "Number_of_Visits", y = "placement_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE,  cor.coeff.args = list(method = "spearman", label.x = 6, label.sep = "\n"), xlab = "Number of Visits", ylab = "Placement Error (cm)") # sig

ggscatter(data_NO, x = "Time_to_first_Visit", y = "placement_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman", xlab = "Time to first Visit", ylab = "Placement Error (cm)") # not sig

ggscatter(data_NO, x = "Duration_of_first_Visit", y = "placement_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE,  cor.coeff.args = list(method = "spearman", label.x = 3000, label.sep = "\n"), xlab = "Duration of first Visit", ylab = "Placement Error (cm)") # not sig

# x_error_cm
ggscatter(data_NO, x = "Total_duration_of_fixations", y = "x_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.coeff.args = list(method = "spearman", label.x = 4000, label.sep = "\n"), xlab = "Total duration of fixations", ylab = "X Error (cm)") # not sig

ggscatter(data_NO, x = "Number_of_fixations", y = "x_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.coeff.args = list(method = "spearman", label.x = 10, label.sep = "\n"), xlab = "Number of fixations", ylab = "X Error (cm)") # not sig

ggscatter(data_NO, x = "Time_to_first_fixation", y = "x_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman", xlab = "Time to first fixation", ylab = "X Error (cm)") # not sig

ggscatter(data_NO, x = "Duration_of_first_fixation", y = "x_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE,  cor.coeff.args = list(method = "spearman", label.x = 1500, label.sep = "\n"), xlab = "Duration of irst_fixation", ylab = "X Error (cm)") # not sig

ggscatter(data_NO, x = "Total_duration_of_whole_fixations", y = "x_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE,  cor.coeff.args = list(method = "spearman", label.x = 4000, label.sep = "\n"), xlab = "Total duration of whole fixations", ylab = "X Error (cm)") # not sig

ggscatter(data_NO, x = "Number_of_whole_fixations", y = "x_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE,  cor.coeff.args = list(method = "spearman", label.x = 10, label.sep = "\n"), xlab = "Number of whole fixations", ylab = "X Error (cm)") # not sig

ggscatter(data_NO, x = "Time_to_first_whole_fixation", y = "x_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman", xlab = "Time to first whole fixation", ylab = "X Error (cm)") # not sig

ggscatter(data_NO, x = "Duration_of_first_whole_fixation", y = "x_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman", xlab = "Duration of first whole fixation", ylab = "X Error (cm)") # not sig

ggscatter(data_NO, x = "Total_duration_of_Visit", y = "x_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE,  cor.coeff.args = list(method = "spearman", label.x = 4500, label.sep = "\n"), xlab = "Total duration of Visit", ylab = "X Error (cm)") # not sig

ggscatter(data_NO, x = "Number_of_Visits", y = "x_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE,  cor.coeff.args = list(method = "spearman", label.x = 6, label.sep = "\n"), xlab = "Number of Visits", ylab = "X Error (cm)") # not sig

ggscatter(data_NO, x = "Time_to_first_Visit", y = "x_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman", xlab = "Time to first Visit", ylab = "X Error (cm)") # not sig

ggscatter(data_NO, x = "Duration_of_first_Visit", y = "x_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE,  cor.coeff.args = list(method = "spearman", label.x = 3000, label.sep = "\n"), xlab = "Duration of first Visit", ylab = "X Error (cm)") # not sig

# y_error_cm
ggscatter(data_NO, x = "Total_duration_of_fixations", y = "y_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.coeff.args = list(method = "spearman", label.x = 4000, label.sep = "\n"), xlab = "Total duration of fixations", ylab = "Y Error (cm)") # not sig

ggscatter(data_NO, x = "Number_of_fixations", y = "y_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.coeff.args = list(method = "spearman", label.x = 10, label.sep = "\n"), xlab = "Number of fixations", ylab = "Y Error (cm)") # not sig

ggscatter(data_NO, x = "Time_to_first_fixation", y = "y_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman", xlab = "Time to first fixation", ylab = "Y Error (cm)") # not sig

ggscatter(data_NO, x = "Duration_of_first_fixation", y = "y_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE,  cor.coeff.args = list(method = "spearman", label.x = 1500, label.sep = "\n"), xlab = "Duration of irst_fixation", ylab = "Y Error (cm)") # not sig

ggscatter(data_NO, x = "Total_duration_of_whole_fixations", y = "y_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE,  cor.coeff.args = list(method = "spearman", label.x = 4000, label.sep = "\n"), xlab = "Total duration of whole fixations", ylab = "Y Error (cm)") # not sig

ggscatter(data_NO, x = "Number_of_whole_fixations", y = "y_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE,  cor.coeff.args = list(method = "spearman", label.x = 10, label.sep = "\n"), xlab = "Number of whole fixations", ylab = "Y Error (cm)") # not sig

ggscatter(data_NO, x = "Time_to_first_whole_fixation", y = "y_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman", xlab = "Time to first whole fixation", ylab = "Y Error (cm)") # not sig

ggscatter(data_NO, x = "Duration_of_first_whole_fixation", y = "y_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman", xlab = "Duration of first whole fixation", ylab = "Y Error (cm)") # not sig

ggscatter(data_NO, x = "Total_duration_of_Visit", y = "y_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE,  cor.coeff.args = list(method = "spearman", label.x = 4500, label.sep = "\n"), xlab = "Total duration of Visit", ylab = "Y Error (cm)") # not sig

ggscatter(data_NO, x = "Number_of_Visits", y = "y_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE,  cor.coeff.args = list(method = "spearman", label.x = 6, label.sep = "\n"), xlab = "Number of Visits", ylab = "Y Error (cm)") # not sig

ggscatter(data_NO, x = "Time_to_first_Visit", y = "y_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman", xlab = "Time to first Visit", ylab = "Y Error (cm)") # not sig

ggscatter(data_NO, x = "Duration_of_first_Visit", y = "y_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE,  cor.coeff.args = list(method = "spearman", label.x = 3000, label.sep = "\n"), xlab = "Duration of first Visit", ylab = "Y Error (cm)") # not sig





cor(data_NO$x_error_cm, data_NO$y_error_cm)

ggscatter(data_NO, x = "x_error_cm", y = "y_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE,  cor.coeff.args = list(method = "spearman", label.x = 2, label.sep = "\n"), xlab = "X Error (cm)", ylab = "Y Error (cm)") # not sig
hist(data_NO$x_error_cm)
hist(data_NO$y_error_cm)



data_NO$placement_error_cm_scale <- scale(data_NO$placement_error_cm, center = TRUE, scale = TRUE)
hist(data_NO$placement_error_cm)

basic.lm <- lm(placement_error_cm ~ Number_of_fixations, data = data_NO)
summary(basic.lm)

plot(basic.lm, which = 1)
plot(basic.lm, which = 2)

(split_plot <- ggplot(aes(Number_of_fixations, placement_error_cm), data = data_NO) + 
    geom_point() + 
    facet_wrap(~ trial) + # create a facet for each mountain range
    xlab("Number of Fixations") + 
    ylab("placement Error (cm)"))

library(lme4)


### fixation number and duration for living and non-living objects
fixation_alive <- data_NO %>%
  group_by(subject, object_aliveORnot) %>%
  summarize(
    data_median = median(Number_of_fixations, na.rm = TRUE)
  )
friedman.test(data_median ~ object_aliveORnot | subject, data = fixation_alive) # p = .6171

fixdur_alive <- data_NO %>%
  group_by(subject, object_aliveORnot) %>%
  summarize(
    data_median = median(Total_duration_of_fixations, na.rm = TRUE)
  )
friedman.test(data_median ~ object_aliveORnot | subject, data = fixdur_alive) # p = .09467

### fixation for inflatable and non-inflatable
fixation_material <- data_NO %>%
  group_by(subject, object_material) %>%
  summarize(
    data_median = median(Number_of_fixations, na.rm = TRUE)
  )
friedman.test(data_median ~ object_material | subject, data = fixation_material) # p = .7815

fixdur_material <- data_NO %>%
  group_by(subject, object_material) %>%
  summarize(
    data_median = median(Total_duration_of_fixations, na.rm = TRUE)
  )
friedman.test(data_median ~ object_material | subject, data = fixdur_material) # p = .5775
