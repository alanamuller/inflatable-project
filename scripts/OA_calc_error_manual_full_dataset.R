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

rm(list = ls())

setwd("D:/Nav_1stYr_project_data")
# Had to manually paste in the young adult data

# Read in data
inputData <- read_csv("D:/Nav_1stYr_project_data/manuscript_data_OA_preprocessed.csv")

myData <- inputData
str(myData)

# fix the data frame to have the correct properties
myData <- as.data.frame(myData)
myData$subject <- as.factor(myData$subject)
myData$trial <- as.factor(myData$trial)
myData$wall_side <- as.factor(myData$wall_side)
myData$object <- as.factor(myData$object)
myData$object_size <- as.factor(myData$object_size)
myData$object_material <- as.factor(myData$object_material)
myData$object_aliveORnot <- as.factor(myData$object_aliveORnot)
myData$walk_noWalk <- as.factor(myData$walk_noWalk)
myData$same_diff <- as.factor(myData$same_diff)
myData$start_wall <- as.factor(myData$start_wall)
myData$height <- as.factor(myData$height)
myData$width <- as.factor(myData$width)
myData$next_to_landmark <- as.factor(myData$next_to_landmark)
myData$which_landmark <- as.factor(myData$which_landmark)
myData$cart <- as.factor(myData$cart)
myData$group <- as.factor(myData$group)
myData$`objects_put_back_order (same/not_same)` <- as.factor(myData$`objects_put_back_order (same/not_same)`)
myData$replace_strategy <- as.factor(myData$replace_strategy)
myData$`cart (took/left/half)`<- as.factor(myData$`cart (took/left/half)`)
myData$Gender <- as.factor(myData$Gender)
myData$Video_game_exp <- as.factor(myData$Video_game_exp)
myData$Fixated_or_not <- as.factor(myData$Fixated_or_not)
myData$number <- as.factor(myData$number)

myData$abs_x_error_cm <- as.numeric(myData$abs_x_error_cm)
myData$abs_y_error_cm <- as.numeric(myData$abs_y_error_cm)

### Group of all data and subgroups of data

myData <- unite(myData, move_view, c(walk_noWalk, same_diff), remove = FALSE)
myData$move_view <- as.factor(myData$move_view)

##### group by trial and output for gazecode analysis stuff #####
dataByTrial <- myData %>%
  group_by(subject, trial) %>%
  summarize(
    x_error = mean(x_error, na.rm = TRUE), 
    y_error = mean(y_error, na.rm = TRUE), 
    placement_error = mean(placement_error, na.rm = TRUE),
    x_error_cm = mean(x_error_cm, na.rm = TRUE), 
    y_error_cm = mean(y_error_cm, na.rm = TRUE), 
    placement_error_cm = mean(placement_error_cm, na.rm = TRUE),
    abs_x_error_cm = mean(abs_x_error_cm, na.rm = TRUE), 
    abs_y_error_cm = mean(abs_y_error_cm, na.rm = TRUE),
    fix_num = mean(fix_num, na.rm = TRUE), 
    total_fix_dur_ms = mean(total_fix_dur_ms, na.rm = TRUE),
    avg_fix_dur_ms = mean(avg_fix_dur_ms, na.rm = TRUE)
  )

# Save file
#write.csv(dataByTrial, "D:/Nav_1stYr_project_data/OA_dataByTrial_gazecode.csv", row.names = FALSE)

################ Log transform placement error_cm ##################### still not normal

hist(myData$placement_error_cm)
myData$placement_error_cm_log <- log10(myData$placement_error_cm +1)
hist(myData$placement_error_cm_log)

ggqqplot(myData$placement_error_cm_log)
shapiro.test(myData$placement_error_cm_log) # still really skewed but better
mean_log <- mean(myData$placement_error_cm_log, na.rm = TRUE)
sd_log <- sd(myData$placement_error_cm_log, na.rm = TRUE)

# Make a dataframe for younger and older adults separately
ya_data <- myData %>%
  filter(myData$group == "YA")

oa_data <- myData %>%
  filter(myData$group == "OA")

# get rid of outliers in ya and oa data and full data
data_log_NO <- myData %>%
  filter(placement_error_cm_log < mean_log+(sd_log*3) & placement_error_cm_log > mean_log-(sd_log*3))
hist(data_log_NO$placement_error_cm_log)
ggqqplot(data_log_NO$placement_error_cm_log)
shapiro.test(data_log_NO$placement_error_cm_log)

data_by_subj_NO <- data_log_NO %>%
  group_by(group, number, walk_noWalk, same_diff) %>%
  summarize(
    mean = mean(placement_error_cm_log, na.rm = TRUE),
  )
data_by_subj_NO <- as_tibble(data_by_subj_NO)

# YAs
ya_mean_log <- mean(ya_data$placement_error_cm_log, na.rm = TRUE)
ya_sd_log <- sd(ya_data$placement_error_cm_log, na.rm = TRUE)

ya_log_NO <- ya_data %>%
  filter(placement_error_cm_log < ya_mean_log+(ya_sd_log*3) & placement_error_cm_log > ya_mean_log-(ya_sd_log*3))

# OAs
oa_mean_log <- mean(oa_data$placement_error_cm_log, na.rm = TRUE)
oa_sd_log <- sd(oa_data$placement_error_cm_log, na.rm = TRUE)

oa_log_NO <- oa_data %>%
  filter(placement_error_cm_log < oa_mean_log+(oa_sd_log*3) & placement_error_cm_log > oa_mean_log-(oa_sd_log*3))

#################### 2-way ANOVA with walk and view ################## no sig diff

bxp_ya_oa <- ggboxplot(
  data_by_subj_NO, x = "walk_noWalk", y = "mean", 
  color = "same_diff", add = "jitter", facet.by = "group",
  xlab = "Movement Condition", ylab = "Placement Error (log cm)",
  legend = "right", legend.title = "Viewpoint") + 
  scale_x_discrete(breaks=c("no walk", "walk"), labels=c("Stationary", "Walk")) +
  scale_color_discrete(labels = c("Different", "Same")) +
  theme(axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15), 
        strip.text = element_text(size = 15))

#jpeg("movement_viewpoint_bxp.jpeg", width = 7, height = 6, units = 'in', res = 500)
bxp_ya_oa
#dev.off()



### quick t-test to see if YAs error was lower than OAs error
t_test_table_OAYA <- data_log_NO %>%
  group_by(group, number) %>%
  summarize(
    count = n(), 
    placement_error_cm_log = median(placement_error_cm_log, na.rm = TRUE)
  )

oa_error <- t_test_table_OAYA %>%
  filter(group == "OA")

ya_error <- t_test_table_OAYA %>%
  filter(group == "YA")

t.test(oa_error$placement_error_cm_log, ya_error$placement_error_cm_log) # this is sig so YA has sig less error

friedman_move_view_table <- data_NO %>%
  group_by(subject, move_view) %>%
  summarize(
    count = n(),
    placement_error_cm = median(placement_error_cm, na.rm = TRUE),
  )

##### Is age correlated with error? Turns out yes, yes it is
age_error_table <- data_log_NO %>%
  group_by(subject) %>%
  summarize(
    age = mean(Age), 
    placement_error_cm_log = median(placement_error_cm_log)
  )

plot(age_error_table$age, age_error_table$placement_error_cm_log)
cor.test(age_error_table$age, age_error_table$placement_error_cm_log, use = "complete.obs")

##### oa error cor with fixation time and number
oa_fixated <- oa_data %>%
  filter(Fixated_or_not == "yes")

oa_fix_table <- oa_fixated %>%
  group_by(trial, subject) %>%
  summarize(
    count = n(),
    fix_num = median(fix_num), 
    total_fix_dur_ms = median(total_fix_dur_ms), 
    avg_fix_dur_ms = median(avg_fix_dur_ms)
  )

##### Do older adults fixate on less object?
not_fix_data <- data_log_NO %>%
  filter(fix_num == 0)

aov_smallData <- na.omit(data_log_NO)

aov_trialData <- aov_smallData %>%
  select(subject, walk_noWalk, same_diff, placement_error_cm_log)

aov_data <- aov_smallData %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    mean = mean(placement_error_cm_log, na.rm = TRUE),
  )
aov_data <- as_tibble(aov_data)
aov_data$subject <- as.factor(aov_data$subject)
aov_data$walk_noWalk <- as.factor(aov_data$walk_noWalk)
aov_data$same_diff <- as.factor(aov_data$same_diff)

results <- aov(mean ~ walk_noWalk*same_diff + Error(subject/(walk_noWalk*same_diff)), data = aov_data)
summary(results)

# Right above works!!!

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

friedman.test(median ~ move_view | subject, data = friedmanTableCM) # p = .81

res.fried <- friedmanTableCM %>% friedman_test(median ~ move_view |subject)
res.fried 

friedmanTableCM %>%
  group_by(move_view) %>%
  identify_outliers(median) # no outliers

bxp <- ggboxplot(
  friedmanTableCM, x = "move_view", y = "median", add = "jitter", title = "Placement error in cm (accounting for both x and y-axes)", 
  xlab = "Movement and Replacement View Conditions", ylab = "Placement Error (cm)") +
  scale_y_continuous(breaks = seq(0,60,10))
bxp

bxp <- ggboxplot(
  friedmanTableCM, x = "move_view", y = "median", add = "jitter", 
  xlab = "Movement and Viewpoint Conditions", ylab = "Placement Error (cm)") +
  scale_y_continuous(breaks = seq(0,60,10))
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

friedmanTableCM2 <- data_NO %>%
  group_by(subject, walk_noWalk, same_diff) %>%
  summarize(
    count = n(),
    placement_error_cm = median(placement_error_cm, na.rm = TRUE)
  )

bxp <- ggboxplot(
  friedmanTableCM2, x = "walk_noWalk", y = "placement_error_cm", color = "same_diff", add = "jitter",
  xlab = "Movement Condition", ylab = "Placement Error (cm)") +   
  scale_y_continuous(breaks = seq(0,80,20)) + scale_x_discrete(breaks=c("no walk", "walk"), labels=c("Stationary", "Walk")) +
  labs(color = "Viewpoint") + scale_color_discrete(labels = c("Different", "Same"))
bxp

# Graph: Placement error by movement and orientation with full data
ggboxplot(data_NO, x = "walk_noWalk", y = "placement_error_cm", color = "same_diff", add = "jitter") +
  xlab("Movement Condition") +
  ylab("Placement Error (cm)") +
  scale_x_discrete(labels = c("No Walk", "Walk")) +
  labs(color = "Retrieval Viewpoint") +
  scale_y_continuous(breaks = seq(0,140,10))

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

# Placement error (cm) values
ggboxplot(datamelt_xvy3, x = "type_error", y = "value", add = "jitter",
          color = "type_error", ylab = "Placement Error (cm)", 
          xlab = "Type of Error") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), legend.position = "none") +
  scale_y_continuous(breaks = seq(-100,100,20))

kruskal.test(value ~ type_error, data = datamelt_xvy3) # p = 0.006

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

ggboxplot(data_NO, x = "wall_side", y = "placement_error_cm", add = "jitter",
          color = "wall_side", ylab = "Placement Error (cm)", 
          xlab = "Wall Side") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), legend.position = "none") +
  scale_x_discrete(labels = c("Double Door", "Single Door")) +
  scale_y_continuous(breaks = seq(0,100,10))

ggplot(data_NO, aes(x = wall_side, y = placement_error_cm, fill = subject)) +
  geom_boxplot(aes(fill=subject), position=position_dodge(.9)) + 
  theme_classic() +
  stat_summary(fun=mean, geom="point", aes(group=subject), position=position_dodge(.9), 
               color="red", size=2) +
  guides(fill = guide_legend(title = "Subject")) +
  xlab("Movement and View Condition") +
  ylab("Placement Error (cm)") +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16), legend.text = element_text(size = 11), legend.title = element_text(size = 12)) +
  scale_fill_brewer(palette = "Paired") +
  scale_x_discrete(labels = c("Double Door Wall", "Single Door Wall"))

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


ggline(landmark_data, x = "next_to_landmark", y = "data_median", color = "subject", title = "Placement error in cm (accounting for both x and y-axes)") +
  xlab("Movement and Replacement View Conditions") +
  ylab("Placement Error (cm)") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0,100,10))

ggboxplot(data_NO, x = "which_landmark", y = "placement_error_cm", add = "jitter",
          color = "which_landmark", ylab = "Placement Error (cm)", 
          xlab = "Wall Landmarks") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), legend.position = "none") +
  scale_y_continuous(breaks = seq(0,100,10))

kruskal.test(placement_error ~ which_landmark, data = data_NO) # p < .00001

pairwise.wilcox.test(data_NO$placement_error_cm, data_NO$which_landmark,
                     p.adjust.method = "BH") # fire alarm is sig diff from everything except thermostat and light switch



ggboxplot(data_NO, x = "object", y = "placement_error_cm", 
          color = "object", ylab = "Placement Error (cm)", 
          xlab = "object") +
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), legend.position = "none")

kruskal.test(placement_error ~ object, data = data_NO) # p = .0357

# object size

ggboxplot(data_NO, x = "object_size", y = "placement_error_cm", add = "jitter",
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

ggboxplot(data_NO, x = "object_material", y = "placement_error_cm", add = "jitter",
          color = "object_material", ylab = "Placement Error (cm)", 
          xlab = "Object Material") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14), legend.position = "none") +
  scale_y_continuous(breaks = seq(0,100,10))

kruskal.test(placement_error ~ object_material, data = data_NO) # p = .001885

# alive or not

ggboxplot(data_NO, x = "object_aliveORnot", y = "placement_error_cm", add = "jitter",
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
ggscatter(data_NO, x = "total_fix_dur_ms", y = "placement_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.coeff.args = list(method = "spearman", label.x = 4000, label.sep = "\n"), 
          xlab = "Total duration of fixations", ylab = "Placement Error (cm)", size = .65) # sig

duration_filter <- data_NO %>%
  filter(data_NO$Total_duration_of_fixations < 2100)

ggscatter(duration_filter, x = "Total_duration_of_fixations", y = "placement_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.coeff.args = list(method = "spearman", label.x = 1500, label.sep = "\n"), 
          xlab = "Total duration of fixations", ylab = "Placement Error (cm)", size = .65) # sig

ggscatter(data_NO, x = "fix_num", y = "placement_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.coeff.args = list(method = "spearman", label.x = 12, label.sep = "\n"), 
          xlab = "Number of fixations", ylab = "Placement Error (cm)", size = .65) # sig

fixation_filter <- data_NO %>%
  filter(data_NO$Number_of_fixations < 11 & data_NO$Number_of_fixations > 0)

ggscatter(fixation_filter, x = "Number_of_fixations", y = "placement_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.coeff.args = list(method = "spearman", label.x = 8, label.sep = "\n"), 
          xlab = "Number of fixations", ylab = "Placement Error (cm)", size = .65) # sig



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

ggscatter(data_NO, x = "Time_to_first_fixation", y = "placement_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman", xlab = "Time to first fixation", ylab = "Placement Error (cm)") # not sig

ggscatter(data_NO, x = "Duration_of_first_fixation", y = "placement_error_cm", add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE,  cor.coeff.args = list(method = "spearman", label.x = 1500, label.sep = "\n"), xlab = "Duration of first_fixation", ylab = "Placement Error (cm)") # sig

ggscatter(data_NO, x = "Total_duration_of_whole_fixations", y = "placement_error_cm", add = "reg.line", conf.int = TRUE, size = .75,
          cor.coef = TRUE,  cor.coeff.args = list(method = "spearman", label.x = 4000, label.sep = "\n"), xlab = "Total duration of whole fixations", ylab = "Placement Error (cm)") +
  scale_y_continuous(breaks = seq(0,120, 20)) +
  scale_x_continuous(breaks = seq(0,6000,1000))# sig

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
