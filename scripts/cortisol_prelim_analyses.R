library(tidyverse)
library(ggpubr)
library(rstatix)
library(readxl)
library(magrittr)
library(dplyr)

# Start fresh by removing everything from the environment
rm(list = ls())

# Set working directory
setwd("E:/Nav Stress Data/Salimetrics reports") # from hard drive

# Enter the data 
samples9Data <- readxl::read_excel("saliva_data_bySubject.xlsx", sheet = "9samples")
samples12Data <- readxl::read_excel("saliva_data_bySubject.xlsx", sheet = "12samples")

samples9Data$cort_nmol_L <- samples9Data$mean_cort*276
samples12Data$cort_nmol_L <- samples12Data$mean_cort*276

all_data <- rbind(samples9Data, samples12Data)

# Set the order of the x-axis
level_order <- c('pre', 'post1', 'post15', 'post30')

##### Pics of all data

# Plot with all participant separated by condition and time
ggplot(data = all_data, aes(x=factor(time, level = level_order), y=cort_nmol_L)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter() +
  facet_wrap(vars(condition)) +
  labs(x = "Time", y = "Cortisol (nmol/L)" )

ggplot(data = all_data, aes(x=factor(time, level = level_order), y=cort_nmol_L)) +
  geom_point() +
  geom_line(aes(group = subjNum)) +
  facet_grid(vars(condition)) +
  labs(x = "Time", y = "Cortisol (nmol/L)" )

bxp <- ggboxplot(
  all_data, x = "time", y = "cort_nmol_L", 
  color = "condition"
)
bxp

##### Pics without 10, 15, 20, 23, 28 that didn't finish the cp task

# Participants to exclude
participants_to_exclude <- c(10, 15, 20, 23, 28)

# Exclude data from the specified participants
finished_cp <- all_data %>%
  filter(!subjNum %in% participants_to_exclude)

# Plot with all participant separated by condition and time
ggplot(data = finished_cp, aes(x=factor(time, level = level_order), y=cort_nmol_L)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter() +
  facet_wrap(vars(condition)) +
  labs(x = "Time", y = "Cortisol (nmol/L)" )

########## Checking assumptions

##### All data

# all_data outliers
outliers_allData <- all_data %>%
  group_by(time, condition) %>%
  identify_outliers(cort_nmol_L)

mean_cort <- mean(all_data$cort_nmol_L)
sd_cort <- sd(all_data$cort_nmol_L)

no_outliers_allData <- subset(all_data, all_data$cort_nmol_L > mean_cort - sd_cort*2.5 & all_data$cort_nmol_L < mean_cort + sd_cort*2.5)

no_outliers_allData$log_cort <- log(no_outliers_allData$cort_nmol_L)

# Checking normality
ggqqplot(no_outliers_allData$cort_nmol_L)
hist(no_outliers_allData$cort_nmol_L)
hist(log(no_outliers_allData$cort_nmol_L))

normality_allData <- no_outliers_allData %>%
  group_by(time) %>%
  shapiro_test(log_cort)

# anova
res.aov <- anova_test(data = no_outliers_allData, dv = log_cort, wid = subjNum, within = c(condition,time))
get_anova_table(res.aov)

# testing simple main effects

# testing effect of condition at every time point
one.way <- no_outliers_allData %>%
  group_by(time) %>%
  anova_test(dv = log_cort, wid = subjNum, within = condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

# unequal cases so pairing it won't work
pwc <- no_outliers_allData %>%
  group_by(time) %>%
  pairwise_t_test(
    log_cort ~ condition, paired =  FALSE,
    p.adjust.method = "bonferroni"
  )
pwc


one.way2 <- no_outliers_allData %>%
  group_by(condition) %>%
  anova_test(dv = log_cort, wid = subjNum, within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2

# unequal cases so pairing it won't work
pwc2 <- no_outliers_allData %>%
  group_by(condition) %>%
  pairwise_t_test(
    log_cort ~ time, paired = FALSE,
    p.adjust.method = "bonferroni"
  )
pwc2


# Make a plot
cond.labs <- c("Cold Pressor", "Control Condition", "Fire Environment")
names(cond.labs) <- c("cp", "ctrl", "fire")

# Plot with all participant separated by condition and time
ggplot(data = no_outliers_allData, aes(x=factor(time, level = level_order), y=log_cort)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter() +
  facet_wrap(vars(condition), labeller = labeller(condition = cond.labs)) +
  labs(x = "Time", y = " Log Cortisol (log nmol/L)" )


##### Stats without the 5 that didn't finish cp task
# outliers
outliers_finished_cp <- finished_cp %>%
  group_by(time, condition) %>%
  identify_outliers(cort_nmol_L)

mean_cort <- mean(finished_cp$cort_nmol_L)
sd_cort <- sd(finished_cp$cort_nmol_L)

no_outliers_finished_cp <- subset(finished_cp, finished_cp$cort_nmol_L > mean_cort - sd_cort*2.5 & finished_cp$cort_nmol_L < mean_cort + sd_cort*2.5)

no_outliers_finished_cp$log_cort <- log(no_outliers_finished_cp$cort_nmol_L)

# Checking normality
ggqqplot(no_outliers_finished_cp$cort_nmol_L)
hist(no_outliers_finished_cp$cort_nmol_L)
hist(log(no_outliers_finished_cp$cort_nmol_L))

normality_allData <- no_outliers_finished_cp %>%
  group_by(time, condition) %>%
  shapiro_test(log_cort) # only one sig value so looks pretty good

# anova
res.aov <- anova_test(data = no_outliers_finished_cp, dv = log_cort, wid = subjNum, within = c(condition,time))
get_anova_table(res.aov) # sig time and condition/time interaction

# testing simple main effect of time
one.wayTime <- no_outliers_finished_cp %>%
  group_by(time) %>%
  anova_test(dv = log_cort, wid = subjNum, within = condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.wayTime # noting survived adjusted p value



# Make a plot
cond.labs <- c("Cold Pressor", "Control", "Fire Environment")
names(cond.labs) <- c("cp", "ctrl", "fire")

# Plot with all participant separated by condition and time
cort_graph1 <- ggplot(data = no_outliers_finished_cp, aes(x=factor(time, level = level_order), y=log_cort)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter() +
  facet_wrap(vars(condition), labeller = labeller(condition = cond.labs)) +
  labs(x = "Time", y = " Log Cortisol (log nmol/L)" ) +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 17), 
        strip.text = element_text(size = 13)) +
  scale_x_discrete(labels = c("Pre", "Post1", "Post15", "Post30"))


#jpeg("C:/Users/amuller/Desktop/Alana/UA/HSCL/Conferences/iNAV 2024/log_cortisol1.jpeg", width = 9, height = 5.75, units = 'in', res = 500)
cort_graph1
#dev.off()



##### Stats with only cases that had 12 samples - aka complete cases
# exclude participant 16, 17, 20, 22, and 23 for now - some incomplete and some waiting on Salimetrics analysis

# Participants to exclude
participants_to_exclude <- c(16, 17, 20, 22, 23)

# Exclude data from the specified participants
completeSamples12 <- samples12Data %>%
  filter(!subjNum %in% participants_to_exclude)

# Checking normality
ggqqplot(completeSamples12$cort_nmol_L)
hist(completeSamples12$cort_nmol_L)
hist(log(completeSamples12$cort_nmol_L))

completeSamples12$log_cort <- log(completeSamples12$cort_nmol_L)

normality_12samples <- completeSamples12 %>%
  group_by(time) %>%
  shapiro_test(log_cort) # normal!

res.aov <- anova_test(data = completeSamples12, dv = log_cort, wid = subjNum, within = c(condition,time))
get_anova_table(res.aov) # sig interaction

# testing effect of condition at every time point
one.way3 <- completeSamples12 %>%
  group_by(time) %>%
  anova_test(dv = log_cort, wid = subjNum, within = condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way3 # nothing survived adjusted p

# check pairwise comparisons
pwc3 <- completeSamples12 %>%
  group_by(time) %>%
  pairwise_t_test(
    log_cort ~ condition, paired =  TRUE,
    p.adjust.method = "bonferroni"
  )
pwc3 # nothing survived adjusted p

one.way4 <- completeSamples12 %>%
  group_by(condition) %>%
  anova_test(dv = log_cort, wid = subjNum, within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way4 # cp and ctrl sig, survived adjusted p

# pairwise comparisons
pwc4 <- completeSamples12 %>%
  group_by(condition) %>%
  pairwise_t_test(
    log_cort ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc4 # 3 comparisons sig

# plot for iNAV
# Make a plot
cond.labs <- c("Cold Pressor", "Control", "Fire Environment")
names(cond.labs) <- c("cp", "ctrl", "fire")

# Plot with all participant separated by condition and time
cort_graph <- ggplot(data = completeSamples12, aes(x=factor(time, level = level_order), y=log_cort)) +
  geom_boxplot(outliers = FALSE, coef = 0) +
  geom_point() +
  geom_line(aes(group = subjNum)) +
  facet_wrap(vars(condition), labeller = labeller(condition = cond.labs)) +
  labs(x = "Time", y = " Log Cortisol (log nmol/L)" ) +
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 17), 
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 17), 
        strip.text = element_text(size = 13)) +
  scale_x_discrete(labels = c("Pre", "Post1", "Post15", "Post30"))


jpeg("C:/Users/amuller/Desktop/Alana/UA/HSCL/Conferences/iNAV 2024/pics/log_cortisol.jpeg", width = 9, height = 5.75, units = 'in', res = 500)
cort_graph
dev.off()


##### Correct using baseline so subtract pre from all posts
smallData <- all_data[,c(3,6,9,11)]
spreadData <- spread(smallData, condition_time, mean_cort)
spreadData <- na.omit(spreadData)

spreadData$cp_preMinusPre <- spreadData$cp_pre - spreadData$cp_pre
spreadData$cp_post1MinusPre <- spreadData$cp_post1 - spreadData$cp_pre
spreadData$cp_post15MinusPre <- spreadData$cp_post15 - spreadData$cp_pre
spreadData$cp_post30MinusPre <- spreadData$cp_post30 - spreadData$cp_pre

spreadData$ctrl_preMinusPre <- spreadData$ctrl_pre - spreadData$ctrl_pre
spreadData$ctrl_post1MinusPre <- spreadData$ctrl_post1 - spreadData$ctrl_pre
spreadData$ctrl_post15MinusPre <- spreadData$ctrl_post15 - spreadData$ctrl_pre
spreadData$ctrl_post30MinusPre <- spreadData$ctrl_post30 - spreadData$ctrl_pre

spreadData$fire_preMinusPre <- spreadData$fire_pre - spreadData$fire_pre
spreadData$fire_post1MinusPre <- spreadData$fire_post1 - spreadData$fire_pre
spreadData$fire_post15MinusPre <- spreadData$fire_post15 - spreadData$fire_pre
spreadData$fire_post30MinusPre <- spreadData$fire_post30 - spreadData$fire_pre

# name the columns you want to gather, the other columns will remain there
gathered_columns <- c("cp_post1MinusPre", "cp_post15MinusPre", "cp_post30MinusPre", "ctrl_post1MinusPre", "ctrl_post15MinusPre", "ctrl_post30MinusPre", "fire_post1MinusPre", "fire_post15MinusPre", "fire_post30MinusPre")

# gather the data for grid numbers
baselineCorr_data <- gather(spreadData, key = cort_type, value = mean_cort_change, gathered_columns, factor_key = TRUE)

baselineCorr_data <- baselineCorr_data %>%
  separate(cort_type, into = c("condition", "time"), sep = "_")

# Set the order of the x-axis
level_order <- c('post1MinusPre', 'post15MinusPre', 'post30MinusPre')

# Plot cort separated by condition and time
ggplot(data = baselineCorr_data, aes(x=factor(time, level = level_order), y=mean_cort_change)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter() +
  facet_wrap(vars(condition)) +
  labs(x = "Time", y = "Cortisol (nmol/L)" )

# Checking normality
ggqqplot(baselineCorr_data$mean_cort_change)
hist(baselineCorr_data$mean_cort_change)
hist(log(baselineCorr_data$mean_cort_change))

normality_baselineCort <- baselineCorr_data %>%
  group_by(condition, time) %>%
  shapiro_test(mean_cort_change) # normal!

res.aov <- anova_test(data = baselineCorr_data, dv = mean_cort_change, wid = subjNum, within = c(condition,time))
get_anova_table(res.aov) # sig main effects, no sig interaction

# testing effect of condition at every time point
one.way5 <- baselineCorr_data %>%
  group_by(time) %>%
  anova_test(dv = mean_cort_change, wid = subjNum, within = condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way5

# check pairwise comparisons
pwc5 <- baselineCorr_data %>%
  group_by(time) %>%
  pairwise_t_test(
    mean_cort_change ~ condition, paired =  TRUE,
    p.adjust.method = "bonferroni"
  )
pwc5

one.way6 <- baselineCorr_data %>%
  group_by(condition) %>%
  anova_test(dv = mean_cort_change, wid = subjNum, within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way6

# unequal cases so pairing it won't work
pwc6 <- baselineCorr_data %>%
  group_by(condition) %>%
  pairwise_t_test(
    mean_cort_change ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc6












