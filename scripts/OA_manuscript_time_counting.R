# Data file used for the manuscript results section
# Counting the amount of time it took to encode, collect, and retrieve
# Alana Muller
# amuller@arizona.edu
# 2022-10-28

rm(list = ls())

# work computer uses E but laptop uses D, change accordingly
setwd("E:/Nav_1stYr_project_data")

# Read in data
inputData <- read_excel("E:/Nav_1stYr_project_data/inflatable_time_counting.xlsx")
inputData <- as.data.frame(inputData)
str(inputData) # check the structure of the data

# Make a copy of inputData that I'll use for analysis
myData <- inputData

subject_data <- myData %>%
  group_by(ID, movement) %>%
  summarise(
    count = n(), 
    encode_mean = mean(total_study_time_s),
    encode_sd = sd(total_study_time_s),
    collect_mean = mean(total_object_collection_time_s), 
    collect_sd = sd(total_object_collection_time_s),
    retrieve_mean = mean(total_retrieval_time_s),
    retrieve_sd = sd(total_retrieval_time_s)
  )

hist(myData$total_study_time_s)
hist(myData$total_object_collection_time_s)
hist(myData$total_retrieval_time_s)

# t-test

ggboxplot(subject_data, x = "movement", y = "collect_mean")
ggboxplot(subject_data, x = "movement", y = "encode_mean")
ggboxplot(subject_data, x = "movement", y = "retrieve_mean")

no_walk <- subset(subject_data, movement == "no walk", collect_mean, drop = TRUE)
walk <- subset(subject_data, movement == "walk", collect_mean, drop = TRUE)

collect_time <- t.test(no_walk, walk, paired = TRUE)
collect_time # very sig, p < .001, walk took more time

study_time <- t.test(data = subject_data, encode_mean ~ movement, paired = TRUE)
study_time

retrieve_time <- t.test(data = subject_data, retrieve_mean ~ movement, paired = TRUE)
retrieve_time # sig, p = 0.0248, walk took more time
