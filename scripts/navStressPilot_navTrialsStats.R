library(readxl)


# Clear the environment to start fresh

rm(list = ls())

# Set the directory where the Excel files are stored
setwd("E:/Nav Stress Pilot Data")

import_data <- read_excel("pilot_navTestTrials.xlsx", sheet = "Sheet 1")

# Copy of data to play with
data <- import_data

# Fix structure of the data: columns 1:7 should be factors
data <- data %>%
  mutate_at(vars(1:7), as.factor)

summary(data$store_start_end)




