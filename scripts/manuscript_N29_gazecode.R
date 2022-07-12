# Data file used for the manuscript results section
# Alana Muller
# amuller@arizona.edu
# 2022-07-11

rm(list = ls())

setwd("E:/Nav_1stYr_project_data/GazeCode data")

# Read in data
inputData <- read_csv("E:/Nav_1stYr_project_data/GazeCode data/R_outputs/subject_002_gazeCodeCounts.csv")
inputData <- as.data.frame(inputData)
str(inputData) # check the structure of the data




