library(stringr)
library(tidyverse)
library(dplyr)

# Made by Alana Muller with a lot of help from ChatGPT

# Set working directory
setwd("C:/Users/amuller/Desktop/Alana/UA/HSCL/Stress Shortcuts/stress-shortcuts-collab/data/tmp")

# Load the data
input_file <- "navStress_pilotAM_city1_navigation_23-03-21D_16.35.12T.log"
input_data <- paste(readLines(input_file), collapse="\n")

# Convert the data to a tibble
data_df <- tibble(input_data = str_split(input_data, "\n")[[1]])

# Use regular expressions to extract the number before "Avatar:"
data_df <- data_df %>%
  mutate(avatar_number = str_extract(input_data, "\\d+(?=\\tAvatar:)"))

# Use regular expressions to extract the three numbers after "Position (xyz):"
data_df <- data_df %>%
  mutate(numbers = str_extract_all(input_data, "(?<=Position \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))

# Split the numbers column into three separate columns
data_df <- data_df %>%
  separate(numbers, into = c("pos_X", "pos_Y", "pos_Z"), sep = "\t")

# Use regular expressions to extract the three numbers after "Rotation (xyz):"
data_df <- data_df %>%
  mutate(numbers = str_extract_all(input_data, "(?<=Rotation \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))

# Split the numbers column into three separate columns
data_df <- data_df %>%
  separate(numbers, into = c("rot_X", "rot_Y", "rot_Z"), sep = "\t")

# Convert the columns to numeric
data_df <- data_df %>%
  mutate(across(2:8, as.numeric))

# Remove the rows with NAs
data_df <- na.omit(data_df)

# Make column for time stamp by making avatar number start from 0
avatar_initial_number <- data_df$avatar_number[1]
data_df <- data_df %>%
  mutate(avatar_time = avatar_number - avatar_initial_number)

##### Above this section, the code works but it doesn't split the chunks into learned trials 

#x <- data_df$pos_X
#z <- data_df$pos_Z

#plot(x,z)


# Still need to split the data into the different chunks (each learning trial and navigation trial)
# TASK_START LearnSmoothPassive SmoothPassivePathStart
# TASK_START LearnActivePath ActivePathStart
# TASK_START	TASK_NavigateInOrder
# TASK_START TASK_NavigationTest
