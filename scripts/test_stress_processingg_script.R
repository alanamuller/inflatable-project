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


library(stringr)
library(tidyverse)

# Example string with multiple instances of TASK_START and TASK_END
text <- "Random text before TASK_START First task text TASK_END Random text before TASK_START Second task text TASK_END Random text after TASK_END"

# Define the start and end keywords
start_keyword <- "TASK_START"
end_keyword <- "TASK_END"

# Split the text into separate lines
lines <- str_split(text, "\n")[[1]]

# Find the indices of the start and end keywords
start_indices <- which(str_detect(lines, start_keyword))
end_indices <- which(str_detect(lines, end_keyword))

# Initialize an empty list to hold the dataframes
dfs <- list()

# Loop through the start indices
for (i in start_indices) {
  # Find the index of the next end keyword
  j <- min(end_indices[end_indices > i])
  
  # Extract the lines between the start and end indices
  task_lines <- lines[(i+1):(j-1)]
  
  # Convert the lines to a single string
  task_text <- paste(task_lines, collapse = "\n")
  
  # Add the task text to the list of dataframes
  dfs[[length(dfs)+1]] <- data.frame(task_text = task_text)
}

# Combine the list of dataframes into a single dataframe
result <- bind_rows(dfs)

# Print the result
result
