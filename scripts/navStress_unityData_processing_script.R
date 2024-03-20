library(stringr)
library(tidyverse)
library(dplyr)
library(openxlsx)
library(longitudinalData)
library(RDP)
library(ggpubr)
library(ggplot2)


# Made blongitudinalData# Made by Alana Muller with a lot of help from ChatGPT

rm(list = ls())

# City 1 - Store Coordinates
#-----------------------------
# Store 1 - (X,Z): -249.37, 279.16
# Store 2 - (X,Z): 207.3, 99.9
# Store 3 - (X,Z): 145.79, -231.68
# Store 4 - (X,Z): -130.43, -112.92

#points(-249.37, 279.16)
#points(207.3, 99.9)
#points(145.79, -231.68)
#points(-130.43, -112.92)

# City 2 - Store Coordinates
#-----------------------------
# Store 1 - (X,Z): -95.22, 107.39
# Store 2 - (X,Z): 111.44, 246.8
# Store 3 - (X,Z): 169.62, -79.86
# Store 4 - (X,Z): -213.14,	-234.83

# City 3 - Store Coordinates
#-----------------------------
# Store 1 - (X,Z): -152.56, 162.41
# Store 2 - (X,Z): 209.17, 221.62
# Store 3 - (X,Z): 266.59, -234.67
# Store 4 - (X,Z): -249.76, -115.34


##### Read in file with subject, condition, and city data
setwd("E:/Nav Stress Data/") # set working directory

subj_cond_city_data <- read.xlsx("subj_cond_city.xlsx") # read in file

##### Change this to run next subject

# for my presentation, 1-13 except 4

subject_num <- "13"
subject_city <- "city3" # options are "city1", "city2", and "city3"

# Set working directory
# setwd("C:/Users/amuller/Desktop/Alana/UA/HSCL/Stress Shortcuts/stress-shortcuts-collab/data/tmp")
setwd(paste("E:/Nav Stress Data/Participant_data/", subject_num, sep = ""))
# setwd("C:/Users/almul/OneDrive/Desktop/Alana/UA/HSCL/Stress Shortcuts")

 
# Load the data
input_file <- paste(subject_num, "_", subject_city, ".log", sep = "")
input_data <- paste(readLines(input_file), collapse="\n")
text <- input_data

# set working directory to save pics - make sure a new folder is created already for the subject's pics
folder_name <- paste("E:/Nav Stress Data/Participant_data/", subject_num, "/pics", sep = "")

setwd(folder_name)

###################### Functions ######################

# function to calculate total distance of the path - aka totDist
totDist <- function(x,y) {
  sum(sqrt(diff(x)^2 + diff(y)^2))
}

##################################### EXTRACT OUTER PATH: PASSIVE AND ACTIVE LEARNING #####################################

############# Extract all lines between TASK_START TASK_EncodeOuterPaths and TASK_END TASK_EncodeOuterPaths

matches <- str_extract_all(text, "(?s)TASK_START\\s+TASK_EncodeOuterPaths\\s+(.*?)\\s+TASK_END\\s+TASK_EncodeOuterPaths") # finds the data between the start and end point
outer_df <- data.frame(matches, stringsAsFactors = FALSE) # puts each instance into a dataframe - there should be 2 or 4 observations
colnames(outer_df)[1] <- "learn_outer_paths"

############# Extract all lines between TASK_START LearnSmoothPassive and TASK_END LearnSmoothPassive
outer_passive_df <- data.frame() # create the dataframe we will put the data into

for (i in 1:nrow(outer_df)) {
  matches <- str_extract_all(outer_df[i,1], "(?s)TASK_START\\s+LearnSmoothPassive\\s+(.*?)\\s+TASK_END\\s+LearnSmoothPassive")
  outer_passive_df[i,1] <- data.frame(matches, stringsAsFactors = FALSE)
}
colnames(outer_passive_df)[1] <- "outer_passive_task" # name the column

outer_passive_df_list <- lapply(seq_len(nrow(outer_passive_df)), function(i) data.frame(value = outer_passive_df[i, ]))


# Loop through each of the dataframes in the list to do the stuff below

for (i in seq_along(outer_passive_df_list)) {
  
  # Get the dataframe from the list
  data_df <- outer_passive_df_list[[i]]
  
  # Convert the data to a tibble
  data_df <- tibble(data_df = str_split(data_df, "\n")[[1]])
  
  # Use regular expressions to extract the number before "Avatar:"
  data_df <- data_df %>%
    mutate(avatar_number = str_extract(data_df, "\\d+(?=\\tAvatar:)"))
  
  # Use regular expressions to extract the three numbers after "Position (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Position \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
  # Split the numbers column into three separate columns
  data_df <- data_df %>%
    separate(numbers, into = c("pos_X", "pos_Y", "pos_Z"), sep = "\t")
  
  # Use regular expressions to extract the three numbers after "Rotation (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Rotation \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
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
    mutate(time_ms = avatar_number - avatar_initial_number,
           time_sec = time_ms/1000)
  
  # Add a column to mark the condition and trial name and number (e.g. outer_passive1)
  data_df$trialname <- paste0("outer_passive", i)
  
  # Update the dataframe in the list
  outer_passive_df_list[[i]] <- data_df
  
}

# make and save a graph
#p <- ggplot(outer_passive_df_list[[1]], aes(x = pos_X, y = pos_Z, color = time_sec)) +
#  geom_point() +
#  scale_color_gradient(low = "lightblue", high = "darkblue") +
#  labs(x = "X", y = "Y", color = "Time (s)", title = "Outer Path Passive Learning 1") +
#  theme(plot.title = element_text(hjust = 0.5, size = 16), 
#        axis.title = element_text(size = 13), axis.text = element_text(size = 12), 
#        legend.title = element_text(size = 13), legend.text = element_text(size = 12)) +
#  geom_point(aes(x = -249.37, y = 279.16), size = 5, color = "red") +
#  geom_point(aes(x = 207.3, y = 99.9), size = 5, color = "red") +
#  geom_point(aes(x = 145.79, y = -231.68), size = 5, color = "red") +
#  geom_point(aes(x = -130.43, y = -112.92), size = 5, color = "red") +
#  geom_text(aes(x = -280, y = 300, label = "Store 1"), size = 4, color = "black") +
#  geom_text(aes(x = 255, y = 110, label = "Store 2"), size = 4, color = "black") +
#  geom_text(aes(x = 200, y = -235, label = "Store 3"), size = 4, color = "black") +
#  geom_text(aes(x = -160, y = -135, label = "Store 4"), size = 4, color = "black")

#jpeg("outer_passive1.jpeg", width = 7, height = 6, units = 'in', res = 500)
#p
#dev.off()

############# Extract all lines between TASK_START LearnActivePath and TASK_END LearnActivePath
outer_active_df <- data.frame() # create the dataframe we will put the data into

for (i in 1:nrow(outer_df)) {
  matches <- str_extract_all(outer_df[i,1], "(?s)TASK_START\\s+LearnActivePath\\s+(.*?)\\s+TASK_END\\s+LearnActivePath")
  outer_active_df[i,1] <- data.frame(matches, stringsAsFactors = FALSE)
}
colnames(outer_active_df)[1] <- "outer_active_task"

outer_active_df_list <- lapply(seq_len(nrow(outer_active_df)), function(i) data.frame(value = outer_active_df[i, ]))

# Loop through each of the dataframes in the list to do the stuff below

for (i in seq_along(outer_active_df_list)) {
  
  # Get the dataframe from the list
  data_df <- outer_active_df_list[[i]]
  
  # Convert the data to a tibble
  data_df <- tibble(data_df = str_split(data_df, "\n")[[1]])
  
  # Use regular expressions to extract the number before "Avatar:"
  data_df <- data_df %>%
    mutate(avatar_number = str_extract(data_df, "\\d+(?=\\tAvatar:)"))
  
  # Use regular expressions to extract the three numbers after "Position (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Position \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
  # Split the numbers column into three separate columns
  data_df <- data_df %>%
    separate(numbers, into = c("pos_X", "pos_Y", "pos_Z"), sep = "\t")
  
  # Use regular expressions to extract the three numbers after "Rotation (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Rotation \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
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
    mutate(time_ms = avatar_number - avatar_initial_number,
           time_sec = time_ms/1000)
  
  # Add a column to mark the condition and trial name and number (e.g. outer_passive1)
  data_df$trialname <- paste0("outer_active", i)
  
  # Update the dataframe in the list
  outer_active_df_list[[i]] <- data_df
  
}

# make and save graph of last outer active
#plot_name <- paste("outer_active", length(outer_active_df_list), ".jpg",sep = "")
#p <- ggplot(outer_active_df_list[[length(outer_active_df_list)]], aes(x = pos_X, y = pos_Z, color = time_sec)) +
#  geom_point() +
#  scale_color_gradient(low = "lightblue", high = "darkblue") +
#  labs(x = "X", y = "Y", color = "Time (s)", title = "Outer Path Active Learning 4") +
#  theme(plot.title = element_text(hjust = 0.5, size = 16), 
#        axis.title = element_text(size = 13), axis.text = element_text(size = 12), 
#        legend.title = element_text(size = 13), legend.text = element_text(size = 12)) +
#  geom_point(aes(x = -249.37, y = 279.16), size = 5, color = "red") +
#  geom_point(aes(x = 207.3, y = 99.9), size = 5, color = "red") +
#  geom_point(aes(x = 145.79, y = -231.68), size = 5, color = "red") +
#  geom_point(aes(x = -130.43, y = -112.92), size = 5, color = "red") +
#  geom_text(aes(x = -280, y = 300, label = "Store 1"), size = 4, color = "black") +
#  geom_text(aes(x = 255, y = 110, label = "Store 2"), size = 4, color = "black") +
#  geom_text(aes(x = 200, y = -235, label = "Store 3"), size = 4, color = "black") +
#  geom_text(aes(x = -160, y = -135, label = "Store 4"), size = 4, color = "black")

#jpeg(plot_name, width = 7, height = 6, units = 'in', res = 500)
#p
#dev.off()

# Use last trial as the actual whole path length
outer_actual_dist <- totDist(outer_active_df_list[[length(outer_active_df_list)]]$pos_X, outer_active_df_list[[length(outer_active_df_list)]]$pos_Z)


##################################### EXTRACT Inner PATH: PASSIVE AND ACTIVE LEARNING #####################################

############# Extract all lines between TASK_START TASK_EncodeInnerPaths and TASK_END TASK_EncodeInnerPaths

matches <- str_extract_all(text, "(?s)TASK_START\\s+TASK_EncodeInnerPaths\\s+(.*?)\\s+TASK_END\\s+TASK_EncodeInnerPaths") # finds the data between the start and end point
inner_df <- data.frame(matches, stringsAsFactors = FALSE) # puts each instance into a dataframe - there should be 2 or 4 observations
colnames(inner_df)[1] <- "learn_Inner_paths"

############# Extract all lines between TASK_START LearnSmoothPassive and TASK_END LearnSmoothPassive
inner_passive_df <- data.frame() # create the dataframe we will put the data into

for (i in 1:nrow(inner_df)) {
  matches <- str_extract_all(inner_df[i,1], "(?s)TASK_START\\s+LearnSmoothPassive\\s+(.*?)\\s+TASK_END\\s+LearnSmoothPassive")
  inner_passive_df[i,1] <- data.frame(matches, stringsAsFactors = FALSE)
}
colnames(inner_passive_df)[1] <- "inner_passive_task" # name the column

inner_passive_df_list <- lapply(seq_len(nrow(inner_passive_df)), function(i) data.frame(value = inner_passive_df[i, ]))


# Loop through each of the dataframes in the list to do the stuff below

for (i in seq_along(inner_passive_df_list)) {
  
  # Get the dataframe from the list
  data_df <- inner_passive_df_list[[i]]
  
  # Convert the data to a tibble
  data_df <- tibble(data_df = str_split(data_df, "\n")[[1]])
  
  # Use regular expressions to extract the number before "Avatar:"
  data_df <- data_df %>%
    mutate(avatar_number = str_extract(data_df, "\\d+(?=\\tAvatar:)"))
  
  # Use regular expressions to extract the three numbers after "Position (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Position \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
  # Split the numbers column into three separate columns
  data_df <- data_df %>%
    separate(numbers, into = c("pos_X", "pos_Y", "pos_Z"), sep = "\t")
  
  # Use regular expressions to extract the three numbers after "Rotation (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Rotation \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
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
    mutate(time_ms = avatar_number - avatar_initial_number,
           time_sec = time_ms/1000)
  
  # Add a column to mark the condition and trial name and number (e.g. inner_passive1)
  data_df$trialname <- paste0("inner_passive", i)
  
  # Update the dataframe in the list
  inner_passive_df_list[[i]] <- data_df
  
}

# make and save a graph
#p <- ggplot(inner_passive_df_list[[1]], aes(x = pos_X, y = pos_Z, color = time_sec)) +
#  geom_point() +
#  scale_color_gradient(low = "lightblue", high = "darkblue") +
#  labs(x = "X", y = "Y", color = "Time (s)", title = "Inner Path Passive Learning 1") +
#  theme(plot.title = element_text(hjust = 0.5, size = 16), 
#        axis.title = element_text(size = 13), axis.text = element_text(size = 12), 
#        legend.title = element_text(size = 13), legend.text = element_text(size = 12)) +
#  geom_point(aes(x = -249.37, y = 279.16), size = 5, color = "red") +
#  geom_point(aes(x = 207.3, y = 99.9), size = 5, color = "red") +
#  geom_point(aes(x = 145.79, y = -231.68), size = 5, color = "red") +
#  geom_point(aes(x = -130.43, y = -112.92), size = 5, color = "red") +
#  geom_text(aes(x = -280, y = 300, label = "Store 1"), size = 4, color = "black") +
#  geom_text(aes(x = 255, y = 110, label = "Store 2"), size = 4, color = "black") +
#  geom_text(aes(x = 200, y = -235, label = "Store 3"), size = 4, color = "black") +
#  geom_text(aes(x = -160, y = -135, label = "Store 4"), size = 4, color = "black")

#jpeg("inner_passive1.jpeg", width = 7, height = 6, units = 'in', res = 500)
#p
#dev.off()

############# Extract all lines between TASK_START LearnActivePath and TASK_END LearnActivePath
inner_active_df <- data.frame() # create the dataframe we will put the data into

for (i in 1:nrow(inner_df)) {
  matches <- str_extract_all(inner_df[i,1], "(?s)TASK_START\\s+LearnActivePath\\s+(.*?)\\s+TASK_END\\s+LearnActivePath")
  inner_active_df[i,1] <- data.frame(matches, stringsAsFactors = FALSE)
}
colnames(inner_active_df)[1] <- "inner_active_task"

inner_active_df_list <- lapply(seq_len(nrow(inner_active_df)), function(i) data.frame(value = inner_active_df[i, ]))

# Loop through each of the dataframes in the list to do the stuff below

for (i in seq_along(inner_active_df_list)) {
  
  # Get the dataframe from the list
  data_df <- inner_active_df_list[[i]]
  
  # Convert the data to a tibble
  data_df <- tibble(data_df = str_split(data_df, "\n")[[1]])
  
  # Use regular expressions to extract the number before "Avatar:"
  data_df <- data_df %>%
    mutate(avatar_number = str_extract(data_df, "\\d+(?=\\tAvatar:)"))
  
  # Use regular expressions to extract the three numbers after "Position (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Position \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
  # Split the numbers column into three separate columns
  data_df <- data_df %>%
    separate(numbers, into = c("pos_X", "pos_Y", "pos_Z"), sep = "\t")
  
  # Use regular expressions to extract the three numbers after "Rotation (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Rotation \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
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
    mutate(time_ms = avatar_number - avatar_initial_number,
           time_sec = time_ms/1000)
  
  # Add a column to mark the condition and trial name and number (e.g. inner_passive1)
  data_df$trialname <- paste0("inner_active", i)
  
  # Update the dataframe in the list
  inner_active_df_list[[i]] <- data_df
  
}

# make and save graph of last inner active
#plot_name <- paste("inner_active", length(inner_active_df_list), ".jpg",sep = "")
#p <- ggplot(inner_active_df_list[[length(inner_active_df_list)]], aes(x = pos_X, y = pos_Z, color = time_sec)) +
#  geom_point() +
#  scale_color_gradient(low = "lightblue", high = "darkblue") +
#  labs(x = "X", y = "Y", color = "Time (s)", title = "Inner Path Active Learning 4") +
#  theme(plot.title = element_text(hjust = 0.5, size = 16), 
#        axis.title = element_text(size = 13), axis.text = element_text(size = 12), 
#        legend.title = element_text(size = 13), legend.text = element_text(size = 12)) +
#  geom_point(aes(x = -249.37, y = 279.16), size = 5, color = "red") +
#  geom_point(aes(x = 207.3, y = 99.9), size = 5, color = "red") +
#  geom_point(aes(x = 145.79, y = -231.68), size = 5, color = "red") +
#  geom_point(aes(x = -130.43, y = -112.92), size = 5, color = "red") +
#  geom_text(aes(x = -280, y = 300, label = "Store 1"), size = 4, color = "black") +
#  geom_text(aes(x = 255, y = 110, label = "Store 2"), size = 4, color = "black") +
#  geom_text(aes(x = 200, y = -235, label = "Store 3"), size = 4, color = "black") +
#  geom_text(aes(x = -160, y = -135, label = "Store 4"), size = 4, color = "black")

#jpeg(plot_name, width = 7, height = 6, units = 'in', res = 500)
#p
#dev.off()

# Use last trial as the actual whole path length
inner_actual_dist <- totDist(inner_active_df_list[[length(inner_active_df_list)]]$pos_X, inner_active_df_list[[length(inner_active_df_list)]]$pos_Z)


##################################### EXTRACT PATH RECREATION DATA #####################################

############# Extract all lines between TASK_START TASK_ExplorationTask and TASK_END TASK_ExplorationTask

matches <- str_extract_all(text, "(?s)TASK_START\\s+TASK_ExplorationTask\\s+(.*?)\\s+TASK_END\\s+TASK_ExplorationTask") # finds the data between the start and end point
recreatePath_df <- data.frame(matches, stringsAsFactors = FALSE) # puts each instance into a dataframe - there should be 2 or 4 observations
colnames(recreatePath_df)[1] <- "recreate_paths"

recreatePath_df_list <- lapply(seq_len(nrow(recreatePath_df)), function(i) data.frame(value = recreatePath_df[i, ]))

# Loop through each of the dataframes in the list to do the stuff below

for (i in seq_along(recreatePath_df_list)) {
  
  # Get the dataframe from the list
  data_df <- recreatePath_df_list[[i]]
  
  # Convert the data to a tibble
  data_df <- tibble(data_df = str_split(data_df, "\n")[[1]])
  
  # Use regular expressions to extract the number before "Avatar:"
  data_df <- data_df %>%
    mutate(avatar_number = str_extract(data_df, "\\d+(?=\\tAvatar:)"))
  
  # Use regular expressions to extract the three numbers after "Position (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Position \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
  # Split the numbers column into three separate columns
  data_df <- data_df %>%
    separate(numbers, into = c("pos_X", "pos_Y", "pos_Z"), sep = "\t")
  
  # Use regular expressions to extract the three numbers after "Rotation (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Rotation \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
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
    mutate(time_ms = avatar_number - avatar_initial_number,
           time_sec = time_ms/1000)
  
  # Add a column to mark the condition and trial name and number (e.g. inner_passive1)
  data_df$trialname <- paste0("recreatePath", i)
  
  # Update the dataframe in the list
  recreatePath_df_list[[i]] <- data_df
  
}

# Loop through each path recreation and generate a plot for each one
# make and save a graph

#for (i in seq_along(recreatePath_df_list)) {
  # Create the plot title name
#  plot_title <- paste("Path Recreation " , i, sep = "")
  # Create plot
#  gg <- ggplot(recreatePath_df_list[[i]], aes(x = pos_X, y = pos_Z, color = time_sec)) +
#   geom_point() +
#   scale_color_gradient(low = "lightblue", high = "darkblue") +
#   labs(x = "X", y = "Y", color = "Time (s)", title = plot_title) +
#   theme(plot.title = element_text(hjust = 0.5, size = 16), 
#         axis.title = element_text(size = 13), axis.text = element_text(size = 12), 
#         legend.title = element_text(size = 13), legend.text = element_text(size = 12)) +
#   geom_point(aes(x = -249.37, y = 279.16), size = 5, color = "red") +
#   geom_point(aes(x = 207.3, y = 99.9), size = 5, color = "red") +
#   geom_point(aes(x = 145.79, y = -231.68), size = 5, color = "red") +
#   geom_point(aes(x = -130.43, y = -112.92), size = 5, color = "red") +
#   geom_text(aes(x = -280, y = 300, label = "Store 1"), size = 4, color = "black") +
#   geom_text(aes(x = 255, y = 110, label = "Store 2"), size = 4, color = "black") +
#   geom_text(aes(x = 200, y = -235, label = "Store 3"), size = 4, color = "black") +
#   geom_text(aes(x = -160, y = -135, label = "Store 4"), size = 4, color = "black")
  
  # Save the plot
# ggsave(paste0("path_recreate", i, ".jpg"), gg, width = 6.5, height = 5.5, units = 'in', dpi = 500)
#}

##################################### RETRIEVE: NAVIGATION TASK #####################################

############# Extract all lines between TASK_START NavigationTrials and TASK_END NavigationTrials

matches <- str_extract_all(text, "(?s)TASK_START\\s+NavigationTrials\\s+(.*?)\\s+TASK_END\\s+NavigationTrials") # finds the data between the start and end point
navTestBlocks_df <- data.frame(matches, stringsAsFactors = FALSE) # creates a dataframe with a row being each chunk of data 
colnames(navTestBlocks_df)[1] <- "navTestBlocks" # renames the first column

############# Extract all lines between TASK_START	Navigate and TASK_END	Navigate

matches <- str_extract_all(text, "(?s)TASK_START\\s+Navigate\\s+(.*?)\\s+TASK_END\\s+Navigate") # finds the data between the start and end point
navTestTrials_df <- data.frame(matches, stringsAsFactors = FALSE) # creates a dataframe with a row being each chunk of data 
colnames(navTestTrials_df)[1] <- "navTestTrials" # renames the first column

navTestTrials_df_list <- lapply(seq_len(nrow(navTestTrials_df)), function(i) data.frame(value = navTestTrials_df[i, ]))

# Loop through each of the dataframes in the list to do the stuff below

for (i in seq_along(navTestTrials_df_list)) {
  
  # Get the dataframe from the list
  data_df <- navTestTrials_df_list[[i]]
  
  # Convert the data to a tibble
  data_df <- tibble(data_df = str_split(data_df, "\n")[[1]])
  
  # Use regular expressions to extract the number before "Avatar:"
  data_df <- data_df %>%
    mutate(avatar_number = str_extract(data_df, "\\d+(?=\\tAvatar:)"))
  
  # Use regular expressions to extract the three numbers after "Position (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Position \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
  # Split the numbers column into three separate columns
  data_df <- data_df %>%
    separate(numbers, into = c("pos_X", "pos_Y", "pos_Z"), sep = "\t")
  
  # Use regular expressions to extract the three numbers after "Rotation (xyz):"
  data_df <- data_df %>%
    mutate(numbers = str_extract_all(data_df, "(?<=Rotation \\(xyz\\): \\s)[0-9.eE+-]+\\s+[0-9.eE+-]+\\s+[0-9.eE+-]+"))
  
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
    mutate(time_ms = avatar_number - avatar_initial_number,
           time_sec = time_ms/1000)
  
  # Add a column to mark the condition and trial name and number (e.g. outer_passive1)
  data_df$trialname <- paste0("navTest_trials", i)
  
  # Update the dataframe in the list
  navTestTrials_df_list[[i]] <- data_df
  
}

# make one big dataframe with all nav trials' x z values to make a graph
#navTest_all_dfs <- do.call(rbind, navTestTrials_df_list)

#all_nav_plot <- ggplot(navTest_all_dfs, aes(x = pos_X, y = pos_Z, color = time_sec)) +
# geom_point() +
# scale_color_gradient(low = "lightblue", high = "darkblue") +
# labs(x = "X", y = "Y", color = "Time (s)", title = "All Navigation Test Trials") +
# theme(plot.title = element_text(hjust = 0.5, size = 20), 
#       axis.title = element_text(size = 13), axis.text = element_text(size = 12), 
#       legend.title = element_text(size = 13), legend.text = element_text(size = 12)) +
# coord_cartesian(ylim = c(-350,350), xlim = c(-350,350)) +
# scale_y_continuous(breaks = seq(-400,400,100)) +
# scale_x_continuous(breaks = seq(-400,400,100)) +
# geom_point(aes(x = -249.37, y = 279.16), size = 5, color = "red") +
# geom_point(aes(x = 207.3, y = 99.9), size = 5, color = "red") +
# geom_point(aes(x = 145.79, y = -231.68), size = 5, color = "red") +
# geom_point(aes(x = -130.43, y = -112.92), size = 5, color = "red") +
# geom_text(aes(x = -300, y = 350, label = "Store 1"), size = 7, color = "black") +
# geom_text(aes(x = 280, y = 140, label = "Store 2"), size = 7, color = "black") +
# geom_text(aes(x = 220, y = -275, label = "Store 3"), size = 7, color = "black") +
# geom_text(aes(x = -230, y = -130, label = "Store 4"), size = 7, color = "black")

#ggsave("all_navTest_trials.jpeg", all_nav_plot, width = 6.5, height = 5.5, units = 'in', dpi = 500)


############# Make another dataframe pulling the numbers that Mike generated in the avatar log (has optimal path)
############# This data frame combines with the overlapping segment code at the bottom

# Get the dataframe from the list
log_data <- input_data

# Convert the data to a tibble
log_data <- tibble(log_data = str_split(log_data, "\n")[[1]])

# Extract the line after "task block trial"
log_data <- log_data %>%
  mutate(line = str_extract(log_data, "^TASK_NavigationTest.*$"))

# Split the line into separate columns
log_data <- log_data %>%
  separate(line, into = c("task", "block", "trial", "catchTrial", "Navigate_target", "Navigate_actualPath", "Navigate_optimalPath", "Navigate_excessPath", "Navigate_duration"), sep = "\\t+")

# Remove the rows with NAs
log_data <- na.omit(log_data)

# Convert the columns to numeric
log_data <- log_data %>%
  mutate(across(7:10, as.numeric))

log_data <- log_data[, -1]

# Add a column for subject ID
log_data$subjectID <- subject_num

# Make the subjectID column the first column
log_data <- log_data[c(ncol(log_data), 1:ncol(log_data)-1)]

############### Add store numbers to log_data

# Make a duplicate column of Navigate_target so we can replace the names with store numbers
log_data$target_store_num <- log_data$Navigate_target

# Get store names for Targets 01-04
pattern <- "Target01:\\s*(.*?)\\s*Position:" # Find the words between "Target01:" and "Position:"
hits <- regmatches(text, gregexpr(pattern, text, perl = TRUE)) # Extract the words between "Target01:" and "Position:"
combined_matches <- paste(unlist(hits), collapse = " ") # Combine all matches into a single entry
extracted_text <- gsub("Target01:\\s*|\\s*Position:", "", combined_matches) # Remove "Target01:" and "Position:" from the combined matches
Target01 <- extracted_text
replaceTarget01 <- "Store1"

pattern <- "Target02:\\s*(.*?)\\s*Position:" # Find the words between "Target02:" and "Position:"
hits <- regmatches(text, gregexpr(pattern, text, perl = TRUE)) # Extract the words between "Target02:" and "Position:"
combined_matches <- paste(unlist(hits), collapse = " ") # Combine all matches into a single entry
extracted_text <- gsub("Target02:\\s*|\\s*Position:", "", combined_matches) # Remove "Target02:" and "Position:" from the combined matches
Target02 <- extracted_text
replaceTarget02 <- "Store2"

pattern <- "Target03:\\s*(.*?)\\s*Position:" # Find the words between "Target03:" and "Position:"
hits <- regmatches(text, gregexpr(pattern, text, perl = TRUE)) # Extract the words between "Target03:" and "Position:"
combined_matches <- paste(unlist(hits), collapse = " ") # Combine all matches into a single entry
extracted_text <- gsub("Target03:\\s*|\\s*Position:", "", combined_matches) # Remove "Target03:" and "Position:" from the combined matches
Target03 <- extracted_text
replaceTarget03 <- "Store3"

pattern <- "Target04:\\s*(.*?)\\s*Position:" # Find the words between "Target04:" and "Position:"
hits <- regmatches(text, gregexpr(pattern, text, perl = TRUE)) # Extract the words between "Target04:" and "Position:"
combined_matches <- paste(unlist(hits), collapse = " ") # Combine all matches into a single entry
extracted_text <- gsub("Target04:\\s*|\\s*Position:", "", combined_matches) # Remove "Target04:" and "Position:" from the combined matches
Target04 <- extracted_text
replaceTarget04 <- "Store4"

# Replace Target number with Store number
log_data <- log_data %>%
  mutate(target_store_num = if_else(target_store_num == Target01, replaceTarget01, target_store_num))

log_data <- log_data %>%
  mutate(target_store_num = if_else(target_store_num == Target02, replaceTarget02, target_store_num))

log_data <- log_data %>%
  mutate(target_store_num = if_else(target_store_num == Target03, replaceTarget03, target_store_num))

log_data <- log_data %>%
  mutate(target_store_num = if_else(target_store_num == Target04, replaceTarget04, target_store_num))

# Add column to indicate the starting and ending stores so categorize trials
log_data$startEnd_store <- paste(lag(log_data$target_store_num), log_data$target_store_num, sep = " ")

# Fix the first entry in the startEnd_store column
log_data$startEnd_store[1] <- paste(log_data$target_store_num[12], log_data$target_store_num[1], sep = " ")

##### Add a column to the log_data sheet indicating the condition
# Get the condition name from the subj_cond_city_data
condition_name <- subj_cond_city_data$condition[subj_cond_city_data$subjectID == as.numeric(subject_num) & subj_cond_city_data$cityNum == subject_city]

log_data$condition <- condition_name # add the column to log_data

log_data$city <- subject_city

# Save log_data to csv file
write.csv(log_data, paste("E:/Nav Stress Data/Participant_data/navTrialsLogData", subject_num, "_", subject_city, ".csv", sep = ""), row.names = FALSE)

####################### Make another dataframe with learning trials: total path length, excess path length, and duration values
# But this total path distance is comparing the learned path to the traveled path

# Make an empty dataframe to put the path distance values into
learning_path_df <- data.frame(trialname = character(), total_path_distance = numeric(), excess_path_distance = numeric(), path_duration = numeric(), stringsAsFactors = FALSE)

########## OUTER PASSIVE ##########
for (i in 1:length(outer_passive_df_list)) {
  
  # calculate the total path distance
  path_dist <- totDist(outer_passive_df_list[[i]]$pos_X, outer_passive_df_list[[i]]$pos_Z)
  
  # no excess path distance to calculate here
  
  # grab the last value of the time in seconds for path duration
  path_dur <- max(outer_passive_df_list[[i]]$time_sec)
  
  # put all the info together, including trial name
  learning_path_df <- rbind(learning_path_df, data.frame(trialname = outer_passive_df_list[[i]][1, 11]$trialname, total_path_distance = path_dist, excess_path_distance = "N/A", path_duration = path_dur))
  
}

########## OUTER ACTIVE ##########
for (i in 1:length(outer_active_df_list)) {
  
  # calculate the total path distance
  path_dist <- totDist(outer_active_df_list[[i]]$pos_X, outer_active_df_list[[i]]$pos_Z)
  
  # calculate excess path distance
  excess_path = path_dist - outer_actual_dist
  
  # grab the last value of the time in seconds for path duration
  path_dur <- max(outer_active_df_list[[i]]$time_sec)
  
  # put all the info together, including trial name
  learning_path_df <- rbind(learning_path_df, data.frame(trialname = outer_active_df_list[[i]][1, 11]$trialname, total_path_distance = path_dist, excess_path_distance = excess_path, path_duration = path_dur))
  
}

########## INNER PASSIVE ##########
for (i in 1:length(inner_passive_df_list)) {
  
  # calculate the total path distance
  path_dist <- totDist(inner_passive_df_list[[i]]$pos_X, inner_passive_df_list[[i]]$pos_Z)
  
  # no excess path distance to calculate here

  # grab the last value of the time in seconds for path duration
  path_dur <- max(inner_passive_df_list[[i]]$time_sec)
  
  # put all the info together, including trial name
  learning_path_df <- rbind(learning_path_df, data.frame(trialname = inner_passive_df_list[[i]][1, 11]$trialname, total_path_distance = path_dist, excess_path_distance = "N/A", path_duration = path_dur))
  
}

########## INNER ACTIVE ##########
for (i in 1:length(inner_active_df_list)) {
  
  # calculate the total path distance
  path_dist <- totDist(inner_active_df_list[[i]]$pos_X, inner_active_df_list[[i]]$pos_Z)
  
  # calculate excess path distance
  excess_path = path_dist - inner_actual_dist
  
  # grab the last value of the time in seconds for path duration
  path_dur <- max(inner_active_df_list[[i]]$time_sec)
  
  # put all the info together, including trial name
  learning_path_df <- rbind(learning_path_df, data.frame(trialname = inner_active_df_list[[i]][1, 11]$trialname, total_path_distance = path_dist, excess_path_distance = excess_path, path_duration = path_dur))
  
}

# Save learning_path_df to csv file
write.csv(learning_path_df, paste("E:/Nav Stress Data/Participant_data/learningTrialsLogData_", subject_num, "_", subject_city, ".csv", sep = ""), row.names = FALSE)


####################### Make plots for each nav test trial #######################

# loop through a dataframe list to generate a plot for each trial
#for (i in seq_along(navTestTrials_df_list)) {
  # create the plot title name
  #plot_title <- paste("Navigation Test Trial ", i)
  # create the ggplot object for the current data frame
  #gg <- ggplot(navTestTrials_df_list[[i]], aes(x = pos_X, y = pos_Z, color = time_sec)) +
  #  geom_point() +
  #  scale_color_gradient(low = "lightblue", high = "darkblue") +
  #  labs(x = "X", y = "Y", color = "Time (s)", title = plot_title) +
  #  theme(plot.title = element_text(hjust = 0.5, size = 20), 
  #        axis.title = element_text(size = 13), axis.text = element_text(size = 12), 
  #        legend.title = element_text(size = 13), legend.text = element_text(size = 12)) +
  #  coord_cartesian(ylim = c(-350,350), xlim = c(-350,350)) +
  #  scale_y_continuous(breaks = seq(-400,400,100)) +
  #  scale_x_continuous(breaks = seq(-400,400,100)) +
  #  geom_point(aes(x = -249.37, y = 279.16), size = 5, color = "red") +
  #  geom_point(aes(x = 207.3, y = 99.9), size = 5, color = "red") +
  #  geom_point(aes(x = 145.79, y = -231.68), size = 5, color = "red") +
  #  geom_point(aes(x = -130.43, y = -112.92), size = 5, color = "red") +
  #  geom_text(aes(x = -300, y = 350, label = "Store 1"), size = 7, color = "black") +
  #  geom_text(aes(x = 280, y = 140, label = "Store 2"), size = 7, color = "black") +
  #  geom_text(aes(x = 220, y = -275, label = "Store 3"), size = 7, color = "black") +
  #  geom_text(aes(x = -230, y = -130, label = "Store 4"), size = 7, color = "black")
  
  # save the plot with a file name based on the index of the data frame
  #ggsave(paste0("navTest_trial", i, ".jpg"), gg, width = 6.5, height = 5.5, units = 'in', dpi = 500)
#}
