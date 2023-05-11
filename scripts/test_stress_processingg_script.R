library(stringr)
library(tidyverse)
library(dplyr)

# Made by Alana Muller with a lot of help from ChatGPT

# Store coordinates

# Store 2 - (X,Z): 207.3, 99.9
# Store 3 - (X,Z): 145.79, -231.68
# Store 4 - (X,Z): -130.43, -112.92
# Store 1 - (X,Z): -249.37, 279.16

#points(-249.37, 279.16)
#points(207.3, 99.9)
#points(145.79, -231.68)
#points(-130.43, -112.92)

# Set working directory
# setwd("C:/Users/amuller/Desktop/Alana/UA/HSCL/Stress Shortcuts/stress-shortcuts-collab/data/tmp")
# setwd("E:/Nav Stress Pilot Data") # for desktop
  setwd("C:/Users/almul/OneDrive/Desktop/Alana/UA/HSCL/Stress Shortcuts")

# Load the data
input_file <- "navStress_P001_city1_navigation_23-03-24D_10.18.47T.log"
# input_file <- "mini_test_log.txt"
# input_file <- "environment_corner_coordinates.log"
input_data <- paste(readLines(input_file), collapse="\n")
text <- input_data


###################### Functions ######################

# function to calculate total distance of the path - aka totDist
totDist <- function(x,y) {
  sum(sqrt(diff(x)^2 + diff(y)^2))
}

##################################### EXTRACT OUTER PATH: PASSIVE AND ACTIVE LEARNING #####################################

############# Extract all lines between TASK_START TASK_EncodeOuterPaths and TASK_END TASK_EncodeOuterPaths

matches <- str_extract_all(text, "(?s)TASK_START\\s+TASK_EncodeOuterPaths\\s+(.*?)\\s+TASK_END\\s+TASK_EncodeOuterPaths") # finds the data between the start and end point
outer_df <- data.frame(matches, stringsAsFactors = FALSE) # make one big dataframe for outer path
colnames(outer_df)[1] <- "learn_outer_paths"

############# Extract all lines between TASK_START LearnSmoothPassive SmoothPassivePathStart and TASK_END LearnSmoothPassive SmoothPassivePathStart

matches <- str_extract_all(outer_df[1], "(?s)TASK_START\\s+LearnSmoothPassive\\s+SmoothPassivePathStart.*?TASK_END\\s+LearnSmoothPassive\\s+SmoothPassivePathStart")
outer_passive_df <- data.frame(matches, stringsAsFactors = FALSE)
colnames(outer_passive_df)[1] <- "outer_passive_task"

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

x <- outer_passive_df_list[[1]]$pos_X
z <- outer_passive_df_list[[1]]$pos_Z
plot(x,z)

x <- outer_passive_df_list[[2]]$pos_X
z <- outer_passive_df_list[[2]]$pos_Z
plot(x,z)

x <- outer_passive_df_list[[3]]$pos_X
z <- outer_passive_df_list[[3]]$pos_Z
plot(x,z)

x <- outer_passive_df_list[[4]]$pos_X
z <- outer_passive_df_list[[4]]$pos_Z
plot(x,z)

############# Extract all lines between TASK_START LearnActivePath ActivePathStart and TASK_END LearnActivePath ActivePathStart

matches <- str_extract_all(outer_df[1], "(?s)TASK_START\\s+LearnActivePath\\s+ActivePathStart.*?TASK_END\\s+LearnActivePath\\s+ActivePathStart")
outer_active_df <- data.frame(matches, stringsAsFactors = FALSE)
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

x <- outer_active_df_list[[1]]$pos_X
z <- outer_active_df_list[[1]]$pos_Z
plot(x,z)

x <- outer_active_df_list[[2]]$pos_X
z <- outer_active_df_list[[2]]$pos_Z
plot(x,z)

x <- outer_active_df_list[[3]]$pos_X
z <- outer_active_df_list[[3]]$pos_Z
plot(x,z)

# this one will be the optimal whole outer route
x <- outer_active_df_list[[4]]$pos_X
z <- outer_active_df_list[[4]]$pos_Z
plot(x,z)

ggplot(outer_active_df_list[[4]], aes(x = pos_X, y = pos_Z, color = time_sec)) +
  geom_point() +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "X", y = "Y", color = "Time") +
  geom_point(aes(x = -249.37, y = 279.16), size = 3, color = "red") +
  geom_point(aes(x = 207.3, y = 99.9), size = 3, color = "red") +
  geom_point(aes(x = 145.79, y = -231.68), size = 3, color = "red") +
  geom_point(aes(x = -130.43, y = -112.92), size = 3, color = "red")

# Use this as the actual path length
outer_actual_length <- totDist(outer_active_df_list[[4]]$pos_X, outer_active_df_list[[4]]$pos_Z)


############# Extract all lines between TASK_START Navigate	NavigationTask and TASK_END Navigate NavigationTask	

matches <- str_extract_all(outer_df[1], "(?s)TASK_START\\s+Navigate\\s+NavigationTask.*?TASK_END\\s+Navigate\\s+NavigationTask")
outer_navInOrder_df <- data.frame(matches, stringsAsFactors = FALSE)
colnames(outer_navInOrder_df)[1] <- "outer_navInOrder_task"

outer_navInOrder_df_list <- lapply(seq_len(nrow(outer_navInOrder_df)), function(i) data.frame(value = outer_navInOrder_df[i, ]))

# Loop through each of the dataframes in the list to do the stuff below

for (i in seq_along(outer_navInOrder_df_list)) {
  
  # Get the dataframe from the list
  data_df <- outer_navInOrder_df_list[[i]]
  
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
  data_df$trialname <- paste0("outer_navInOrder", i)
  
  # Update the dataframe in the list
  outer_navInOrder_df_list[[i]] <- data_df
  
}

# make one big dataframe with all outer nav in order x z values
outer_navInOrder_all_dfs <- do.call(rbind, outer_navInOrder_df_list)

x <- outer_navInOrder_df_list[[1]]$pos_X
z <- outer_navInOrder_df_list[[1]]$pos_Z
plot(x,z)

x <- outer_navInOrder_df_list[[2]]$pos_X
z <- outer_navInOrder_df_list[[2]]$pos_Z
plot(x,z)

x <- outer_navInOrder_df_list[[3]]$pos_X
z <- outer_navInOrder_df_list[[3]]$pos_Z
plot(x,z)

x <- outer_navInOrder_df_list[[4]]$pos_X
z <- outer_navInOrder_df_list[[4]]$pos_Z
plot(x,z)

# this is the participant's whole traveled path (segments combined together)
x <- outer_navInOrder_all_dfs$pos_X
z <- outer_navInOrder_all_dfs$pos_Z
plot(x,z)


##################################### EXTRACT INNER PATH: PASSIVE AND ACTIVE LEARNING #####################################

############# Extract all lines between TASK_START TASK_EncodeInnerPaths and TASK_END TASK_EncodeInnerPaths

matches <- str_extract_all(text, "(?s)TASK_START\\s+TASK_EncodeInnerPaths\\s+(.*?)\\s+TASK_END\\s+TASK_EncodeInnerPaths") # finds the data between the start and end point
inner_df <- data.frame(matches, stringsAsFactors = FALSE) # make one big dataframe for the inner path
colnames(inner_df)[1] <- "learn_inner_paths"

############# Extract all lines between TASK_START LearnSmoothPassive SmoothPassivePathStart and TASK_END LearnSmoothPassive SmoothPassivePathStart

matches <- str_extract_all(inner_df[1], "(?s)TASK_START\\s+LearnSmoothPassive\\s+SmoothPassivePathStart.*?TASK_END\\s+LearnSmoothPassive\\s+SmoothPassivePathStart")
inner_passive_df <- data.frame(matches, stringsAsFactors = FALSE)
colnames(inner_passive_df)[1] <- "inner_passive_task"

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

x <- inner_passive_df_list[[1]]$pos_X
z <- inner_passive_df_list[[1]]$pos_Z
plot(x,z)

x <- inner_passive_df_list[[2]]$pos_X
z <- inner_passive_df_list[[2]]$pos_Z
plot(x,z)

############# Extract all lines between TASK_START LearnActivePath ActivePathStart and TASK_END LearnActivePath ActivePathStart

matches <- str_extract_all(inner_df[1], "(?s)TASK_START\\s+LearnActivePath\\s+ActivePathStart.*?TASK_END\\s+LearnActivePath\\s+ActivePathStart")
inner_active_df <- data.frame(matches, stringsAsFactors = FALSE)
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


x <- inner_active_df_list[[1]]$pos_X
z <- inner_active_df_list[[1]]$pos_Z
plot(x,z)

# this will be the inner optimal path
x <- inner_active_df_list[[2]]$pos_X
z <- inner_active_df_list[[2]]$pos_Z
plot(x,z)

# inner path actual path length
inner_actual_length <- totDist(inner_active_df_list[[2]]$pos_X, inner_active_df_list[[2]]$pos_Z)

# now I just need to split the paths into the four paths without adding too much error

############# Extract all lines between TASK_START Navigate	NavigationTask and TASK_END Navigate NavigationTask	

matches <- str_extract_all(inner_df[1], "(?s)TASK_START\\s+Navigate\\s+NavigationTask.*?TASK_END\\s+Navigate\\s+NavigationTask")
inner_navInOrder_df <- data.frame(matches, stringsAsFactors = FALSE)
colnames(inner_navInOrder_df)[1] <- "inner_navInOrder_task"

inner_navInOrder_df_list <- lapply(seq_len(nrow(inner_navInOrder_df)), function(i) data.frame(value = inner_navInOrder_df[i, ]))

# Loop through each of the dataframes in the list to do the stuff below

for (i in seq_along(inner_navInOrder_df_list)) {
  
  # Get the dataframe from the list
  data_df <- inner_navInOrder_df_list[[i]]
  
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
  data_df$trialname <- paste0("inner_navInOrder", i)
  
  # Update the dataframe in the list
  inner_navInOrder_df_list[[i]] <- data_df
  
}

# make one big dataframe with all inner nav in order x z values
inner_navInOrder_all_dfs <- do.call(rbind, inner_navInOrder_df_list)

x <- inner_navInOrder_df_list[[1]]$pos_X
z <- inner_navInOrder_df_list[[1]]$pos_Z
plot(x,z)

x <- inner_navInOrder_df_list[[2]]$pos_X
z <- inner_navInOrder_df_list[[2]]$pos_Z
plot(x,z)

x <- inner_navInOrder_df_list[[3]]$pos_X
z <- inner_navInOrder_df_list[[3]]$pos_Z
plot(x,z)

x <- inner_navInOrder_df_list[[4]]$pos_X
z <- inner_navInOrder_df_list[[4]]$pos_Z
plot(x,z)

# participant's whole path
x <- inner_navInOrder_all_dfs$pos_X
z <- inner_navInOrder_all_dfs$pos_Z
plot(x,z)


##################################### RETRIEVE: NAVIGATION TASK #####################################

############# Extract all lines between TASK_START TASK_NavigationTest and TASK_END TASK_NavigationTest

matches <- str_extract_all(text, "(?s)TASK_START\\s+TASK_NavigationTest\\s+(.*?)\\s+TASK_END\\s+TASK_NavigationTest") # finds the data between the start and end point
navTest_df <- data.frame(matches, stringsAsFactors = FALSE) # creates a dataframe with a row being each chunk of data 
colnames(navTest_df)[1] <- "navInOrder_task" # renames the first column

############# Extract all lines between TASK_START	Navigate	NavigationTask and TASK_END	Navigate	NavigationTask

matches <- str_extract_all(navTest_df[1], "(?s)TASK_START\\s+Navigate\\s+NavigationTask.*?TASK_END\\s+Navigate\\s+NavigationTask")
navTest_trials_df <- data.frame(matches, stringsAsFactors = FALSE)
colnames(navTest_trials_df)[1] <- "navTest_trials_task"

navTest_trials_df_list <- lapply(seq_len(nrow(navTest_trials_df)), function(i) data.frame(value = navTest_trials_df[i, ]))

# Loop through each of the dataframes in the list to do the stuff below

for (i in seq_along(navTest_trials_df_list)) {
  
  # Get the dataframe from the list
  data_df <- navTest_trials_df_list[[i]]
  
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
  navTest_trials_df_list[[i]] <- data_df
  
}

####################### Make another dataframe with path length and duration values #######################

# function to calculate total distance of the path - aka totDist
totDist <- function(x,y) {
  sum(sqrt(diff(x)^2 + diff(y)^2))
}

# Make an empty dataframe to put the path distance values into
path_dist_df <- data.frame(trialname = character(), path_distance = numeric(), path_duration = numeric(), stringsAsFactors = FALSE)

# Calculate distance of learned path
inner_learned_dist <- 


########## INNER PASSIVE ##########
for (i in 1:length(inner_passive_df_list)) {
  
  # calculate the total path distance
  path_dist <- totDist(inner_passive_df_list[[i]]$pos_X, inner_passive_df_list[[i]]$pos_Z)
  
  # grab the last value of the time in seconds for path duration
  path_dur <- max(inner_passive_df_list[[i]]$time_sec)
  
  # put all the info together, including trial name
  path_dist_df <- rbind(path_dist_df, data.frame(trialname = inner_passive_df_list[[i]][1, 11]$trialname, path_distance = path_dist, path_duration = path_dur))
  
}

########## INNER ACTIVE ##########
for (i in 1:length(inner_active_df_list)) {
  
  # calculate the total path distance
  path_dist <- totDist(inner_active_df_list[[i]]$pos_X, inner_active_df_list[[i]]$pos_Z)
  
  # grab the last value of the time in seconds for path duration
  path_dur <- max(inner_active_df_list[[i]]$time_sec)
  
  # put all the info together, including trial name
  path_dist_df <- rbind(path_dist_df, data.frame(trialname = inner_active_df_list[[i]][1, 11]$trialname, path_distance = path_dist, path_duration = path_dur))
  
}

########## INNER NAV IN ORDER WHOLE PATH ##########
path_dist <- totDist(inner_navInOrder_all_dfs$pos_X, inner_navInOrder_all_dfs$pos_Z)
path_dur <- sum(max(inner_navInOrder_df_list[[1]]$time_sec), max(inner_navInOrder_df_list[[2]]$time_sec),
                max(inner_navInOrder_df_list[[3]]$time_sec), max(inner_navInOrder_df_list[[4]]$time_sec))

path_dist_df <- rbind(path_dist_df, data.frame(trialname = "inner_navInOrder_all", path_distance = path_dist, path_duration = path_dur))

########## INNER NAV IN ORDER SEGMENTS ##########
for (i in 1:length(inner_navInOrder_df_list)) {
  
  # calculate the total path distance
  path_dist <- totDist(inner_navInOrder_df_list[[i]]$pos_X, inner_navInOrder_df_list[[i]]$pos_Z)
  
  # grab the last value of the time in seconds for path duration
  path_dur <- max(inner_navInOrder_df_list[[i]]$time_sec)
  
  # put all the info together, including trial name
  path_dist_df <- rbind(path_dist_df, data.frame(trialname = inner_navInOrder_df_list[[i]][1, 11]$trialname, path_distance = path_dist, path_duration = path_dur))
  
}

########## OUTER PASSIVE ##########
for (i in 1:length(outer_passive_df_list)) {
  
  # calculate the total path distance
  path_dist <- totDist(outer_passive_df_list[[i]]$pos_X, outer_passive_df_list[[i]]$pos_Z)
  
  # grab the last value of the time in seconds for path duration
  path_dur <- max(outer_passive_df_list[[i]]$time_sec)
  
  # put all the info together, including trial name
  path_dist_df <- rbind(path_dist_df, data.frame(trialname = outer_passive_df_list[[i]][1, 11]$trialname, path_distance = path_dist, path_duration = path_dur))
  
}

########## OUTER ACTIVE ##########
for (i in 1:length(outer_active_df_list)) {
  
  # calculate the total path distance
  path_dist <- totDist(outer_active_df_list[[i]]$pos_X, outer_active_df_list[[i]]$pos_Z)
  
  # grab the last value of the time in seconds for path duration
  path_dur <- max(outer_active_df_list[[i]]$time_sec)
  
  # put all the info together, including trial name
  path_dist_df <- rbind(path_dist_df, data.frame(trialname = outer_active_df_list[[i]][1, 11]$trialname, path_distance = path_dist, path_duration = path_dur))
  
}

########## OUTER NAV IN ORDER WHOLE PATH ##########
path_dist <- totDist(outer_navInOrder_all_dfs$pos_X, outer_navInOrder_all_dfs$pos_Z)
path_dur <- sum(max(outer_navInOrder_df_list[[1]]$time_sec), max(outer_navInOrder_df_list[[2]]$time_sec),
                max(outer_navInOrder_df_list[[3]]$time_sec), max(outer_navInOrder_df_list[[4]]$time_sec))

path_dist_df <- rbind(path_dist_df, data.frame(trialname = "outer_navInOrder_all", path_distance = path_dist, path_duration = path_dur))


########## OUTER NAV IN ORDER SEGMENTS ##########
for (i in 1:length(outer_navInOrder_df_list)) {
  
  # calculate the total path distance
  path_dist <- totDist(outer_navInOrder_df_list[[i]]$pos_X, outer_navInOrder_df_list[[i]]$pos_Z)
  
  # grab the last value of the time in seconds for path duration
  path_dur <- max(outer_navInOrder_df_list[[i]]$time_sec)
  
  # put all the info together, including trial name
  path_dist_df <- rbind(path_dist_df, data.frame(trialname = outer_navInOrder_df_list[[i]][1, 11]$trialname, path_distance = path_dist, path_duration = path_dur))
  
}

########## NAV TEST ##########
for (i in 1:length(navTest_trials_df_list)) {
  
  # calculate the total path distance
  path_dist <- totDist(navTest_trials_df_list[[i]]$pos_X, navTest_trials_df_list[[i]]$pos_Z)
  
  # grab the last value of the time in seconds for path duration
  path_dur <- max(navTest_trials_df_list[[i]]$time_sec)
  
  # put all the info together, including trial name
  path_dist_df <- rbind(path_dist_df, data.frame(trialname = navTest_trials_df_list[[i]][1, 11]$trialname, path_distance = path_dist, path_duration = path_dur))
  
}


##################### getting the closest points to separate the whole active path into four segments

# Define the points to search for
search_points <- data.frame(x = c(207.3, 145.79, -130.43, -249.37), y = c(99.9, -231.68, -112.92, 279.16))

# Find the closest point to each search point
closest_points <- lapply(1:nrow(search_points), function(i) {
  # Calculate distances to all points in data frame
  distances <- sqrt((outer_active_df_list[[4]]$pos_X - search_points[i, "x"])^2 + (outer_active_df_list[[4]]$pos_Z - search_points[i, "y"])^2)
  
  # Get index of minimum distance
  min_index <- which.min(distances)
  
  # Return x-y values and index of closest point
  return(data.frame(x = outer_active_df_list[[4]]$pos_X[min_index], 
                    y = outer_active_df_list[[4]]$pos_Z[min_index], 
                    index = min_index,
                    time = outer_active_df_list[[4]]$time_ms[min_index]))
})

# Combine the closest points into a single data frame
closest_points_df <- do.call(rbind, closest_points)

# Print the closest points
print(closest_points_df)


###############################################################################

######################## Experimental ChatGPT Stuff ###########################




# Create a new column in the data frame for the closest time value
outer_active_df_list[[4]]$closest_time <- closest_time$closest_time

# Split the data frame into four subsets based on the closest time values
df_list <- split(outer_active_df_list[[4]], outer_active_df_list[[4]]$closest_time)

# Print the first few rows of each subset
lapply(df_list, head)



