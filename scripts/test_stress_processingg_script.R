library(stringr)
library(tidyverse)
library(dplyr)

# Made by Alana Muller with a lot of help from ChatGPT

# Set working directory
# setwd("C:/Users/amuller/Desktop/Alana/UA/HSCL/Stress Shortcuts/stress-shortcuts-collab/data/tmp")
  setwd("E:/Nav Stress Pilot Data")

# Load the data
input_file <- "navStress_P001_city1_navigation_23-03-24D_10.18.47T.log"
# input_file <- "mini_test_log.txt"
input_data <- paste(readLines(input_file), collapse="\n")
text <- input_data


############# Extract all lines between TASK_START LearnSmoothPassive SmoothPassivePathStart and TASK_END LearnSmoothPassive SmoothPassivePathStart

matches <- str_extract_all(text, "(?s)TASK_START\\s+LearnSmoothPassive\\s+SmoothPassivePathStart.*?TASK_END\\s+LearnSmoothPassive\\s+SmoothPassivePathStart")
learn_passive_df <- data.frame(matches, stringsAsFactors = FALSE)
colnames(learn_passive_df)[1] <- "learn_passive_task"

learn_passive_df_list <- lapply(seq_len(nrow(learn_passive_df)), function(i) data.frame(value = learn_passive_df[i, ]))


# Loop through each of the dataframes in the list to do the stuff below

for (i in seq_along(learn_passive_df_list)) {
  
  # Get the dataframe from the list
  data_df <- learn_passive_df_list[[i]]
  
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
    mutate(avatar_time = avatar_number - avatar_initial_number)
  
  # Update the dataframe in the list
  learn_passive_df_list[[i]] <- data_df
  
}

x1 <- learn_passive_df_list[[1]]$pos_X
z1 <- learn_passive_df_list[[1]]$pos_Z
plot(x1,z1)

x2 <- learn_passive_df_list[[2]]$pos_X
z2 <- learn_passive_df_list[[2]]$pos_Z
plot(x2,z2)


############# Extract all lines between LearnActivePath ActivePathStart and TASK_END LearnActivePath ActivePathStart

matches <- str_extract_all(text, "(?s)TASK_START\\s+LearnActivePath\\s+ActivePathStart.*?TASK_END\\s+LearnActivePath\\s+ActivePathStart")
learn_active_df <- data.frame(matches, stringsAsFactors = FALSE)
colnames(learn_active_df)[1] <- "learn_active_task"

learn_active_df_list <- lapply(seq_len(nrow(learn_active_df)), function(i) data.frame(value = learn_active_df[i, ]))


# Loop through each of the dataframes in the list to do the stuff below

for (i in seq_along(learn_active_df_list)) {
  
  # Get the dataframe from the list
  data_df <- learn_active_df_list[[i]]
  
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
    mutate(avatar_time = avatar_number - avatar_initial_number)
  
  # Update the dataframe in the list
  learn_active_df_list[[i]] <- data_df
  
}

x1 <- learn_active_df_list[[1]]$pos_X
z1 <- learn_active_df_list[[1]]$pos_Z
plot(x1,z1)

x2 <- learn_active_df_list[[2]]$pos_X
z2 <- learn_active_df_list[[2]]$pos_Z
plot(x2,z2)


############# Extract all lines between TASK_START TASK_NavigateInOrder and TASK_END TASK_NavigateInOrder

matches <- str_extract_all(text, "(?s)TASK_START\\s+TASK_NavigateInOrder\\s+(.*?)\\s+TASK_END\\s+TASK_NavigateInOrder") # finds the data between the start and end point
navInOrder_df <- data.frame(matches, stringsAsFactors = FALSE) # creates a dataframe with a row being each chunck of data 
colnames(navInOrder_df)[1] <- "navInOrder_task" # renames the first column

navInOrder_df_list <- lapply(seq_len(nrow(navInOrder_df)), function(i) data.frame(value = navInOrder_df[i, ])) # makes each row its own dataframe in a list of dataframes


# Extract all lines between TASK_START	Navigate	NavigationTask and TASK_END	Navigate	NavigationTask

for (j in seq_along(navInOrder_df_list)) {
  text <- navInOrder_df_list[[j]]
  matches <- str_extract_all(text, "(?s)TASK_START\\s+Navigate\\s+NavigationTask.*?TASK_END\\s+Navigate\\s+NavigationTask")
  navInOrder_df_list[[j]] <- data.frame(matches, stringsAsFactors = FALSE)
  # make a list of dataframes in the list of dataframes
  navInOrder_df_list[[j]] <- lapply(seq_len(nrow(navInOrder_df_list[[j]])), function(i) data.frame(value = navInOrder_df_list[[j]][i, ]))
}

# Loop through each of the two loops to do the things below for each loop inside the loop

for (i in seq_along(navInOrder_df_list)) {
  
  for (j in seq_along(navInOrder_df_list[[i]])) {
    
    # Get the dataframe from the list
    data_df <- navInOrder_df_list[[i]][[j]]
    
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
      mutate(avatar_time = avatar_number - avatar_initial_number)
    
    # Update the dataframe in the list
    navInOrder_df_list[[i]][[j]] <- data_df
    
  }
}

# how well Ss learned the outer path
x1 <- navInOrder_df_list[[1]][[1]]$pos_X
z1 <- navInOrder_df_list[[1]][[1]]$pos_Z
plot(x1,z1)

x2 <- navInOrder_df_list[[1]][[2]]$pos_X
z2 <- navInOrder_df_list[[1]][[2]]$pos_Z
plot(x2,z2)

x3 <- navInOrder_df_list[[1]][[3]]$pos_X
z3 <- navInOrder_df_list[[1]][[3]]$pos_Z
plot(x3,z3)

x4 <- navInOrder_df_list[[1]][[4]]$pos_X
z4 <- navInOrder_df_list[[1]][[4]]$pos_Z
plot(x4,z4)

# how well the Ss learned the inner path
x1 <- navInOrder_df_list[[2]][[1]]$pos_X
z1 <- navInOrder_df_list[[2]][[1]]$pos_Z
plot(x1,z1)

x2 <- navInOrder_df_list[[2]][[2]]$pos_X
z2 <- navInOrder_df_list[[2]][[2]]$pos_Z
plot(x2,z2)

x3 <- navInOrder_df_list[[2]][[3]]$pos_X
z3 <- navInOrder_df_list[[2]][[3]]$pos_Z
plot(x3,z3)

x4 <- navInOrder_df_list[[2]][[4]]$pos_X
z4 <- navInOrder_df_list[[2]][[4]]$pos_Z
plot(x4,z4)


############# Extract all lines between TASK_START TASK_NavigationTest and TASK_END TASK_NavigationTest

matches <- str_extract_all(text, "(?s)TASK_START\\s+TASK_NavigationTest\\s+(.*?)\\s+TASK_END\\s+TASK_NavigationTest") # finds the data between the start and end point
navTest_df <- data.frame(matches, stringsAsFactors = FALSE) # creates a dataframe with a row being each chunck of data 
colnames(navTest_df)[1] <- "navInOrder_task" # renames the first column

navInOrder_df_list <- lapply(seq_len(nrow(navInOrder_df)), function(i) data.frame(value = navInOrder_df[i, ])) # makes each row its own dataframe in a list of dataframes


# Extract all lines between TASK_START	Navigate	NavigationTask and TASK_END	Navigate	NavigationTask

for (j in seq_along(navInOrder_df_list)) {
  text <- navInOrder_df_list[[j]]
  matches <- str_extract_all(text, "(?s)TASK_START\\s+Navigate\\s+NavigationTask.*?TASK_END\\s+Navigate\\s+NavigationTask")
  navInOrder_df_list[[j]] <- data.frame(matches, stringsAsFactors = FALSE)
  # make a list of dataframes in the list of dataframes
  navInOrder_df_list[[j]] <- lapply(seq_len(nrow(navInOrder_df_list[[j]])), function(i) data.frame(value = navInOrder_df_list[[j]][i, ]))
}

# Loop through each of the two loops to do the things below for each loop inside the loop

for (i in seq_along(navInOrder_df_list)) {
  
  for (j in seq_along(navInOrder_df_list[[i]])) {
    
    # Get the dataframe from the list
    data_df <- navInOrder_df_list[[i]][[j]]
    
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
      mutate(avatar_time = avatar_number - avatar_initial_number)
    
    # Update the dataframe in the list
    navInOrder_df_list[[i]][[j]] <- data_df
    
  }
}

# how well Ss learned the outer path
x1 <- navInOrder_df_list[[1]][[1]]$pos_X
z1 <- navInOrder_df_list[[1]][[1]]$pos_Z
plot(x1,z1)

x2 <- navInOrder_df_list[[1]][[2]]$pos_X
z2 <- navInOrder_df_list[[1]][[2]]$pos_Z
plot(x2,z2)

x3 <- navInOrder_df_list[[1]][[3]]$pos_X
z3 <- navInOrder_df_list[[1]][[3]]$pos_Z
plot(x3,z3)

x4 <- navInOrder_df_list[[1]][[4]]$pos_X
z4 <- navInOrder_df_list[[1]][[4]]$pos_Z
plot(x4,z4)

# how well the Ss learned the inner path
x1 <- navInOrder_df_list[[2]][[1]]$pos_X
z1 <- navInOrder_df_list[[2]][[1]]$pos_Z
plot(x1,z1)

x2 <- navInOrder_df_list[[2]][[2]]$pos_X
z2 <- navInOrder_df_list[[2]][[2]]$pos_Z
plot(x2,z2)

x3 <- navInOrder_df_list[[2]][[3]]$pos_X
z3 <- navInOrder_df_list[[2]][[3]]$pos_Z
plot(x3,z3)

x4 <- navInOrder_df_list[[2]][[4]]$pos_X
z4 <- navInOrder_df_list[[2]][[4]]$pos_Z
plot(x4,z4)


############################ This is still plotting everything, need to plot per task part


##### Above this section, the code works but it doesn't split the chunks into learned trials 

#x <- data_df$pos_X
#z <- data_df$pos_Z

#plot(x,z)


# Still need to split the data into the different chunks (each learning trial and navigation trial)
# TASK_START LearnSmoothPassive SmoothPassivePathStart
# TASK_START LearnActivePath ActivePathStart
# TASK_START	TASK_NavigateInOrder
# TASK_START TASK_NavigationTest
# TASK_START	Navigate	NavigationTask





################################









