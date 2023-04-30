library(stringr)
library(tidyverse)
library(dplyr)

# Made by Alana Muller with a lot of help from ChatGPT

# Set working directory
setwd("C:/Users/amuller/Desktop/Alana/UA/HSCL/Stress Shortcuts/stress-shortcuts-collab/data/tmp")

# Load the data
input_file <- "navStress_pilotAM_city1_navigation_23-03-21D_16.35.12T.log"
input_data <- paste(readLines(input_file), collapse="\n")
text <- input_data


############# Extract all lines between TASK_START LearnSmoothPassive SmoothPassivePathStart and TASK_END LearnSmoothPassive SmoothPassivePathStart

matches <- str_extract_all(text, "(?s)TASK_START\\s+LearnSmoothPassive\\s+SmoothPassivePathStart.*?TASK_END\\s+LearnSmoothPassive\\s+SmoothPassivePathStart")
learn_passive_df <- data.frame(matches, stringsAsFactors = FALSE)
colnames(learn_passive_df)[1] <- "learn_passive_task"

learn_passive_df_list <- lapply(seq_len(nrow(learn_passive_df)), function(i) data.frame(value = learn_passive_df[i, ]))


############# Loop through each of the dataframes in the list to do the stuff below

for (i in seq_along(learn_passive_df_list)) {
  
  # Get the dataframe from the list
  data_df <- learn_passive_df_list[[i]]
  
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
  
  # Update the dataframe in the list
  learn_passive_df_list[[i]] <- data_df
  
}

learn_pass_df1_list <- learn_passive_df_list[1]
# combine the dataframes into a single dataframe
learn_pass_df1 <- do.call(rbind, learn_pass_df1_list)

max(learn_pass_df1$pos_X)
min(learn_pass_df1$pos_X)

max(learn_pass_df1$pos_Z)
min(learn_pass_df1$pos_Z)

x <- learn_pass_df1$pos_X
z <- learn_pass_df1$pos_Z

max(x, na.rm = TRUE)

plot(x,z)

############################ This is still plotting everything, need to plot per task part

#############################################




##### Above this section, the code works but it doesn't split the chunks into learned trials 

#x <- data_df$pos_X
#z <- data_df$pos_Z

#plot(x,z)


# Still need to split the data into the different chunks (each learning trial and navigation trial)
# TASK_START LearnSmoothPassive SmoothPassivePathStart
# TASK_START LearnActivePath ActivePathStart
# TASK_START	TASK_NavigateInOrder
# TASK_START TASK_NavigationTest





library(stringr)

text <- "63816379065996	TASK_START	LearnSmoothPassive	SmoothPassivePathStart
63816379066012	Avatar: 	KeyboardMouseController		
63816379066025	Avatar: 	KeyboardMouseController
63816379066046	Avatar: 	KeyboardMouseController
63816379217199	TASK_END	LearnSmoothPassive	SmoothPassivePathStart	151203
63816379217200	LM_Output	TaskList
TaskListName	RepetitionNumber	CatchFlag
63816379065996	TASK_START	LearnSmoothPassive	SmoothPassivePathStart
63816379066012	Avatar: 	KeyboardMouseController		
63816379066025	Avatar: 	KeyboardMouseController
63816379066046	Avatar: 	KeyboardMouseController
63816379217199	TASK_END	LearnSmoothPassive	SmoothPassivePathStart	151203
63816379217200	LM_Output	TaskList
TaskListName	RepetitionNumber	CatchFlag"

task_start <- "TASK_START	LearnSmoothPassive"
task_end <- "TASK_END	LearnSmoothPassive"

task_text <- str_sub(text, 
                     str_locate(text, fixed(task_start))[2], 
                     str_locate(text, fixed(task_end))[1] - 1)



#############################

# create a list of dataframes
df_list <- list(data.frame(x = 1:3, y = 4:6), data.frame(x = 4:6, y = 7:9))

# combine the dataframes into a single dataframe
df <- do.call(rbind, df_list)



