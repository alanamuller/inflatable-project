# Count fixation number and duration from .xls exported from gazecode
# Made for Sara summer research work

# Clear the environment to start fresh
rm(list = ls())

##### Read in file with subject, condition, and city data
setwd("E:/Nav_1stYr_project_data/GazeCode data/raw_gazecode_output") # set working directory

# Define a function
convert_to_min_sec <- function(seconds) {
  minutes <- floor(seconds / 60)
  remaining_seconds <- round(seconds %% 60, 2)
  sprintf("%d:%05.2f", minutes, remaining_seconds)
}

# Import raw data

##### CHANGE THE FILE NAME
subj_num <- 2
recording_num <- 1

# This will import the data based on the subj_num and recording_num you input
file_name <- paste0("oa", subj_num, "_", recording_num, ".xls")

rawData <- read.delim(file_name) # read in file

# Make a copy to work with
myData <- rawData

# Make a column of seconds instead of ms - easier to work with mentally
myData$fix.start.sec <- myData$fix.start..ms./1000
myData$fix.end.sec <- myData$fix.end..ms./1000
myData$fix.start.min.sec <- sapply(myData$fix.start.sec, convert_to_min_sec)

# I need to separate the data by label, group together the fixations that fall within a certain duration,
# transform the start and end columns from ms to mins and s, count the number of fixations, and calculate average duration

# Separate by fixation number - 0 means not coded so no need to pay attention to those
dfs <- list() # create a list to store each of the dataframes for each label number 1-9

# Make separate lists for each label number
for (i in 1:9) {
  dfs[[paste0("df_", i)]] <- myData[myData$label == i, ]
}

# Function to group fixations together within 60 of each other
find_next_group <- function(df, start_index) {
  subset_df <- df[start_index, ]
  for (i in (start_index + 1):nrow(df)) {
    time_diff <- df$fix.start.sec[i] - df$fix.start.sec[i-1]
    
    # Debugging print statement
    cat("Processing row:", i, "Time diff:", time_diff, "\n")
    
    # Check if time_diff is NA
    if (!is.na(time_diff) && time_diff <= 60) {
      subset_df <- rbind(subset_df, df[i, ])
    } else {
      return(list(subset_df = subset_df, next_start_index = i))
    }
  }
  return(list(subset_df = subset_df, next_start_index = nrow(df) + 1))
}

# Loop over all dfs

for (i in seq_along(dfs)){
  
  # States which dataframe we're working with now
  current_df <- dfs[[i]]
  
  # Initialize a list to store the subsets
  all_subsets <- list()
  
  # Define starting index as 1
  start_index <- 1
  
  while (start_index <= nrow(current_df)) {
    result <- find_next_group(current_df, start_index)
    all_subsets <- append(all_subsets, list(result$subset_df))
    start_index <- result$next_start_index
  }
  
  new_name <- paste0("group_", i)
  assign(new_name, all_subsets)
}

# Make a giant list of all groups
big_list <- list(group_1 = group_1, 
                 group_2 = group_2, 
                 group_3 = group_3, 
                 group_4 = group_4, 
                 group_5 = group_5, 
                 group_6 = group_6, 
                 group_7 = group_7, 
                 group_8 = group_8, 
                 group_9 = group_9)

# Initialize the empty table to work with in the loop
combo_table <- data.frame(label = numeric(), fix_num = numeric(), total_fix_dur_ms = numeric(), avg_fix_dur_ms = numeric(), fix_start_min.sec = numeric()) # create an empty dataframe to fill in

# Create table with all labels and info below
for (i in seq_along(big_list)){
  # Set current group list
  current_group_list <- big_list[[i]]
  
  # Loop through the lists in the current group list
  for (j in seq_along(current_group_list)){
    # Set current fixation group list
    current_fix_list <- current_group_list[[j]]
    
    # Do the measures below and append a new row to the table
    combo_table <- rbind(combo_table, data.frame(
    label = current_fix_list$label[1], # add the label number
    fix_num = nrow(current_fix_list), # add number of fixations on that label
    total_fix_dur_ms = sum(current_fix_list$fix.dur..ms., na.rm = TRUE), # add total fixation duration
    avg_fix_dur_ms = mean(current_fix_list$fix.dur..ms., na.rm = TRUE), # add average fixation duration
    fix_start_min.sec = current_fix_list$fix.start.min.sec[1] # add the time the fixation started in mins and secs
    ))
  }
}

print(combo_table) # print table to console so you can check the numbers

# Write fixation table to a new csv file
write.csv(combo_table, paste0("E:/Nav_1stYr_project_data/OA_gazecode_outputs/OA", subj_num, "_", recording_num,".csv"), row.names = FALSE)

