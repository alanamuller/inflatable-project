library(regexPipes)
library(regexSelect)

setwd("C:/Users/amuller/Desktop/Alana/UA/HSCL/Stress Shortcuts/stress-shortcuts-collab/data/tmp")

input_file <- "mini_test_log.txt"

input_lines <- readLines(input_file)




library(stringr)
library(tidyverse)

# Read in the data
data <- "63815013312726	TASK_START	LM_Timeline	TaskList
63815013329493\tAvatar:\tKeyboardMouseController\tPosition (xyz):\t-230.886\t-0.09318924\t278.2897\tRotation (xyz):\t359.5977\t272.9221\t1.334054E-08\tCamera\t(xyz):\t359.5977\t272.9221\t1.334054E-08
63815013329500\tAvatar:\tKeyboardMouseController\tPosition (xyz):\t-230.886\t-0.09318924\t278.2897\tRotation (xyz):\t359.596\t273.33\t0.001655868\tCamera\t(xyz):\t359.596\t273.33\t0.001655868
63815013329519\tAvatar:\tKeyboardMouseController\tPosition (xyz):\t-230.886\t-0.09318924\t278.2897\tRotation (xyz):\t359.5945\t273.6767\t0.003053638\tCamera\t(xyz):\t359.5945\t273.6767\t0.003053638"



data <- paste(readLines("C:/Users/amuller/Desktop/Alana/UA/HSCL/Stress Shortcuts/stress-shortcuts-collab/data/tmp/mini_test_log.txt"), collapse="\n")


# Convert the data to a tibble
df <- tibble(data = str_split(data, "\n")[[1]])

# Use regular expressions to extract the three numbers after "Position (xyz):"
df <- df %>%
  mutate(numbers = str_extract_all(data, "(?<=Position \\(xyz\\):\\s)[0-9.-]+\\s+[0-9.-]+\\s+[0-9.-]+"))

# Split the numbers column into three separate columns
df <- df %>%
  separate(numbers, into = c("X", "Y", "Z"), sep = "\t")

# Convert the columns to numeric
df <- df %>%
  mutate(across(c(X, Y, Z), as.numeric))



