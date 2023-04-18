library(regexPipes)
library(regexSelect)

setwd("C:/Users/amuller/Desktop/Alana/UA/HSCL/Stress Shortcuts/stress-shortcuts-collab/data/tmp")

input_file <- "mini_test_log.txt"

input_lines <- readLines(input_file)


position_numbers <- lapply(input_lines, function(line) {
  regmatches(line, regexpr(line, "(?<=Position \\(xyz\\):\\s)[-0-9.]+", perl = TRUE))
})

# Convert the resulting list to a data frame
df <- data.frame(do.call("rbind", position_numbers))

# Rename the columns
names(df) <- c("X", "Y", "Z")

# Convert the columns to numeric format
df$X <- as.numeric(df$X)
df$Y <- as.numeric(df$Y)
df$Z <- as.numeric(df$Z)

# Print the resulting data frame
print(df)

