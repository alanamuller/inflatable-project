
# load libraries
library(readxl)
library(tidyr)
library(tidyverse)

# import data
rawData <- read_excel("E:/Nav_1stYr_project_data/GazeCode data/Recording025__01.xlsx")

# make a copy to work with
myData <- rawData
myData <- as.data.frame(myData)
myData$label <- as.factor(myData$label)

# filter out the label 0's which mean fixations weren't coded (represents dead time)
myData <- myData %>%
  filter(label != 0)

# make table of counts for each label category
  # 1 = landmark, 2 = door, 3 = same object
  # 4 = diff object same wall, 5 = other
  # 6 = diff object diff wall, 7 = cart
  # 8 = chosen object, 9 = first object
label_counts <- myData %>%
group_by(label) %>%
  summarise(
    count = n(),
  )

# histogram of label categories
ggplot(myData, aes(x = label)) +
  geom_bar() + theme_classic()

# get table of counts for each event with consecutive codes
z <- myData$label
zd <- as.data.frame(z)
pairs <- data.frame(head(zd, -1), tail(zd, -1))
names(pairs)[1] <- "first" # rename first column
names(pairs)[2] <- "second" # rename second column
pairs_table <- table(pairs) # save the table
pairs_table <- pairs_table[-1,-1] # remove the first row and column

# add counts for each category
  
# 9-1, 3-1, 8-1, 4-1, 6-1, 9-2, 3-2, 8-2, 4-2, 6-2
obj_to_lm <- sum(pairs_table[9,1],pairs_table[3,1],pairs_table[8,1],pairs_table[4,1],pairs_table[6,1],
                 pairs_table[9,2],pairs_table[3,2],pairs_table[8,2],pairs_table[4,2],pairs_table[6,2])

# 1-9, 1-3, 1-8, 1-4, 1-6, 2-9, 2-3, 2-8, 2-4, 2-6
lm_to_obj <- sum(pairs_table[1,9],pairs_table[1,3],pairs_table[1,8],pairs_table[1,4],pairs_table[1,6],
                 pairs_table[2,9],pairs_table[2,3],pairs_table[2,8],pairs_table[2,4],pairs_table[2,6])

# 9-3, 8-3, 4-3, 6-3
obj_to_so <- sum(pairs_table[9,3],pairs_table[8,3],pairs_table[4,3],pairs_table[6,3])

# 9-4, 9-6, 8-1, 8-6, 3-4, 3-6, 4-4, 4-6, 6-4, 6-6
obj_to_diffObj <- sum(pairs_table[9,4],pairs_table[9,6],pairs_table[8,1],pairs_table[8,6],
                      pairs_table[3,4],pairs_table[3,6],pairs_table[4,4],pairs_table[4,6],
                      pairs_table[6,4],pairs_table[6,6])

# 1-1, 2-2, 1-2, 2-1
lm_to_lm <- sum(pairs_table[1,1],pairs_table[2,2],pairs_table[1,2],pairs_table[2,1])

# dataframe of values
values <- data.frame(landmarks = sum(label_counts$count[1], label_counts$count[2]), same_object = label_counts$count[3],
                     DOSW = label_counts$count[4], other = label_counts$count[5], DODW = label_counts$count[6],
                     obj_to_lm = obj_to_lm, lm_to_obj = lm_to_obj, obj_to_so = obj_to_so, obj_to_diffObj = obj_to_diffObj,
                     lm_to_lm = lm_to_lm)

    
