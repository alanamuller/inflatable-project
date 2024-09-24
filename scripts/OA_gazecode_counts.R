# load libraries
library(readxl)
library(tidyr)
library(tidyverse)
library(gtools)
library(dplyr)

rm(list = ls())

# Define a function
convert_to_min_sec <- function(seconds) {
  minutes <- floor(seconds / 60)
  remaining_seconds <- round(seconds %% 60, 2)
  sprintf("%d:%05.2f", minutes, remaining_seconds)
}

### Make counting/pair counting function
category_counts <- function(subjectNum,trialNum,trial_type,startSec,endSec) {
  funcData <- myData %>%
    filter(videoTime >= startSec & videoTime <= endSec) # filter data to only important time frames
  
  funcData <- funcData %>%
    filter(label != 0) # filter out the label 0's (fixations weren't coded, aka dead time)
  
  # make table of counts for each label category
  # 1 = landmark, 2 = door, 3 = same object
  # 4 = diff object same wall, 5 = wall
  # 6 = diff object diff wall, 7 = cart
  # 8 = other, 9 = chosen object
  
  label_counts <- data.frame(matrix(NA, nrow = 9, ncol = 2))
  x <- c("label", "count")
  colnames(label_counts) <- x
  
  label_counts[1,1] <- 1
  label_counts[2,1] <- 2
  label_counts[3,1] <- 3
  label_counts[4,1] <- 4
  label_counts[5,1] <- 5
  label_counts[6,1] <- 6
  label_counts[7,1] <- 7
  label_counts[8,1] <- 8
  label_counts[9,1] <- 9
  
  label_counts[1,2] <- sum(funcData$label == 1)
  label_counts[2,2] <- sum(funcData$label == 2)
  label_counts[3,2] <- sum(funcData$label == 3)
  label_counts[4,2] <- sum(funcData$label == 4)
  label_counts[5,2] <- sum(funcData$label == 5)
  label_counts[6,2] <- sum(funcData$label == 6)
  label_counts[7,2] <- sum(funcData$label == 7)
  label_counts[8,2] <- sum(funcData$label == 8)
  label_counts[9,2] <- sum(funcData$label == 9)
  
  # get table of counts for each event with consecutive codes
  z <- funcData$label
  zd <- as.data.frame(z)
  pairs <- data.frame(head(zd, -1), tail(zd, -1))
  names(pairs)[1] <- "first" # rename first column
  names(pairs)[2] <- "second" # rename second column
  pairs_table <- table(factor(pairs$first, levels = 1:9), factor(pairs$second, levels = 1:9)) # save the table
  
  # add counts for each category
  
  # Object --> Landmark 
  # 9-1, 3-1, 4-1, 6-1, 9-2, 3-2, 4-2, 6-2
  obj_to_lm <- sum(pairs_table[9,1],pairs_table[3,1],pairs_table[4,1],pairs_table[6,1],
                   pairs_table[9,2],pairs_table[3,2],pairs_table[4,2],pairs_table[6,2])
  
  # Landmark --> Object
  # 1-9, 1-3, 1-4, 1-6, 2-9, 2-3, 2-4, 2-6
  lm_to_obj <- sum(pairs_table[1,9],pairs_table[1,3],pairs_table[1,4],pairs_table[1,6],
                   pairs_table[2,9],pairs_table[2,3],pairs_table[2,4],pairs_table[2,6])
  
  # Object --> Same Object
  # 9-3, 4-3, 6-3, 3-3
  obj_to_so <- sum(pairs_table[9,3],pairs_table[4,3],pairs_table[6,3],pairs_table[3,3])
  
  # Object --> Diff Object
  # 9-4, 9-6, 3-4, 3-6, 3-9, 4-4, 4-6, 6-4, 6-6
  obj_to_diffObj <- sum(pairs_table[9,4],pairs_table[9,6],
                        pairs_table[3,4],pairs_table[3,6],pairs_table[3,9],
                        pairs_table[4,4],pairs_table[4,6],
                        pairs_table[6,4],pairs_table[6,6])
  
  # Landmark --> Landmark
  # 1-1, 2-2, 1-2, 2-1
  lm_to_lm <- sum(pairs_table[1,1],pairs_table[2,2],pairs_table[1,2],pairs_table[2,1])
  
  # dataframe of values
  newTable <<- data.frame(subject = subjectNum, trial = trialNum, trialType = trial_type, landmarks = sum(label_counts$count[1], 
                       label_counts$count[2]), same_object = label_counts$count[3],DOSW = label_counts$count[4], 
                       wall = label_counts$count[5], DODW = label_counts$count[6], cart = label_counts$count[7], other = label_counts$count[8],
                       obj_to_lm = obj_to_lm, lm_to_obj = lm_to_obj, obj_to_so = obj_to_so, 
                       obj_to_diffObj = obj_to_diffObj, lm_to_lm = lm_to_lm, 
                       timeStart = startSec, timeEnd = endSec, duration = endSec - startSec)
  print("Table saved to global environment")
}
#################################### start importing data below ####################################
####################################################################################################
####################################################################################################

# E is the drive on my work PC, D is the drive on my laptop, change accordingly
setwd("E:/Nav_1stYr_project_data/GazeCode data/OA_sheets/retrieval fixations/sheets_renamed/")

# subject number
subjnum <- 30

data_part1 <- paste0("oa", subjnum, "_part1.xls")
data_part2 <- paste0("oa", subjnum, "_part2.xls")
data_part3 <- paste0("oa", subjnum, "_part3.xls")
data_part4 <- paste0("oa", subjnum, "_part4.xls")

##### add in the start and end times, td = times data

td <- data.frame(
  col1 <- c(517,
            1202,
            1744,
            2245,
            2702,
            198,
            702,
            215,
            651,
            1123
            
  ),
  col2 <- c(651,
            1304,
            1851,
            2328,
            2812,
            311,
            803,
            313,
            813,
            1195
            
  )
)

start1 <- td[1, 1]
end1 <- td[1, 2]
start2 <- td[2, 1]
end2 <- td[2, 2]
start3 <- td[3, 1]
end3 <- td[3, 2]
start4 <- td[4, 1]
end4 <- td[4, 2]
start5 <- td[5, 1]
end5 <- td[5, 2]
start6 <- td[6, 1]
end6 <- td[6, 2]
start7 <- td[7, 1]
end7 <- td[7, 2]
start8 <- td[8, 1]
end8 <- td[8, 2]
start9 <- td[9, 1]
end9 <- td[9, 2]
start10 <- td[10, 1]
end10 <- td[10, 2]



##### import data part 1
rawData_part1 <- read.delim(data_part1)

# make a copy to work with
myData <- rawData_part1
myData <- as.data.frame(myData)
myData$label <- as.factor(myData$label)
myData$videoTime <- myData$fix.start..ms./1000

# histogram of label categories
hist_part1 <- ggplot(myData, aes(x = label)) +
  geom_bar() + theme_classic() + 
  scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6", "7", "8", "9"), 
                   labels=c("LM", "Door", "SO", "DOSW", "Wall", "DODW", "Cart", "Other", "CO")) +
  ggtitle("Some Number of Trials Part 1") + theme(plot.title = element_text(hjust = 0.5))
hist_part1

############### data part 1 retrieval trials processed below ###############

category_counts(subjnum,1,"retrieval",start1,end1
)
trial_1_retrieval <- newTable

category_counts(subjnum,2,"retrieval",start2,end2
)
trial_2_retrieval <- newTable

category_counts(subjnum,3,"retrieval",start3,end3
)
trial_3_retrieval <- newTable

category_counts(subjnum,4,"retrieval",start4,end4
)
trial_4_retrieval <- newTable

category_counts(subjnum,5,"retrieval",start5,end5
)
trial_5_retrieval <- newTable






############### ############### ############### ############### ############### 

##### import data part 2
rawData_part2 <- read.delim(data_part2)

# make a copy to work with
myData <- rawData_part2
myData <- as.data.frame(myData)
myData$label <- as.factor(myData$label)
myData$videoTime <- myData$fix.start..ms./1000

# histogram of label categories
hist_part2 <- ggplot(myData, aes(x = label)) +
  geom_bar() + theme_classic() + 
  scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6", "7", "8", "9"), 
                   labels=c("LM", "Door", "SO", "DOSW", "Wall", "DODW", "Cart", "Other", "CO")) +
  ggtitle("Some Number of Trials Part 2") + theme(plot.title = element_text(hjust = 0.5))
hist_part2

############### data part 2 retrieval trials processed below ###############

category_counts(subjnum,6,"retrieval",start6,end6
)
trial_6_retrieval <- newTable

category_counts(subjnum,7,"retrieval",start7,end7
)
trial_7_retrieval <- newTable





############### ############### ############### ############### ############### 

##### import data part 3
rawData_part3 <- read.delim(data_part3)

# make a copy to work with
myData <- rawData_part3
myData <- as.data.frame(myData)
myData$label <- as.factor(myData$label)
myData$videoTime <- myData$fix.start..ms./1000

# histogram of label categories
hist_part3 <- ggplot(myData, aes(x = label)) +
  geom_bar() + theme_classic() + 
  scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6", "7", "8", "9"), 
                   labels=c("LM", "Door", "SO", "DOSW", "Wall", "DODW", "Cart", "Other", "CO")) +
  ggtitle("Some Number of Trials Part 3") + theme(plot.title = element_text(hjust = 0.5))
hist_part3

############### data part 3 retrieval trials processed below ###############

category_counts(subjnum,8,"retrieval",start8,end8
)
trial_8_retrieval <- newTable

category_counts(subjnum,9,"retrieval",start9,end9
)
trial_9_retrieval <- newTable

category_counts(subjnum,10,"retrieval",start10,end10
)
trial_10_retrieval <- newTable


############### ############### ############### ############### ############### 

##### import data part 4
rawData_part4 <- read.delim(data_part4)

# make a copy to work with
myData <- rawData_part4
myData <- as.data.frame(myData)
myData$label <- as.factor(myData$label)
myData$videoTime <- myData$fix.start..ms./1000

# histogram of label categories
hist_part4 <- ggplot(myData, aes(x = label)) +
  geom_bar() + theme_classic() + 
  scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6", "7", "8", "9"), 
                   labels=c("LM", "Door", "SO", "DOSW", "Wall", "DODW", "Cart", "Other", "CO")) +
  ggtitle("Some Number of Trials Part 4") + theme(plot.title = element_text(hjust = 0.5))
hist_part4

############### data part 4 retrieval trials processed below ###############




############### ############### ############### ############### ############### 
retrieval_tables <- mget(ls(pattern = ".*_retrieval$"))

subject_table <- do.call(rbind, retrieval_tables)

rownames(subject_table)
sorted_table <- subject_table[order(as.numeric(gsub("trial_|_retrieval", "", rownames(subject_table)))), ]

# rename the columns to match the younger adults
colnames(sorted_table)[-c(1,2)] <- paste0("r.", colnames(sorted_table)[-c(1,2)])

# E is the drive on my work PC, D is the drive on my laptop, change accordingly
setwd("E:/Nav_1stYr_project_data/GazeCode data/R_outputs")

sink(paste0("OA_", subjnum, "_retrieval.csv"))
write.csv(sorted_table, row.names = FALSE)
sink()

