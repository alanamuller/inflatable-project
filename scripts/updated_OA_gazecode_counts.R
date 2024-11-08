# load libraries
library(readxl)
library(tidyr)
library(tidyverse)

rm(list = ls())

# E is the drive on my work PC, D is the drive on my laptop, change accordingly
setwd("E:/Nav_1stYr_project_data/GazeCode data/OA_sheets/study_retrieval_coding")

# subject number
subjnum <- 11

data_part1 <- paste0("oa", subjnum, "_part1.xls")
data_part2 <- paste0("oa", subjnum, "_part2.xls")
data_part3 <- paste0("oa", subjnum, "_part3.xls")
data_part4 <- paste0("oa", subjnum, "_part4.xls")

##### add in the start and end times for study and retrieval, td = times data

study_td <- data.frame(
  col1 <- c(149,
            611,
            53,
            444,
            875,
            1322,
            1730,
            2171,
            2541,
            48
            
  ),
  col2 <- c(186,
            646,
            88,
            476,
            910,
            1355,
            1765,
            2207,
            2575,
            81
            
  )
)

study_start1 <- study_td[1, 1]
study_end1 <- study_td[1, 2]
study_start2 <- study_td[2, 1]
study_end2 <- study_td[2, 2]
study_start3 <- study_td[3, 1]
study_end3 <- study_td[3, 2]
study_start4 <- study_td[4, 1]
study_end4 <- study_td[4, 2]
study_start5 <- study_td[5, 1]
study_end5 <- study_td[5, 2]
study_start6 <- study_td[6, 1]
study_end6 <- study_td[6, 2]
study_start7 <- study_td[7, 1]
study_end7 <- study_td[7, 2]
study_start8 <- study_td[8, 1]
study_end8 <- study_td[8, 2]
study_start9 <- study_td[9, 1]
study_end9 <- study_td[9, 2]
study_start10 <- study_td[10, 1]
study_end10 <- study_td[10, 2]


retrieval_td <- data.frame(
  col1 <- c(291,
            780,
            192,
            580,
            1016,
            1452,
            1873,
            2296,
            2686,
            162
            
  ),
  col2 <- c(456,
            800,
            294,
            703,
            1125,
            1571,
            2010,
            2379,
            2816,
            254
            
  )
)

retrieval_start1 <- retrieval_td[1, 1]
retrieval_end1 <- retrieval_td[1, 2]
retrieval_start2 <- retrieval_td[2, 1]
retrieval_end2 <- retrieval_td[2, 2]
retrieval_start3 <- retrieval_td[3, 1]
retrieval_end3 <- retrieval_td[3, 2]
retrieval_start4 <- retrieval_td[4, 1]
retrieval_end4 <- retrieval_td[4, 2]
retrieval_start5 <- retrieval_td[5, 1]
retrieval_end5 <- retrieval_td[5, 2]
retrieval_start6 <- retrieval_td[6, 1]
retrieval_end6 <- retrieval_td[6, 2]
retrieval_start7 <- retrieval_td[7, 1]
retrieval_end7 <- retrieval_td[7, 2]
retrieval_start8 <- retrieval_td[8, 1]
retrieval_end8 <- retrieval_td[8, 2]
retrieval_start9 <- retrieval_td[9, 1]
retrieval_end9 <- retrieval_td[9, 2]
retrieval_start10 <- retrieval_td[10, 1]
retrieval_end10 <- retrieval_td[10, 2]

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

category_counts(subjnum,1,"study",study_start1,	study_end1)
trial_1_study <- newTable

category_counts(subjnum,1,"retrieval",retrieval_start1	,retrieval_end1)
trial_1_retrieval <- newTable

category_counts(subjnum,2,"study",study_start2,	study_end2)
trial_2_study <- newTable

category_counts(subjnum,2,"retrieval",retrieval_start2	,retrieval_end2)
trial_2_retrieval <- newTable



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

category_counts(subjnum,3,"study",study_start3,	study_end3)
trial_3_study <- newTable

category_counts(subjnum,3,"retrieval",retrieval_start3,	retrieval_end3)
trial_3_retrieval <- newTable

category_counts(subjnum,4,"study",study_start4,	study_end4)
trial_4_study <- newTable

category_counts(subjnum,4,"retrieval",retrieval_start4,	retrieval_end4)
trial_4_retrieval <- newTable

category_counts(subjnum,5,"study",study_start5,	study_end5)
trial_5_study <- newTable

category_counts(subjnum,5,"retrieval",retrieval_start5,	retrieval_end5)
trial_5_retrieval <- newTable

category_counts(subjnum,6,"study",study_start6	,study_end6)
trial_6_study <- newTable

category_counts(subjnum,6,"retrieval",retrieval_start6,	retrieval_end6)
trial_6_retrieval <- newTable

category_counts(subjnum,7,"study",study_start7	,study_end7)
trial_7_study <- newTable

category_counts(subjnum,7,"retrieval",retrieval_start7,	retrieval_end7)
trial_7_retrieval <- newTable

category_counts(subjnum,8,"study",study_start8	,study_end8)
trial_8_study <- newTable

category_counts(subjnum,8,"retrieval",retrieval_start8,	retrieval_end8)
trial_8_retrieval <- newTable

category_counts(subjnum,9,"study",study_start9,	study_end9)
trial_9_study <- newTable

category_counts(subjnum,9,"retrieval",retrieval_start9,	retrieval_end9)
trial_9_retrieval <- newTable







############### ############### ############### ############### ############### 






 






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


category_counts(subjnum,10,"study",study_start10,	study_end10)
trial_10_study <- newTable

category_counts(subjnum,10,"retrieval",retrieval_start10,	retrieval_end10)
trial_10_retrieval <- newTable


subject_table <- rbind(trial_1_study,trial_1_retrieval,trial_2_study,trial_2_retrieval,
                       trial_3_study,trial_3_retrieval,trial_4_study,trial_4_retrieval,
                       trial_5_study,trial_5_retrieval,trial_6_study,trial_6_retrieval,
                       trial_7_study,trial_7_retrieval,trial_8_study,trial_8_retrieval,
                       trial_9_study,trial_9_retrieval,trial_10_study,trial_10_retrieval)

subject_table_wide <- reshape(subject_table, direction = "wide",
                              idvar = c("subject", "trial"),
                              timevar = "trialType",
                              v.names = c("landmarks", "same_object", "DOSW", "wall", "DODW","cart", "other", "obj_to_lm", "lm_to_obj",
                                          "obj_to_so", "obj_to_diffObj", "lm_to_lm", "timeStart", "timeEnd", "duration"),
                              varying = c("s.landmarks", "s.same_object", "s.DOSW", "s.wall", "s.DODW","s.cart", "s.other", "s.obj_to_lm", "s.lm_to_obj", 
                                          "s.obj_to_so", "s.obj_to_diffObj", "s.lm_to_lm", "s.timeStart", "s.timeEnd", "s.duration",
                                          "r.landmarks", "r.same_object", "r.DOSW", "r.wall", "r.DODW","r.cart", "r.other", "r.obj_to_lm", "r.lm_to_obj",
                                          "r.obj_to_so", "r.obj_to_diffObj", "r.lm_to_lm", "r.timeStart", "r.timeEnd", "r.duration")
)

# E is the drive on my work PC, D is the drive on my laptop, change accordingly
setwd("E:/Nav_1stYr_project_data/GazeCode data/R_outputs")

sink(paste0("OA", subjnum, "_gazeCodeCounts.csv"))
write.csv(subject_table, row.names = FALSE)
cat("\n")
cat("\n")
cat("\n")
write.csv(subject_table_wide, row.names = FALSE)
sink()





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










