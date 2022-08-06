# load libraries
library(readxl)
library(tidyr)
library(tidyverse)

rm(list = ls())

# E is the drive on my work PC, D is the drive on my laptop, change accordingly
setwd("D:/Nav_1stYr_project_data/GazeCode data")

# subject number
subjnum <- 22

# I just need some working data sheet here so the script runs, don't use trial 1-3
data_part1 <- "D:/Nav_1stYr_project_data/GazeCode data/s020_part1_data.xlsx"

data_part2 <- "D:/Nav_1stYr_project_data/GazeCode data/s022_part2_noPart1_data.xlsx"
data_part3 <- "D:/Nav_1stYr_project_data/GazeCode data/s022_part3_data.xlsx"

##### import data part 1
rawData_part1 <- read_excel(data_part1)

# make a copy to work with
myData <- rawData_part1
myData <- as.data.frame(myData)
myData$label <- as.factor(myData$label)
myData$videoTime <- myData$`fix start (ms)`/1000

# histogram of label categories
hist_part1 <- ggplot(myData, aes(x = label)) +
  geom_bar() + theme_classic() + 
  scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6", "7", "8", "9"), 
                   labels=c("LM", "Door", "SO", "DOSW", "Wall", "DODW", "Cart", "Other", "CO")) +
  ggtitle("Trials 1-5") + theme(plot.title = element_text(hjust = 0.5))
hist_part1

### Make counting/pair counting function
category_counts <- function(subjectNum,trialNum,trial_type,startSec,endSec) {
  funcData <- myData %>%
    filter(videoTime >= startSec & videoTime <= endSec) # filter data to only important time frames
  
  myData <- myData %>%
    filter(label != 0) # filter out the label 0's (fixations weren't coded, aka dead time)
  
  # make table of counts for each label category
  # 1 = landmark, 2 = door, 3 = same object
  # 4 = diff object same wall, 5 = wall
  # 6 = diff object diff wall, 7 = cart
  # 8 = other, 9 = chosen object
  
  label_counts <- funcData %>%
    group_by(label) %>%
    summarise(
      count = n(),
    )
  
  # get table of counts for each event with consecutive codes
  z <- funcData$label
  zd <- as.data.frame(z)
  pairs <- data.frame(head(zd, -1), tail(zd, -1))
  names(pairs)[1] <- "first" # rename first column
  names(pairs)[2] <- "second" # rename second column
  pairs_table <- table(pairs) # save the table
  pairs_table <- pairs_table[-1,-1] # remove the first row and column
  
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


category_counts(subjnum,1,"study",0,	0)
trial_1_study <- newTable

category_counts(subjnum,1,"retrieval",0	,0)
trial_1_retrieval <- newTable

category_counts(subjnum,2,"study",0,	0)
trial_2_study <- newTable

category_counts(subjnum,2,"retrieval",0	,0)
trial_2_retrieval <- newTable

category_counts(subjnum,3,"study",0,	0)
trial_3_study <- newTable

category_counts(subjnum,3,"retrieval",0,	0)
trial_3_retrieval <- newTable

##### import data for part 2
rawData_part2 <- read_excel(data_part2)

# make a copy to work with
myData <- rawData_part2
myData <- as.data.frame(myData)
myData$label <- as.factor(myData$label)
myData$videoTime <- myData$`fix start (ms)`/1000

# histogram of label categories
hist_part2 <- ggplot(myData, aes(x = label)) +
  geom_bar() + theme_classic() + 
  scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6", "7", "8", "9"), 
                   labels=c("LM", "Door", "SO", "DOSW", "Wall", "DODW", "Cart", "Other", "CO")) +
  ggtitle("Trials 6-10") + theme(plot.title = element_text(hjust = 0.5))
hist_part2

category_counts(subjnum,4,"study",163,	197)
trial_4_study <- newTable

category_counts(subjnum,4,"retrieval",323,	444)
trial_4_retrieval <- newTable

category_counts(subjnum,5,"study",619	,653)
trial_5_study <- newTable

category_counts(subjnum,5,"retrieval",768	,868)
trial_5_retrieval <- newTable


#### import data part 3
rawData_part3 <- read_excel(data_part3)

# make a copy to work with
myData <- rawData_part3
myData <- as.data.frame(myData)
myData$label <- as.factor(myData$label)
myData$videoTime <- myData$`fix start (ms)`/1000


# finish category counts
category_counts(subjnum,6,"study",61,	95)
trial_6_study <- newTable

category_counts(subjnum,6,"retrieval",190	,298)
trial_6_retrieval <- newTable

category_counts(subjnum,7,"study",486	,520)
trial_7_study <- newTable

category_counts(subjnum,7,"retrieval",616,	751)
trial_7_retrieval <- newTable

category_counts(subjnum,8,"study",925	,958)
trial_8_study <- newTable

category_counts(subjnum,8,"retrieval",1067,	1154)
trial_8_retrieval <- newTable

category_counts(subjnum,9,"study",1337,	1370)
trial_9_study <- newTable

category_counts(subjnum,9,"retrieval",1467,	1588)
trial_9_retrieval <- newTable

category_counts(subjnum,10,"study",1771	,1806)
trial_10_study <- newTable

category_counts(subjnum,10,"retrieval",1889,	2009)
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
setwd("D:/Nav_1stYr_project_data/GazeCode data/R_outputs")

sink("subject_022_gazeCodeCounts.csv")
write.csv(subject_table, row.names = FALSE)
cat("\n")
cat("\n")
cat("\n")
write.csv(subject_table_wide, row.names = FALSE)
sink()




