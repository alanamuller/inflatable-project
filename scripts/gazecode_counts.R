# load libraries
library(readxl)
library(tidyr)
library(tidyverse)

rm(list = ls())

# import data
rawData <- read_excel("E:/Nav_1stYr_project_data/GazeCode data/Recording025__01_updated.xlsx")

# make a copy to work with
myData <- rawData
myData <- as.data.frame(myData)
myData$label <- as.factor(myData$label)
myData$videoTime <- myData$`fix start (ms)`/1000

# histogram of label categories
ggplot(myData, aes(x = label)) +
  geom_bar() + theme_classic() + 
  scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6", "7", "8", "9"), 
                   labels=c("LM", "Door", "SO", "DOSW", "Wall", "DODW", "Cart", "Other", "CO"))


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
                       label_counts$count[2]), same_object = label_counts$count[3],
                       DOSW = label_counts$count[4], other = label_counts$count[5], DODW = label_counts$count[6],
                       obj_to_lm = obj_to_lm, lm_to_obj = lm_to_obj, obj_to_so = obj_to_so, 
                       obj_to_diffObj = obj_to_diffObj,
                       lm_to_lm = lm_to_lm)
  print("Table saved to global environment")
}


category_counts(2,1,"study",98,131)
trial_1_study <- newTable

category_counts(2,1,"retrieval",245,409)
trial_1_retrieval <- newTable

category_counts(2,2,"study",629,660)
trial_2_study <- newTable

category_counts(2,2,"retrieval",747,858)
trial_2_retrieval <- newTable

category_counts(2,3,"study",1168,1201)
trial_3_study <- newTable

category_counts(2,3,"retrieval",1339,1477)
trial_3_retrieval <- newTable

category_counts(2,4,"study",1667,1701)
trial_4_study <- newTable

category_counts(2,4,"retrieval",1797,1918)
trial_4_retrieval <- newTable

category_counts(2,5,"study",2086,1218)
trial_5_study <- newTable

category_counts(2,5,"retrieval",2223,2349)
trial_5_retrieval <- newTable

category_counts(2,6,"study",74,110)
trial_6_study <- newTable

category_counts(2,6,"retrieval",224,350)
trial_6_retrieval <- newTable

category_counts(2,7,"study",505,543)
trial_7_study <- newTable

category_counts(2,7,"retrieval",623,757)
trial_7_retrieval <- newTable

category_counts(2,8,"study",938,974)
trial_8_study <- newTable

category_counts(2,8,"retrieval",1069,1170)
trial_8_retrieval <- newTable

category_counts(2,9,"study",1350,1383)
trial_9_study <- newTable

category_counts(2,9,"retrieval",1479,1587)
trial_9_retrieval <- newTable

category_counts(2,10,"study",1758,1792)
trial_10_study <- newTable

category_counts(2,10,"retrieval",1863,1955)
trial_10_retrieval <- newTable


subject2table <- rbind(trial_1_study,trial_1_retrieval,trial_2_study,trial_2_retrieval,
                       trial_3_study,trial_3_retrieval,trial_4_study,trial_4_retrieval,
                       trial_5_study,trial_5_retrieval,trial_6_study,trial_6_retrieval,
                       trial_7_study,trial_7_retrieval,trial_8_study,trial_8_retrieval,
                       trial_9_study,trial_9_retrieval,trial_10_study,trial_10_retrieval,)

