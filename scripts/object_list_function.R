library(tidyverse)
library(gtools)

rm(list = ls())

setwd("C:/Users/Updates/Desktop/Alana/UA/R_projects/inflatable-project")

letterAndNumberList <- function(lanl) {
  wall_high <- c("A","B","C")
  wall_low <- c("D","E","F","G")
  
  one_to_three <-c(1,2,3)
  four_to_six <- c(4,5,6)
  seven_to_nine <- c(7,8,9)
  ten_to_twelve <- c(10,11,12)
  
  choose_2high <- sample(wall_high, 2, replace = FALSE)
  choose_2low <- sample(wall_low, 2, replace = FALSE)
  
  letters <- c(choose_2high, choose_2low)
  
  choose_1to3 <- sample(one_to_three, 1, replace = FALSE)
  choose_4to6 <- sample(four_to_six, 1, replace = FALSE)
  choose_7to9 <- sample(seven_to_nine, 1, replace = FALSE)
  choose_10to12 <- sample(ten_to_twelve, 1, replace = FALSE)
  
  nums <- c(choose_1to3, choose_4to6, choose_7to9, choose_10to12)
  
  list1 <- data.frame(matrix(nrow = 4,ncol = 2))
  colnames(list1)[1] <- "letter"
  colnames(list1)[2] <- "number"
  list1[,1] <- sample(letters)
  list1[,2] <- sort(nums)
  
  return(list1tog <- paste(list1$letter,list1$number, sep = ""))
}

DDlist1tog <- letterAndNumberList()
DDlist2tog <- letterAndNumberList()
DDlist3tog <- letterAndNumberList()
DDlist4tog <- letterAndNumberList()
DDlist5tog <- letterAndNumberList()
DDlist6tog <- letterAndNumberList()
DDlist7tog <- letterAndNumberList()
DDlist8tog <- letterAndNumberList()
DDlist9tog <- letterAndNumberList()
DDlist10tog <- letterAndNumberList()

SDlist1tog <- letterAndNumberList()
SDlist2tog <- letterAndNumberList()
SDlist3tog <- letterAndNumberList()
SDlist4tog <- letterAndNumberList()
SDlist5tog <- letterAndNumberList()
SDlist6tog <- letterAndNumberList()
SDlist7tog <- letterAndNumberList()
SDlist8tog <- letterAndNumberList()
SDlist9tog <- letterAndNumberList()
SDlist10tog <- letterAndNumberList()

###############################################################

# Create lists of objects
alive <- c("alien", "alpaca", "astronaut", "cactus", "catepillar", "cow", "dinosaur", "dog", "flower", "frog", "lizard", "pig", "tree") #13
birds <- c("chicken", "flamingo", "owl", "parrot", "penguin") #5
seaCreatures <- c("crab", "dolphin", "goldfish", "lobster", "octopus", "seal", "seahorse", "starfish", "turtle") #9
zooAnimals <- c("elephant", "giraffe", "lion", "monkey", "zebra") #5

thing <- c("baby bottle", "box", "masks", "cowboy hat", "crayon", "crown", "finger",
           "fire", "fire extinguisher", "globe", "oven mitt", "phone", "pinata", "pitcher", "quarter",
           "space shuttle", "sword", "tiki mask", "trophy", "water container") #21 witch hat too hard to velcro, fanny pack decommissioned
food <- c("banana", "bread", "carrot", "chili", "donut", "gummy bear", "hot dog", "pretzel", "turkey") #9 pumpkin decommissioned
game <- c("basketball", "bat", "bowling ball", "bowling pin", "football", "glove", "card") #7, dice decommissioned
music <- c("boombox", "guitar", "microphone", "saxophone", "trumpet") #5
tool <- c("bucket", "hammer", "hanger", "mallet", "pick axe", "screw driver", "wrench") #7


# Shuffle objects within each list
shuffleAlive <- sample(alive)
shuffleBirds <- sample(birds)
shuffleSeaCreatures <- sample(seaCreatures)
shuffleZooAnimals <- sample(zooAnimals)

shuffleThing <- sample(thing)
shuffleFood <- sample(food)
shuffleGame <- sample(game)
shuffleMusic <- sample(music)
shuffleTool <- sample(tool)

# Put the randomized objects into 10 lists of 8 objects
objList1 <- c(shuffleAlive[1:2], shuffleSeaCreatures[1], shuffleZooAnimals[1], shuffleGame[1], shuffleMusic[1], shuffleThing[1:2]) #equal living and non-living
objList2 <- c(shuffleAlive[3:4], shuffleSeaCreatures[2], shuffleZooAnimals[2], shuffleGame[2], shuffleMusic[2], shuffleThing[3:4]) #equal living and non-living
objList3 <- c(shuffleAlive[5], shuffleBirds[1], shuffleSeaCreatures[3], shuffleFood[1], shuffleGame[3], shuffleTool[1], shuffleThing[5:6]) #3 living, 5 non-living 
objList4 <- c(shuffleAlive[6], shuffleBirds[2], shuffleSeaCreatures[4], shuffleFood[2], shuffleGame[4], shuffleTool[2], shuffleThing[7:8]) #3 living, 5 non-living 
objList5 <- c(shuffleAlive[7], shuffleBirds[3], shuffleSeaCreatures[5], shuffleFood[3], shuffleGame[5], shuffleTool[3], shuffleThing[9:10]) #3 living, 5 non-living 
objList6 <- c(shuffleAlive[8], shuffleBirds[4], shuffleSeaCreatures[6], shuffleFood[4], shuffleGame[6], shuffleTool[4], shuffleThing[11:12]) #3 living, 5 non-living 
objList7 <- c(shuffleAlive[9], shuffleBirds[5], shuffleZooAnimals[3], shuffleFood[5], shuffleGame[7], shuffleTool[5], shuffleThing[13:14]) #3 living, 5 non-living 
objList8 <- c(shuffleAlive[10], shuffleSeaCreatures[7], shuffleZooAnimals[4], shuffleFood[6], shuffleMusic[3], shuffleTool[6], shuffleThing[15:16]) #3 living, 5 non-living 
objList9 <- c(shuffleAlive[11], shuffleSeaCreatures[8], shuffleZooAnimals[5], shuffleFood[7:8], shuffleMusic[4], shuffleTool[7], shuffleThing[17]) #3 living, 5 non-living 
objList10 <- c(shuffleAlive[12:13], shuffleSeaCreatures[9], shuffleFood[9], shuffleMusic[5], shuffleThing[18:20]) #equal living and non-living 

#extraObject <- shuffleThing[21] # there is no extra object anymore

# Shuffle the newly created lists
shuffleObjList1 <- sample(objList1)
shuffleObjList2 <- sample(objList2)
shuffleObjList3 <- sample(objList3)
shuffleObjList4 <- sample(objList4)
shuffleObjList5 <- sample(objList5)
shuffleObjList6 <- sample(objList6)
shuffleObjList7 <- sample(objList7)
shuffleObjList8 <- sample(objList8)
shuffleObjList9 <- sample(objList9)
shuffleObjList10 <- sample(objList10)

# Initialize matrices for my trials
list1 <- data.frame(matrix(nrow = 4, ncol = 4))
list2 <- data.frame(matrix(nrow = 4, ncol = 4))
list3 <- data.frame(matrix(nrow = 4, ncol = 4))
list4 <- data.frame(matrix(nrow = 4, ncol = 4))
list5 <- data.frame(matrix(nrow = 4, ncol = 4))
list6 <- data.frame(matrix(nrow = 4, ncol = 4))
list7 <- data.frame(matrix(nrow = 4, ncol = 4))
list8 <- data.frame(matrix(nrow = 4, ncol = 4))
list9 <- data.frame(matrix(nrow = 4, ncol = 4))
list10 <- data.frame(matrix(nrow = 4, ncol = 4))

# Rename the columns for the above created matrices
renameColumns <- function(listName) {
  colnames(listName)[1] <- "DD_#"
  colnames(listName)[2] <- "DD_object"
  colnames(listName)[3] <- "SD_#"
  colnames(listName)[4] <- "SD_object"
  return(listName)
}

list1 <- renameColumns(list1)
list2 <- renameColumns(list2)
list3 <- renameColumns(list3)
list4 <- renameColumns(list4)
list5 <- renameColumns(list5)
list6 <- renameColumns(list6)
list7 <- renameColumns(list7)
list8 <- renameColumns(list8)
list9 <- renameColumns(list9)
list10 <- renameColumns(list10)

# Fill the initialized matrices with grid placements and object names
list1[,1] <- DDlist1tog
list1[,2] <- shuffleObjList1[1:4]
list1[,3] <- SDlist1tog
list1[,4] <- shuffleObjList1[5:8]

list2[,1] <- DDlist2tog
list2[,2] <- shuffleObjList2[1:4]
list2[,3] <- SDlist2tog
list2[,4] <- shuffleObjList2[5:8]

list3[,1] <- DDlist3tog
list3[,2] <- shuffleObjList3[1:4]
list3[,3] <- SDlist3tog
list3[,4] <- shuffleObjList3[5:8]

list4[,1] <- DDlist4tog
list4[,2] <- shuffleObjList4[1:4]
list4[,3] <- SDlist4tog
list4[,4] <- shuffleObjList4[5:8]

list5[,1] <- DDlist5tog
list5[,2] <- shuffleObjList5[1:4]
list5[,3] <- SDlist5tog
list5[,4] <- shuffleObjList5[5:8]

list6[,1] <- DDlist6tog
list6[,2] <- shuffleObjList6[1:4]
list6[,3] <- SDlist6tog
list6[,4] <- shuffleObjList6[5:8]

list7[,1] <- DDlist7tog
list7[,2] <- shuffleObjList7[1:4]
list7[,3] <- SDlist7tog
list7[,4] <- shuffleObjList7[5:8]

list8[,1] <- DDlist8tog
list8[,2] <- shuffleObjList8[1:4]
list8[,3] <- SDlist8tog
list8[,4] <- shuffleObjList8[5:8]

list9[,1] <- DDlist9tog
list9[,2] <- shuffleObjList9[1:4]
list9[,3] <- SDlist9tog
list9[,4] <- shuffleObjList9[5:8]

list10[,1] <- DDlist10tog
list10[,2] <- shuffleObjList10[1:4]
list10[,3] <- SDlist10tog
list10[,4] <- shuffleObjList10[5:8]


############# walk/no walk --- same replace/diff replace

shuffle_walk_view1 <- c("walk/same", "walk/diff", "no walk/same", "no walk/diff", "walk/same", "walk/diff", "no walk/same", "no walk/diff")

# Group (walk, no walk) with (same, diff)
x <- as.data.frame(sample(c("walk", "no walk")))
y <- as.data.frame(sample(c("same", "diff")))

walk_view2 <- paste(x$`sample(c("walk", "no walk"))`,y$`sample(c("same", "diff"))`, sep = "/")

walk_view_list <- c(shuffle_walk_view1, walk_view2)

############## Save as a csv

sink(file = "outputs/ten_lists.csv")
write.csv(list1, row.names = FALSE)
cat("\n")
write.csv(list2, row.names = FALSE)
cat("\n")
write.csv(list3, row.names = FALSE)
cat("\n")
write.csv(list4, row.names = FALSE)
cat("\n")
write.csv(list5, row.names = FALSE)
cat("\n")
write.csv(list6, row.names = FALSE)
cat("\n")
write.csv(list7, row.names = FALSE)
cat("\n")
write.csv(list8, row.names = FALSE)
cat("\n")
write.csv(list9, row.names = FALSE)
cat("\n")
write.csv(list10, row.names = FALSE)
cat("\n")
write.csv(walk_view_list)
#cat("\n")
#write.csv(extraObject)
sink()




