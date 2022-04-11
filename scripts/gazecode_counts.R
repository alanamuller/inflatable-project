
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
names(pairs)[1] <- "first"
names(pairs)[2] <- "second"
table(pairs)


    
