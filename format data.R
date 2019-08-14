library(dplyr)
library()

data <- read.csv("data.csv")

#change mating column to 0 and 1
data$mating_edit <- ifelse(data$mating == "N" | data$mating == "NA" |data$mating == "ES" |data$mating =="O" |data$mating =="OVI" |data$mating =="V" |data$mating =="DEAD" |data$mating =="C" |data$mating =="dead?" |data$mating == "?", 0, 1)

#change activity column to 0 and 1
data$activity <- ifelse(data$behavior == "W" | data$behavior == "WF" | data$behavior == "WC"| data$behavior == "WES", 1, 0)


###Group activity and mating data by id so we have proportion of time mating and active for each individual in each trial

test <- data %>% group_by(date) %>% group_by(tank) %>% summarize(unique(id)) %>% ungroup


#add column for trial number based on date and treatment

test %>% 