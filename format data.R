library(dplyr)

data <- read.csv("data.csv")

#change mating column to 0 and 1
data$mating <- ifelse(data$mating == "N" | data$mating == "NA" |data$mating == "ES" |data$mating =="O" |data$mating =="OVI" |data$mating =="V" |data$mating =="DEAD" |data$mating =="C" |data$mating =="dead?" |data$mating == "?", 0, 1)

#change activity column to 0 and 1
data$activity <- ifelse(data$behavior == "W" | data$behavior == "WF" | data$behavior == "WC"| data$behavior == "WES", 1, 0)

#subset data to include only relevant columns
data <- subset(data, select = c(date, tank, id, sex, time, activity, mating, Treatment))




###Group activity and mating data by id so we have proportion of time mating and active for each individual in each trial
test <- data %>% group_by(date) %>% group_by(tank)
  
  
newDat <- with(data, split(data, list(date, tank, Treatment), drop = TRUE))
newDat$

ids <- lapply(1:length(newDate), function(z){
  
}
  
  
  
  
ids <- lapply(newDat, `[`, c('date', 'tank', 'sex', 'id', 'Treatment'))
id2 <- rbind_list(ids)
