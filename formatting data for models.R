library(dplyr)
data <- read.csv("data1.csv")


###change mating to binary
data$mating <- ifelse(data$mating == "N" | data$mating == "C" | data$mating == "DEAD" |data$mating == "Dead?" |data$mating == "?" |data$mating == "<NA>" | data$mating == "OVI" |data$mating == "v" |data$mating == "ES" |data$mating == "O" |data$mating == "V", "0", "1")


###change activity to binary
data$activity <- ifelse(data$activity == "1", "1", "0")


###change behavior to binary
data$behavior <- ifelse(data$behavior == "W" | data$behavior == "WC" | data$behavior == "WES" | data$behavior == "WF" , "1", "0")


###Create data frame with mating score, behavior score, id, trial, treatment type and sex

#vectors have to be numeric in order to sum
data$behavior <- as.numeric(data$behavior)
data$mating <- as.numeric(data$mating)

#create new data frame with summed behavior for each individual in each trial
df <- data %>% group_by(id, period) %>% na.omit(.) %>% summarize(sum(behavior)) %>% ungroup

#sum mating for each individual in each trial
mating <- data %>% group_by(id) %>% na.omit(.) %>% summarize(sum(mating)) %>% ungroup
#add mating to behavior data frame
df <- merge(x = df, y = mating[ , c("id", "sum(mating)")], by = "id", all.x=TRUE)

#data frame with sex
#first check that each individual has only one entry for sex
sex <- data %>% group_by(id) %>% summarize(length(unique(sex))) %>% ungroup #some of them have two so I corrected the entries in excel
#no that it's corrected make a sex data frame to merge
sex <- data %>% group_by(id) %>% summarize(unique(sex)) %>% ungroup
#add sex to behavior data frame
df <- merge(x = df, y = sex, by = "id")

#data frame with treatment
#first check that each individual has only one treatment entry for each trial period
treatment <- data %>% group_by(id, period) %>% summarize(length(unique(small))) %>% ungroup #lots of individual with the more than one treatment type for a period so fix that (in excel)
treatment <- data %>% group_by(id, period) %>% summarize(unique(small)) %>% ungroup


###To be continued


