data <- read.csv("data.csv")


###change mating to binary
data$mating <- ifelse(data$mating == "N" | data$mating == "C" | data$mating == "DEAD" |data$mating == "Dead?" |data$mating == "?" |data$mating == "<NA>" | data$mating == "OVI" |data$mating == "v" |data$mating == "ES" |data$mating == "O" |data$mating == "V", "0", "1")


###change activity to binary
data$activity <- ifelse(data$activity == "1", "1", "0")


###change behavior to binary
data$behavior <- ifelse(data$behavior == "W" | data$behavior == "WC" | data$behavior == "WES" | data$behavior == "WF" , "1", "0")


###edit tank so that all tanks are numbers only


df = data

df = df %>% group_by(date, tank, time) %>% 
  mutate(uniq_ids = paste0(sort(unique(id)), collapse = "_")) %>% ungroup()

tanksdays_changingstriders = df %>% group_by(date, tank) %>% summarize(n = length(unique(uniq_ids))) %>% ungroup()
#the above dataframe contains the tank ids that contain a 
#different set of striders within the same day
#moving forward, we decided to eliminate these observations
#there are only four-days with changing group membership

tanksdays_changingstriders %>% filter(n > 1) #filter code filters the dataframe to those
#observations where n is greater than 1
#       date tank      n
# <int> <fct> <int>
# 1 20130502 18        2
# 2 20130505 D         2
# 3 20130507 T5        2
# 4 20130507 T9        2


#so, to eliminate these observations, use anti-join
#removes observations from df which are present in tanksdays...
df_mod = tanksdays_changingstriders %>% filter(n > 1) %>%
  select(-n) %>% #this line removes the n column, which we no longer need
  anti_join(df, ., by = c("date", "tank"))

(nrow(df) - nrow(df_mod))/nrow(df)
#we lose 0.86% of the data

#df_mod contains observations where group membership does not change on a single day
#so now, let's calculate the unique groups labels, which is the unique combination
#of uniq_id and tank id
df_mod$groupID = paste0("group", df_mod$tank, " ", df_mod$uniq_ids)


#now add a trial label to each of the different groups, where trial is just the day
#that that group is being assayed. We don't technically need this, since we already
#have the day information, but it's probably useful since we want to think about
#trial 1 across different groups, even if that trial 1 occurred on different days for
#those groups
#to do this, I will group by our new "groupID" variable, and then sort the data
#within that grouping variable by "date", 'time', and 'id' so that when we add 
#a new trial column according to date, we can be sure that the earliest date corresponds
#to the earliest trial
#this probably isn't necessary, assuming the data are already correctly sorted, but on the
#off chance that for some data the values are not entered chronologically, this code
#will prevent that from being an issue
#note that in the arrange code, the .by_group argument ensures that the data are arranged
#within their grouping category (rather than as a whole, regardless of grouping)
df_mod = df_mod %>% group_by(groupID) %>% arrange(date, time, id, .by_group = T) %>%
  mutate(trial = as.integer(as.factor(date))) %>% ungroup()
#the as.factor and as.integer code ensures that for different groups with different
#first dates, that first date gets a trial value of 1 for both groups

#we have found that sex and treatment are problematic because there are multiple
#entries of sex and treatment for some groupID-trial-id combinations
test = df_mod %>% group_by(groupID, trial, id) %>%
  summarize( n_treatment = length(unique(Treatment)), 
             n_sex = length(unique(sex))) %>% ungroup()


#fix issues with tanks who were assigned large treatment even though they are small 
df_mod$Treatment <- as.character(df_mod$Treatment)
df_mod$Treatment <- ifelse(df_mod$groupID == "groupT20 144_157-G_160-R_161-B_39_97", "small", df_mod$Treatment)

#fix sex for individual 145-Y who was assigned male for one entry
df_mod$sex <- as.character(df_mod$sex)
df_mod$sex <- ifelse(df_mod$id == "145-Y" | df_mod$id == "98-Y", "F", df_mod$sex)


###exclude striders who have multiple entries
test = df_mod %>% group_by(date, id) %>%
  summarize( n_treatment = length(unique(Treatment)), 
             n_sex = length(unique(sex)), n_tank = length(unique(tank))) %>% ungroup()

df_mod = test %>% filter(n_treatment > 1) %>%
  select(-n_sex, -n_treatment, -n_tank) %>% #this line removes the n column, which we no longer need
  anti_join(df_mod, ., by = c("date", "id"))

df_mod = test %>% filter(n_tank > 1) %>%
  select(-n_sex, -n_treatment, -n_tank) %>% #this line removes the n column, which we no longer need
  anti_join(df_mod, ., by = c("date", "id"))

#and so now we want to calculate the proportion of time active and mating, within a day,
#for each male, for each of its observed trials, for each of its observed groups
#but remember that trial is the same thing as day
df_mod$behavior <- as.numeric(df_mod$behavior)
df_mod$mating <- as.numeric(df_mod$mating)

bloop = df_mod %>% group_by(date, id) %>% 
  summarize(prop_behavior = sum(behavior, na.rm = T)/sum(!is.na(behavior)),
            prop_mating = sum(mating, na.rm = T)/sum(!is.na(mating)),
            sex = unique(sex),
            Treatment = unique(Treatment),
            tank = unique(tank)) %>% ungroup()

#now, to filter to males ###saving this for later because our data frame is messed up!
df_mod = df_mod %>% filter(sex == "m")

save(df_mod, file = "./df_mod.rdata")
load(file = "./df_mod.rdata")
