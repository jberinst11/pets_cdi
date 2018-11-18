#Load Library

#Great 4 new tables(CDI pos/male, CDIneg/male, CDIpos/female, CDIneg/female)
#Arrange each table by age
#bind tables horrizontally so that you can build stratum of matched pairs
#Assess closeness in age
#Re-create a new table build vertically with the new coulm status added. 

#CDI positive and male arranged by age in ascending order

male_cdi_pos <- mtable5 %>% 
  filter(gender == "M", cdi_status == 1) %>% 
  arrange(age)

dim(male_cdi_pos) #96

#CDI negative and male arranged by age in ascending order

male_cdi_neg<- mtable5 %>% 
  filter(gender == "M", cdi_status == 0) %>% 
  arrange(age)

dim(male_cdi_neg) #96


#CDI positive and female arranged by age in ascending order

female_cdi_pos<- mtable5 %>% 
  filter(gender == "F", cdi_status == 1) %>% 
  arrange(age)
dim(female_cdi_pos) #109


#CDI negative and female arranged by age in ascending order
female_cdi_neg<- mtable5 %>% 
  filter(gender == "F", cdi_status == 0) %>% 
  arrange(age)
dim(female_cdi_neg) #109

#Male table
#Remove excess columns and keep only study_num, cdi_status, gener, age

male_cdi_pos1<- male_cdi_pos[c(1,3,4,5)]
names(male_cdi_pos1)

names(male_cdi_neg)
male_cdi_neg1<- male_cdi_neg[c(1,3,4,5)]


#Rename male_cdi_neg1 
colnames(male_cdi_neg1) <- c("study_num.1","cdi_status.1","gender.1","age.1")

#Join male_cdi_pos and male_cdi_neg

male_table<- cbind(male_cdi_pos1, male_cdi_neg1)

#Create new column with mutate called statum that contains the matched pair number 1:86
male_table1<- male_table %>% 
  mutate(statum =1:nrow(male_table))

#Evaluate diff in age
age_diff_table<- male_table1 %>% 
  mutate(age_diff =age-age.1)

#Keep only the study_num and startum column for CDI pos males
names(male_table1)
male_table_pos_strat<- male_table1[c(1,9)]

#Keep only the study_num and startum column for CDI neg males
male_table_neg_strat <- male_table1[c(5,9)]

#Rename "study_num.1" to study_num

colnames(male_table_neg_strat) <- c("study_num","statum")


#Left join to add starum to to male_cdi_pos

dim(male_cdi_pos)
male_cdi_pos_strat1 <- merge(male_cdi_pos, y = male_table_pos_strat, by = "study_num", all = TRUE)  
dim(male_cdi_pos_strat1)

#Left join to add starum to to male_cdi_neg

dim(male_cdi_neg)
male_cdi_neg_strat1<-merge(male_cdi_neg, y =male_table_neg_strat, by = "study_num", all = TRUE)
dim(male_cdi_neg_strat1)

####Master Male table
#Create a male table

total_male_table_status <-rbind(male_cdi_neg_strat1, male_cdi_pos_strat1)
dim(total_male_table_status)


#female tables
#Remove excess columns and keep only study_num, cdi_status, gener, age
female_cdi_pos1<- female_cdi_pos[c(1,3,4,5)] 
female_cdi_neg1<- female_cdi_neg[c(1,3,4,5)]


#Rename female_cdi_neg1 
colnames(female_cdi_neg1) <- c("study_num.1","cdi_status.1","gender.1","age.1")

#Join female_cdi_pos and female_cdi_neg

female_table<- cbind(female_cdi_pos1, female_cdi_neg1)


#Create new column with mutate called statum that contains the matched pair number 1:86
female_table1<- female_table %>% 
  mutate(statum =1:nrow(female_table))

#Create age diff eval
age_diff_table_female<- female_table1 %>% 
  mutate(age_diff =age-age.1)

#Keep only the study_num and startum column for CDI pos females
names(female_table1)
female_table_pos_strat<- female_table1[c(1,9)]

#Keep only the study_num and startum column for CDI neg females

female_table_neg_strat <- female_table1[c(5,9)]

#Rename "study_num.1" to study_num

colnames(female_table_neg_strat) <- c("study_num","statum")

#Left join to add starum to female_cdi_pos

dim(female_cdi_pos)
female_cdi_pos_strat2 <- merge(female_cdi_pos, y = female_table_pos_strat, by = "study_num", all = TRUE)    
dim(female_cdi_pos_strat2)

#Left join to add starum to to female_cdi_neg

dim(female_cdi_neg)
female_cdi_neg_strat2<-merge(female_cdi_neg, y =female_table_neg_strat, by = "study_num", all = TRUE)  

dim(female_cdi_neg_strat2)

####Master Male table
#Create a male table
total_female_table_status <-rbind(female_cdi_neg_strat2, female_cdi_pos_strat2)
dim(total_female_table_status)

#Combine male and female table vertically 

clean_table_stata<-rbind(total_female_table_status,total_male_table_status)

dim(clean_table_stata)
