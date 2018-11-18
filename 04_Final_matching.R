library(plyr)

mtable6 <- mtable5 %>%
  arrange(age, gender, cdi_status)

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
dim(female_cdi_pos) 108

#CDI negative and female arranged by age in ascending order
female_cdi_neg<- mtable5 %>% 
  filter(gender == "F", cdi_status == 0) %>% 
  arrange(age)
dim(female_cdi_neg) #108

#Male table
#Remove excess columns and keep only study_num, cdi_status, gener, age

male_cdi_pos1<- male_cdi_pos[c(1,3,4,5)]
names(male_cdi_pos1)

names(male_cdi_neg)
male_cdi_neg1<- male_cdi_neg[c(1,3,4,5)]

#Join male_cdi_pos and male_cdi_neg

male_table<- cbind(male_cdi_pos1, male_cdi_neg1)

#Create new column with mutate called statum that contains the matched pair number 1:86
male_table1<- male_table %>% 
  mutate(statum =1:nrow(male_table))

#Keep only the study_num and startum column for CDI pos males
names(male_table1)
male_table_pos_strat<- male_table1[c(1,9)]

#Keep only the study_num and startum column for CDI neg males

male_table_neg_strat <- male_table1[c(5,9)]

#Rename "study_num.1" to study_num

colnames(male_table_neg_strat) <- c("study_num","statum")


#Left join to add starum to to male_cdi_pos

dim(male_cdi_pos)
male_cdi_pos_strat2 <- merge(male_cdi_pos, y = male_table_pos_strat, by = "study_num", all = TRUE)  
dim(male_cdi_pos_strat)

#Left join to add starum to to male_cdi_neg

dim(male_cdi_neg)
male_cdi_neg_strat2<-merge(male_cdi_neg, y =male_table_neg_strat, by = "study_num", all = TRUE)
dim(male_cdi_pos_strat)

#female tables

female_cdi_pos1<- female_cdi_pos[c(1,3,4,5)]
female_cdi_neg1<- female_cdi_neg[c(1,3,4,5)]

#Join female_cdi_pos and female_cdi_neg

female_table<- cbind(female_cdi_pos1, female_cdi_neg1)


#Create new column with mutate called statum that contains the matched pair number 1:86
female_table1<- female_table %>% 
  mutate(statum =1:nrow(female_table))


#Keep only the study_num and startum column for CDI pos females
names(female_table1)
female_table_pos_strat<- female_table1[c(1,9)]

#Keep only the study_num and startum column for CDI neg females

female_table_neg_strat <- female_table1[c(5,9)]

#Rename "study_num.1" to study_num

colnames(female_table_neg_strat) <- c("study_num","statum")


#Left join to add starum to to male_cdi_pos

dim(female_cdi_pos)
female_cdi_pos_strat2 <- merge(female_cdi_pos, y = female_table_pos_strat, by = "study_num", all = TRUE)    
dim(female_cdi_pos_strat2)

#Left join to add starum to to male_cdi_neg

dim(female_cdi_neg)
female_cdi_neg_strat2<-merge(female_cdi_neg, y =female_table_neg_strat, by = "study_num", all = TRUE)  

dim(female_cdi_neg_strat2)

r.bind()
