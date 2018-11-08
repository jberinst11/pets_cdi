#Note that data are downloaded from SurveyMonkey
#Using Analyze Results, Exports, All individual responses
# XLS+, Original View
# then open Excel folder, PETS SURVEY FINAL EXCEL.xlsx, dl 11.6.2018

#Install Latest Janitor Package with Github
devtools::install_github("sfirke/janitor")
install.packages("devtools")

#load libraries
library(rio)
library(janitor) #note using github version
library(dplyr)
library(visdat)
library(naniar)
library(plotly)
library(readxl)
library(devtools)
library(tidyr)


#Import master_survey and svy_monkey

master_cdi1 <- read_excel("PETS SURVEY FINAL EXCEL.xlsx")
svy_monkey1 <- read_excel("Clostridium difficile Pets Survey.xlsx")


#remove unneeded rows and columns

svy_monkey2<- svy_monkey1  %>%
  clean_names() %>% 
  remove_empty(c("rows", "cols"))

#remove unwanted columns and remove Nas

svy_monkey3 <-svy_monkey2 %>% 
  subset(select = -c(respondent_id, collector_id, start_date, end_date, ip_address, 
                     i_would_like_to_continue_and_complete_this_22_question_survey))

dim(svy_monkey3)
svy_monkey4 <- svy_monkey3 %>% 
  filter(!is.na(study_id_number))
dim(svy_monkey4)

#rename ugly columns
svy_monkey5 <- svy_monkey4 %>%  
  rename(study_num = study_id_number) %>% 
  rename(restaurant3 = do_you_eat_at_restaurants_more_than_3_times_per_week) %>% 
  rename(dessert3 = do_you_eat_dessert_more_than_3_times_per_week) %>% 
  rename(meat3 = do_you_eat_meat_more_than_3_times_per_week) %>% 
  rename(salad3 = do_you_eat_salads_more_than_3_times_per_week) %>% 
  rename(redwine3 = do_you_drink_red_wine_more_than_3_times_per_week) %>% 
  rename(dairy = how_many_servings_of_dairy_do_you_consume_each_week_1_serving_8oz_milk_this_includes_cheeses_and_yogurt_but_not_butter) %>% 
  rename(vitamin = do_you_take_either_a_multivitamin_daily_or_vitamin_d_supplements_daily) %>% 
  rename(probiotic = do_you_take_probiotics_daily) %>% 
  rename(antibiotic = did_you_use_any_antibiotics_in_the_three_months_before_or_prior_to_or_at_the_time_of_your_c_diff_test_test_date_in_letter_if_no_enter_no_if_yes_please_enter_yes_followed_by_the_name_of_the_antibiotic_i_e_yes_keflex_or_if_unknown_yes_unknown) %>% 
  rename(omeprazole = did_you_use_any_of_the_following_acid_blocking_medications_in_the_following_4_weeks_prior_to_or_at_the_time_of_your_c_diff_test_test_date_in_letter) %>% 
  rename(esomeprazole = x_1) %>%
  rename(prevacid = x_2) %>%
  rename(dexlansoprazole = x_3) %>% 
  rename(rabeprazole = x_4) %>%
  rename(pantoprazole = x_5) %>% 
  rename(famotidine = x_6) %>%
  rename(ranitidine = x_7) %>% 
  rename(cimetidine = x_8) %>% 
  rename(nizatidine = x_9) %>% 
  rename(acid_blocker = x_10) %>%
  rename(cdi = have_you_ever_in_your_life_tested_positive_for_an_intestinal_infection_called_clostridium_difficile_c_diff_that_required_antibiotics) %>% 
  rename(health_care = does_anyone_who_lives_in_your_household_work_in_a_health_care_setting_where_there_are_patients_being_treated_for_illnesses_hospital_clinic_etc) %>% 
  rename(hospital3 = were_you_admitted_to_a_hospital_in_the_three_months_prior_to_at_time_of_c_diff_test_test_date_in_letter) %>% 
  rename(hc_facility3 = did_you_live_in_a_health_care_facility_nursing_home_rehabilitation_center_prior_to_or_at_the_time_of_your_c_diff_test_test_date_in_letter) %>% 
  rename(feeding_adl = prior_to_your_c_diff_test_on_supply_date_if_not_known_did_you_have_difficulty_or_require_assistance_in_daily_activity_such_as) %>% 
  rename(walking_adl = x_11) %>% 
  rename(transfer_adl = x_12) %>% 
  rename(dressing_adl = x_13) %>% 
  rename(grooming_adl = x_14) %>% 
  rename(toileting_adl = x_15) %>% 
  rename(dog_allerg = are_you_allergic_to_dogs) %>% 
  rename(cat_allerg = are_you_allergic_to_cats) %>% 
  rename(dog = do_you_have_a_dog_that_sleeps_in_your_house_each_night) %>% 
  rename(dog_outside = does_your_dog_go_outside_each_day) %>% 
  rename(cat = do_you_have_a_cat_that_sleeps_in_your_house_each_night) %>% 
  rename(cat_outside = does_your_cat_go_outside_each_day)  

#Remove Row 1

svy_monkey6 <- svy_monkey5[-1,]

#Visual Representation of missing Data  
vis_dat(svy_monkey6)
vis_miss(svy_monkey6)

#Remove observations with missing data. Remove 260, 574, 158
#Andrew Help
#attach(svy_monkey6)
#svy_monkey7 <- svy_monkey6[which(study_num == -c("260","574","158")),]
#detach(svy_monkey6)  

#Mutate to add additional Column antibiotics_yes_no
svy_monkey7<- svy_monkey6
str(svy_monkey7)
svy_monkey8 <- mutate(svy_monkey7, antibiotics = ifelse(grepl("No|NO|N0|no|entocort", svy_monkey7$antibiotic), "No", "Yes"))

#Reorder Columns so that Antibiotics next to Ab_no 
names(svy_monkey8)

svy_monkey9 <- svy_monkey8[c(1:9,10, 38, 11:37)] 

# note that some yes = entocort, zofran
#This was personally verified to ensure it transferred

#Gather all the PPI

svy_monkey10 <-gather(svy_monkey9,type,value,omeprazole:pantoprazole)
svy_monkey11 <- mutate(svy_monkey10, ppi = ifelse(is.na(svy_monkey10$value), "No", "Yes"))

#Gather all the H2RA
names(svy_monkey11)
svy_monkey12 <-gather(svy_monkey11,type1,value1,famotidine:nizatidine)
svy_monkey13 <- mutate(svy_monkey12, h2ra = ifelse(is.na(svy_monkey12$value1), "No", "Yes"))

#Convert "I Have not uced acid blockers"

svy_monkey14 <- mutate(svy_monkey13, acid_blocker_yes  = ifelse(grepl("blocking", acid_blocker), "no", "yes"))

names(svy_monkey14)

#Remove columns: "antibiotic",type", "value", "type1","value1"  
svy_monkey15 <- svy_monkey14[,-c(10,29,30,32,33)] 

#Change adls to factors where Independent =1, Some Assistance = 2, Full Assistance =3
str(svy_monkey15)

str()
svy_monkey16$feeding_adl <-as.numeric(factor(svy_monkey15$feeding_adl, levels = c("Independent", "Some Assistance", "Full Assistance"), ordered =TRUE))
svy_monkey16$walking_adl <-as.numeric(factor(svy_monkey15$walking_adl, levels = c("Independent", "Some Assistance", "Full Assistance"), ordered =TRUE))
svy_monkey16$transfer_adl <-as.numeric(factor(svy_monkey15$transfer_adl, levels = c("Independent", "Some Assistance", "Full Assistance"), ordered =TRUE)) 
svy_monkey16$dressing_adl<-as.numeric(factor(svy_monkey15$dressing_adl, levels = c("Independent", "Some Assistance", "Full Assistance"), ordered =TRUE)) 
svy_monkey16$grooming_adl  <-as.numeric(factor(svy_monkey15$grooming_adl , levels = c("Independent", "Some Assistance", "Full Assistance"), ordered =TRUE))
svy_monkey16$toileting_adl <-as.numeric(factor(svy_monkey15$toileting_adl, levels = c("Independent", "Some Assistance", "Full Assistance"), ordered =TRUE)) 


str(svy_monkey16)
names(svy_monkey15)

#Replace ADLs with #

svy_monkey17 <- svy_monkey16 %>% 
  mutate(adl_total = feeding_adl + walking_adl + transfer_adl + dressing_adl + grooming_adl + toileting_adl)

#Reorder so that adl_total is beside other adls

names(svy_monkey17)

#Reorder
svy_monkey18 <- svy_monkey17[c(1:10, 28:30, 12:15, 31, 22:27)] 
names(svy_monkey18)


#Create all columns as factors then conver to numeric

svy_monkey18$restaurant3 <-ifelse(svy_monkey18$restaurant3=="Yes",1,0)
svy_monkey18$dessert3 <-ifelse(svy_monkey18$dessert3=="Yes",1,0)
svy_monkey18$meat3 <-ifelse(svy_monkey18$meat3=="Yes",1,0)
svy_monkey18$salad3 <-ifelse(svy_monkey18$salad3=="Yes",1,0)
svy_monkey18$redwine3 <-ifelse(svy_monkey18$redwine3=="Yes",1,0)
svy_monkey18$vitamin <-ifelse(svy_monkey18$vitamin=="Yes",1,0)
svy_monkey18$probiotic <-ifelse(svy_monkey18$probiotic=="Yes",1,0)
svy_monkey18$antibiotic <-ifelse(svy_monkey18$antibiotic=="Yes",1,0)
svy_monkey18$cdi <-ifelse(svy_monkey18$cdi=="Yes",1,0)
svy_monkey18$health_care <-ifelse(svy_monkey18$health_care=="Yes",1,0)
svy_monkey18$hospital3 <-ifelse(svy_monkey18$hospital3=="Yes",1,0)
svy_monkey18$hc_facility3 <-ifelse(svy_monkey18$hc_facility3=="Yes",1,0)
svy_monkey18$dog_allerg <-ifelse(svy_monkey18$dog_allerg =="Yes",1,0)
svy_monkey18$cat_allerg <-ifelse(svy_monkey18$cat_allerg=="Yes",1,0)
svy_monkey18$dog <-ifelse(svy_monkey18$dog=="Yes",1,0)
svy_monkey18$dog_outside <-ifelse(svy_monkey18$dog_outside=="Yes",1,0)
svy_monkey18$cat <-ifelse(svy_monkey18$cat=="Yes",1,0)
svy_monkey18$cat_outside <-ifelse(svy_monkey18$cat_outside=="Yes",1,0)
svy_monkey18$ppi <-ifelse(svy_monkey18$ppi=="Yes",1,0)
svy_monkey18$h2ra <-ifelse(svy_monkey18$h2ra=="Yes",1,0)
svy_monkey18$acid_blocker_yes <-ifelse(svy_monkey18$acid_blocker_yes=="Yes",1,0)
svy_monkey18$dairy <- as.numeric(svy_monkey18$dairy)

str(svy_monkey18)
View(svy_monkey18)


#svy_monkey16$adl_score[svy_monkey16$feeding_adl == "Independent"] <- pets$adl_score[svy_monkey16$feeding_adl == "Independent"]+2

#pets$adl_score[pets$adl_feed == "Some Assistance"] <- pets$adl_score[pets$adl_feed == "Some Assistance"]+1

#pets$adl_score[pets$adl_walk == "Independent"] <- pets$adl_score[pets$adl_walk == "Independent"]+2
pets$adl_score[pets$adl_walk == "Some Assistance"] <- pets$adl_score[pets$adl_walk == "Some Assistance"]+1

pets$adl_score[pets$adl_transfer == "Independent"] <- pets$adl_score[pets$adl_transfer == "Independent"]+2
pets$adl_score[pets$adl_transfer == "Some Assistance"] <- pets$adl_score[pets$adl_transfer == "Some Assistance"]+1

pets$adl_score[pets$adl_dress == "Independent"] <- pets$adl_score[pets$adl_dress == "Independent"]+2
pets$adl_score[pets$adl_dress == "Some Assistance"] <- pets$adl_score[pets$adl_dress == "Some Assistance"]+1

pets$adl_score[pets$adl_groom == "Independent"] <- pets$adl_score[pets$adl_groom == "Independent"]+2
pets$adl_score[pets$adl_groom == "Some Assistance"] <- pets$adl_score[pets$adl_groom == "Some Assistance"]+1

pets$adl_score[pets$adl_bathroom == "Independent"] <- pets$adl_score[pets$adl_bathroom == "Independent"]+2
pets$adl_score[pets$adl_bathroom == "Some Assistance"] <- pets$adl_score[pets$adl_bathroom == "Some Assistance"]+1


svy_monkey15$adl_score <- 0
###########
svy_monkey10 <- svy_monkey9 %>% 
mutate(ppi = ("omeprazole" + "pantoprazole"))

svy_monkey10 <-gather(svy_monkey9,type,value,omeprazole:pantoprazole)
svy_monkey11 <- svy_monkey9 %>% 
  mutate(ppi= svy_monkey10$value)
         
svy_monkey11 <- svy_monkey10[c(1:16, 35, 17:33)] 
names(svy_monkey10)


svy_monkey10 <- mutate(svy_monkey9,PPI_yes = ifelse(grepl("omep|esom|prevacid|dexlansoprazole|rabeprazole|pantoprazole", svy_monkey9$ppi), "yes", "no"))

svy_monkey10 <- gather(svy_monkey9, key="ppi", value = "omeprazole")



svy_monkey10 <- mutate(svy_monkey9,PPI_yes = ifelse(grepl("omeprazole|esomeprazole|prevacid|dexlansoprazole|rabeprazole|pantoprazole", svy_monkey7$), "No", "Yes"))

names(svy_monkey9)




svy_monkey7$antibiotic[grepl("yes", svy_monkey7$antibiotic_exp, ignore.case = TRUE)] <- "Yes"

pets$abx[grepl("no", pets$antibiotic_exp, ignore.case = TRUE)] <- "No"
pets$abx[c(19,21,28, 33,36,47,52, 62, 63,65,66,72,73,76)] <- "Yes"
pets$abx[c(16,37, 46,74,75)] <- "No"

str(svy_monkey6)

##################

#fix acid_blocker variable
#sum(is.na(svy_monkey6$acid_blocker)) #59 missing
#fix up one with multiple acid blockers
#pets$x_7[pets$study_id == "106"] <- NA #eliminates famotidine, keeps PPI

#now start filling in acid_blocker
#pets$acid_blocker[is.na(pets$acid_blocker)] <- pets$x_1[is.na(pets$acid_blocker)] #now 57
#pets$acid_blocker[is.na(pets$acid_blocker)] <- pets$x_2[is.na(pets$acid_blocker)] #now 56
#pets$acid_blocker[is.na(pets$acid_blocker)] <- pets$x_3[is.na(pets$acid_blocker)]
#pets$acid_blocker[is.na(pets$acid_blocker)] <- pets$x_4[is.na(pets$acid_blocker)] #now 55
#pets$acid_blocker[is.na(pets$acid_blocker)] <- pets$x_5[is.na(pets$acid_blocker)] #now 54
#pets$acid_blocker[is.na(pets$acid_blocker)] <- pets$x_6[is.na(pets$acid_blocker)] #now 49
#pets$acid_blocker[is.na(pets$acid_blocker)] <- pets$x_7[is.na(pets$acid_blocker)] #now 46
#pets$acid_blocker[is.na(pets$acid_blocker)] <- pets$x_8[is.na(pets$acid_blocker)]
#pets$acid_blocker[is.na(pets$acid_blocker)] <- pets$x_9[is.na(pets$acid_blocker)]
#pets$acid_blocker[is.na(pets$acid_blocker)] <- "None"
#now none missing
#tabyl(pets$acid_blocker)

#now drop x_1 to x_9
#pets <- pets %>% select(-(x_1:x_10))



# fix antibiotic exposure
#ideally edit each entry to include a clear (and correct) Yes or No,
# edit out any inadvertent "no" or "not"
# note that some yes = entocort, zofran
pets$abx[grepl("yes", pets$antibiotic_exp, ignore.case = TRUE)] <- "Yes"
pets$abx[grepl("no", pets$antibiotic_exp, ignore.case = TRUE)] <- "No"
pets$abx[c(19,21,28, 33,36,47,52, 62, 63,65,66,72,73,76)] <- "Yes"
pets$abx[c(16,37, 46,74,75)] <- "No"


# fix dairy entries
#pets$dairy[c(21)] <- "6.5"
#pets$dairy[c(22)] <- "5.5"
#pets$dairy[c(23)] <- "17.5"
#pets$dairy[c(24)] <- "10.5"
#pets$dairy[c(25)] <- "8.5"
#pets$dairy[c(46)] <- "0"
#pets$dairy[c(47)] <- "2.5"
#pets$dairy[c(54)] <- "8"
#pets$dairy[c(63)] <- "3"
#pets$dairy[c(64)] <- "3"
#pets$dairy[c(67)] <- "5" # for not sure
#pets$dairy[c(71)] <- "25" # for >20
#pets$dairy[c(72)] <- "8"
#pets$dairy[c(73)] <- "9"
#pets$dairy[c(75)] <- "2"
#pets$dairy[c(76)] <- "24"
#pets$dairy[c(77)] <- "11" #for 10+

#pets$dairy <- as.integer(pets$dairy)

# check, fix cdi Yes/no


# fix adl variables - sum up
pets <- pets %>%  
  rename(adl_feed = adl) %>% 
  rename(adl_walk = x_11) %>% 
  rename(adl_transfer = x_12) %>% 
  rename(adl_dress = x_13) %>% 
  rename(adl_groom = x_14) %>% 
  rename(adl_bathroom = x_15)

#fix one case with missing values
pets$adl_feed[pets$study_id == 260] <- "Some Assistance"
pets$adl_walk[pets$study_id == 260] <- "Some Assistance"
pets$adl_transfer[pets$study_id == 260] <- "Some Assistance"
pets$adl_groom[pets$study_id == 260] <- "Some Assistance"
pets$adl_bathroom[pets$study_id == 260] <- "Some Assistance"


#create adl summary score
pets$adl_score <- 0
pets$adl_score[pets$adl_feed == "Independent"] <- pets$adl_score[pets$adl_feed == "Independent"]+2
pets$adl_score[pets$adl_feed == "Some Assistance"] <- pets$adl_score[pets$adl_feed == "Some Assistance"]+1

pets$adl_score[pets$adl_walk == "Independent"] <- pets$adl_score[pets$adl_walk == "Independent"]+2
pets$adl_score[pets$adl_walk == "Some Assistance"] <- pets$adl_score[pets$adl_walk == "Some Assistance"]+1

pets$adl_score[pets$adl_transfer == "Independent"] <- pets$adl_score[pets$adl_transfer == "Independent"]+2
pets$adl_score[pets$adl_transfer == "Some Assistance"] <- pets$adl_score[pets$adl_transfer == "Some Assistance"]+1

pets$adl_score[pets$adl_dress == "Independent"] <- pets$adl_score[pets$adl_dress == "Independent"]+2
pets$adl_score[pets$adl_dress == "Some Assistance"] <- pets$adl_score[pets$adl_dress == "Some Assistance"]+1

pets$adl_score[pets$adl_groom == "Independent"] <- pets$adl_score[pets$adl_groom == "Independent"]+2
pets$adl_score[pets$adl_groom == "Some Assistance"] <- pets$adl_score[pets$adl_groom == "Some Assistance"]+1

pets$adl_score[pets$adl_bathroom == "Independent"] <- pets$adl_score[pets$adl_bathroom == "Independent"]+2
pets$adl_score[pets$adl_bathroom == "Some Assistance"] <- pets$adl_score[pets$adl_bathroom == "Some Assistance"]+1


#fix some missing animal data
#fix dog_outside not applicable in 20, 36, 67, 77
pets$dog_outside[c(20,36,67,77)] <- "Not Applicable"
#fix cat_outside not applicable in 20
pets$cat_outside[20] <- "Not Applicable"

# identify, fix missing data esp at end of survey
vis_dat(pets)
vis_miss(pets)
#pets %>% select(starts_with("x")) %>% vis_miss() %>% ggplotly()
vis_miss(pets) %>% ggplotly() #interactive - a bit slow - to be replaced by vis_miss_ly
gg_miss_var(pets)
miss_case_table(pets)
miss_case_summary(pets)
miss_var_summary(pets)

# still missing- all pet Q for obs 30, study_id 169
# still missing cat Q for obs 77, study_id 108


#convert a lot of character variables to factors
# convert some to integers study_id

