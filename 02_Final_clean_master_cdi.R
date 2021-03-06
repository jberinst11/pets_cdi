#Import Data
#Downloaded from mbox on 11.14.2018 with MRNs, pairs, and additional tabs removed
#File name: "PETS SURVEY FINAL EXCEL 11.18.2018.xlsx


cdi1 <- read_excel("PETS SURVEY FINAL EXCEL 11.18.2018.xlsx")


View(cdi1)
names(cdi1)

#Keep the follow columns: study_num, gender, age, race

cdi2<- cdi1[, c(1,2,3,4,6)]
names(cdi2)
View(cdi2)
str(cdi2)
unique(cdi2$gender)

#Convert gender into factor F=1, M=2

cdi2$gender<-factor(cdi2$gender)

#Convert age into numeric
cdi2$age<-as.numeric(cdi2$age)

#Fix multiple spelling errors w/ race

unique(cdi2$race)
cdi4<- cdi2

cdi4$race[which(cdi4$race %in% c("white", "White", "White or Caucasian"))] = "white"
cdi4$race[which(cdi4$race %in% c("black", "Black or African American", "Black"))] = "black"
cdi4$race[which(cdi4$race %in% c("asian", "Asian", "Asia"))]  = "asian"
cdi4$race[which(cdi4$race %in% c("Unknown", "Unknwon", "unknown"))]  = "unknown"
cdi4$race[which(cdi4$race %in% c("other", "Other"))]  = "other"

#Convert Race into factor with 1=white, 2= black, 3= Hispanic
cdi5<-cdi4

cdi5$race<-factor(cdi5$race, levels = c("white", "black", "Hispanic", "asian", 
                                        "American Indian and Alaska Native", "other","unknown"))
levels(cdi5$race)
str(cdi5)

View(cdi5)

dim(cdi5) #896 patients contacted

#Catagorize patients as either  CDI+ and CDI-
#CDI positive = 1-500 and 2000-2999
#CDI negaitve = 501-1999 and 3000-3200
#Rename as cdi_status

cdi6 <- cdi5 %>% 
  mutate(cdi_status = ifelse(cdi5$study_num<500 | cdi5$study_num> 1999 & cdi5$study_num<3000, 1, 0))      

dim(cdi6)
group_no <-cdi6 %>% 
  group_by(cdi_status) %>% 
  dplyr::summarize(total=n())

cdi_total_by_cdi_status #435 CDI positive and #461 CDI negative contacted

duplicatestest <- cdi6 %>%
  group_by(study_num) %>% 
  filter(n() > 1)

#Combine tables using inner join
mtable1 <- merge(x = svy_monkey23, y =cdi6, by = "study_num")
dim(mtable1) #444 total remain(890-444=) =446 excluxed for not answering


#Remove people who were CDI status negative (i.e negative in EMR and called as a negaitve control, 
#however survey question demonstrated a past medical history of CDI)

str(mtable1)
View(mtable1)

duplicatestest <- mtable1 %>%
  group_by(study_num) %>% 
  filter(n() > 1)

mtable2 <-mtable1 %>%
  mutate(exclude =ifelse(mtable1$cdi_status ==0 & mtable1$cdi ==1, 1, 0))



#Remove excluded values therefore 27 removed and now with 389
mtable3 <- mtable2 %>% 
  filter(exclude==0)

dim(mtable3)
#206 CDI- and 208 CDI +
group_no1 <-mtable3 %>% 
  group_by(cdi_status) %>% 
  dplyr::summarize(total=n())

mtable4 <- mtable3[c(1,14,29, 26:28, 2:13, 15:24)]
dim(mtable4) #414 afer removal of the excluded cat
names(mtable4)

View(mtable4)
dim(mtable4)
#Remove patient's less than 18

mtable4.1<- mtable4 %>% 
  filter(age >= 18)
dim(mtable4.1) #Remove 3 for being <18


#Remove patient's less than 18
mtable5<- mtable4.1 %>% 
  filter(age <= 90)
dim(mtable5)

dim(mtable5) #Remove 1 for being>90
names(mtable5)

#Note comparison of CDI and cdi_status demonstrates that some CDI+ individuals did not
#report CDI however this was confirmed by chart view and CDI_status is more accurate!

svy_com_status_final <- mtable5 %>% 
  group_by(cdi_status) %>% 
  summarize(total=n())
svy_com_status_final





