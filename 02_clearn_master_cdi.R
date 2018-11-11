
#Import Data

cdi1 <- read_excel("PETS SURVEY FINAL EXCEL.xlsx")

View(cdi1)
names(cdi1)

#Keep study_num, gender, age, race

cdi2<- cdi1[, c(1,2,3,4, 6)]
names(cdi2)
View(cdi2)
str(cdi2)
unique(cdi2$gender)

#Convert gender into factor F=1, M=2

cdi2$gender<-factor(cdi2$gender)

#Convert age into numeric
cdi2$age<-as.numeric(cdi2$age)

#Convert Race into factor with 1=white, 2= black, 3= Hispanic
unique(cdi3$race)

cdi3<- cdi2


cdi4<-cdi3
cdi4$race[which(cdi4$race %in% c("white", "White", "White or Caucasian"))] = "white"
cdi4$race[which(cdi4$race %in% c("black", "Black or African American", "Black"))] = "black"
cdi4$race[which(cdi4$race %in% c("asian", "Asian", "Asia"))]  = "asian"
cdi4$race[which(cdi4$race %in% c("Unknown", "Unknwon", "unknown"))]  = "unknown"
cdi4$race[which(cdi4$race %in% c("other", "Other"))]  = "other"

cdi5<-cdi4
cdi5$race<-factor(cdi5$race, levels = c("white", "black", "Hispanic", "asian", "American Indian and Alaska Native", "other","unknown"))

View(cdi5)

dim(cdi5) #817 patients contacted


#Determine how many patients of each
#CDI positive = 1-500 and 2000-2058
#CDI negaitve = 501-1999 and 3000-3200
#Rename as cdi_status
cdi6 <- cdi5 %>% 
  mutate(cdi_status = ifelse(cdi5$study_num<500 | cdi5$study_num> 1999 & cdi5$study_num<3000, 1, 0))

cdi_total_by_cdi_status <- cdi6 %>% 
group_by(cdi_status) %>% 
  summarize(total=n())



cdi_total_by_cdi_status
#423 CDI positive and #394 CDI negative contacted

#Combine tables using inner join
mtable1 <- merge(x = svy_monkeyfinal, y =cdi6, by = "study_num")
dim(mtable1) #416 total remain(817-401=416) =401 excluxed 

#Confirm based on svy_com 

svy_com_status <- cdi6 %>% 
  group_by(svy_com) %>% 
  summarize(total=n())

svy_com_status #388 completed the survey, 429 did not complete the survey

#Remove people who were CDI status negative (i.e negative and EMR and called as a negaitve control, 
#however survey question revieled a past medical history of CDI)

str(mtable1)

mtable2 <-mtable1 %>%
  mutate(exclude =ifelse(mtable1$cdi_status ==0 & mtable1$cdi ==1, 1, 0))

#Rorder to cdi, cdi_status, exclude
View(mtable2)
names(mtable2)

mtable3 <- mtable2[c(1, 32:34, 2:13, 15:17, 24:31, 14, 35, 36)]
dim(mtable3) #416 before removal of the excluded cat
View(mtable3)

#Remove excluded values therefore 27 removed and now with 389
mtable4<- mtable3 %>% 
  filter(exclude==0)

dim(mtable4)

svy_com_status_final <- mtable4 %>% 
  group_by(cdi) %>% 
  summarize(total=n())
svy_com_status_final

names(mtable4)
