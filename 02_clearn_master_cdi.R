
#Import Data

cdi1 <- read_excel("PETS SURVEY FINAL EXCEL.xlsx")

View(cdi1)
names(cdi1)

#Keep study_num, gender, age, race

cdi2<- cdi1[, c(1,3,4, 6)]
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

str(cdi5)
