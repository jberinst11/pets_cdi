install.packages("survival")
install.packages("foreign")
install.packages("Hmisc")

library(survival)
library(foreign)
library(dplyr)
library(Hmisc)
      
#Use completed merged table from 02_Final_clean_master_cdi:mtable4

#Temporary remove study_num and cdi

mtable5<- mtable4 %>% 
  select(-c(study_num, cdi))

#Convert gender to numeric for correlation


mtable6 <- lapply(mtable5, as.numeric)

str(mtable6)

#Build Correlation matrix with all variables using pearson correlation



cor_matrix <- cor(as.matrix(mtable6), method = "pearson", use = "complete.obs")

res <- cor(mtable5)
round(res,2)

res2 <- rcorr(as.matrix(mtable6))
res2

