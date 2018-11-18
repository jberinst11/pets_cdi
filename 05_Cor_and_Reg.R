install.packages("survival")
install.packages("foreign")
install.packages("Hmisc")

library(survival)
library(foreign)
library(dplyr)
library(Hmisc)
      
#Use "clean_table_stata" table from 04_Final_matching.R


#Temporary remove study_num and cdi

clean_table_strata1<- clean_table_strata %>% 
  select(-c(study_num, cdi))

str(clean_table_stata1)

#Build Correlation matrix with all variables using pearson correlation

clean_table_stata1<- lapply(clean_table_stata1, as.numeric)

cor_matrix <- cor(as.matrix(clean_table_stata1), method = "pearson", use = "complete.obs")

res <- cor(clean_table_stata1)
round(res,2)

res2 <- rcorr(as.matrix(mtable6))
res2

#Convert gender and race to numeric for correlation



mtable6 <- lapply(mtable5, as.numeric)

str(mtable6)

