
#Use "clean_table_strata" table from 04_Final_matching.R

install.packages("leaps")
install.packages("foreign")
install.packages("survival")
install.packages("moderndive")

library(leaps)
library(foreign)
library(survival)
library(moderndive)


#gender + age +race + restaurant3 +dessert3 +meat3 +salad3+redwine3 +dairy +
#vitamin + probiotic + antibiotics3mo + ppi +h2ra + acid_blocker_yes + health_care +hospital3 +
#hc_facility3 + adl_total + dog_allerg + cat_allerg + dog + dog_outside + cat + cat_outside        

names(clean_table_strata)

leaps=regsubsets(cdi_status~ gender + age + race + restaurant3 + dessert3 + meat3 + salad3+redwine3 +dairy +
                   vitamin + probiotic + antibiotics3mo + ppi +h2ra + acid_blocker_yes + health_care + hospital3 +
                   hc_facility3 + adl_total + dog + dog_outside + cat + cat_outside, data=clean_table_strata, nbest=10)

leaps=regsubsets(cdi_status~ hc_facility3 + adl_total + dog + dog_outside + cat + cat_outside, data=clean_table_strata, nbest=10)

plot(leaps, scale="adjr2")

#Use step to perfrom varaible selection

clean_table_strata1<- clean_table_strata %>% 
  select(-c(study_num, cdi))

null1=lm(cdi_status~1, data=clean_table_strata1)
null1

full1=lm(cdi_status~., data=clean_table_strata1)
full1

step(null1, scope=list(lower=null1, upper=full1), direction="forward")

#Best fit includes hc_facility3> probiotic>hospital3>antibiotics3mo>meat3>cat_allerg>restaurants3>salad3>h2ra>acid_bloker_yes
#>health_care>dessert3>dog_allergy>vitamin>dog_outside>dog>cat>age>ppi

#Conditional regression of 1:1 match of CDI positive patients and CDI negative patients according to sex and age
#Difference in Female age (in years) between matched pairs: Median 1.0, Min 0, Max 4.1
#Difference in Male age  (in years)  between matched pairs: Median 1.20, Min 0, Max 5.2
#dog interaction with dog_outside


res.clogit3.int <- survival::clogit(cdi_status ~ hc_facility3 + probiotic + hospital3 + adl_total +
                                      dog:dog_outside + strata(stratum), clean_table_strata1)

summary(res.clogit3.int)
#with dog interaction with dog_outside
res.clogit3.int1 <- survival::clogit(cdi_status ~ hc_facility3 + probiotic + hospital3 + adl_total +
                                      dog+ strata(stratum), clean_table_strata1)
summary(res.clogit3.int1)

#Linear Regression without conditional regression
lm1<-lm(cdi_status ~ hc_facility3 + age +probiotic + hospital3 + adl_total +
  dog:dog_outside, clean_table_strata1)

summary(lm1)
get_regression_table(lm1)
