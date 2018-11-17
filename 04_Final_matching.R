
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

