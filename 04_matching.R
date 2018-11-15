install.packages("MatchIt")
install.packages("rowr")
library(MatchIt)
library(ggplot2)
library(rowr)

#Are we supposed to match only by  CDI status? In most examples of propensity matching, 
#they match many co-variats and exclude their outcome varibale
I have sen examples where they match by every IV

View(mtable4)
names(mtable4)

#Check for balance
p <- ggplot(mtable4, aes(fill = factor(cdi))) + 
  geom_bar(position = "dodge") + 
  scale_fill_discrete("cdi")
p1<- p + aes(x = dog)
p2<- p + aes(x = cat)
p3<- p + aes(x = antibiotics3mo)p1
p4<- p + aes(x = race)

#check the balance and overlap of the continuous w/  histogram

p5<-ggplot(mtable4, aes(x = age, fill = factor(cdi))) + 
  geom_histogram(position = "identity", alpha = 0.5) +
  scale_fill_discrete("cdi") 


#Match according to age, sex, 

m.out <- matchit(cdi ~ age + gender, data = mtable4)

#Did we improve balance? 
#Are the cdi+ and cdi- similar across age, gender?


s.out <- summary(m.out, standardize = TRUE)
plot(s.out) #Left =standard differences in means prior to matching
#right = diff in mean after matching

#Distribution of propensity scores (distance) before and after matching

plot(m.out,  type = "jitter", interactive = FALSE)
plot(m.out,  type = "hist")

summary(m.out)

#Try to generate matched
matched_table <- mtable4
get_matches(m.out, mtable4, id_cols = "age", newdata = matched_table)

View(matched_table)

m.out
class(m.out)

View(match.data(m.out))

testtable10 <- match.data(m.out, group="all", distance = "distance",
           weights = "weights", subclass = "subclass")

View(testtable10)

es.clogit <- clogit(cdi ~ gender + age + race + restaurant3tocc:education + strata(id), logan2)
names(mtable4)


#Poor Man's Match it

#CDI positive and male

mtable5 <- mtable4 %>% 
  filter(gender == "M", cdi == 1) %>% 
  arrange(age)

mtable5<- mtable5[-89,]

View(mtable5)


#CDI negative and male

mtable6 <- mtable4 %>% 
  filter(gender == "M", cdi == 0) %>% 
  arrange(age)

#Union

male_matched <- cbind(mtable5, mtable6)


View(male_matched) 

names(male_matched)

