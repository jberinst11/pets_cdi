install.packages("MatchIt")

library(MatchIt)
library(ggplot2)

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
p3<- p + aes(x = antibiotics3mo)
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


