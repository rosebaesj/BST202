### BST 203


library(ggplot2)
library(haven) 
library(dplyr)
library(tidyverse)
library(rstatix)


###Chapter 12 Exercise 10  #######
cad <- read_dta("inputs/cad.dta")

ggplot(cad, aes(x=age))+
  geom_histogram(color="black", fill="grey", bins = 10)+
  theme_classic()+
  scale_y_continuous(expand = c(0,0),limits = c(0,20))

ggplot() +
  geom_histogram(aes(x = age),bins = 7, data = cad) +
  facet_wrap(~center) +
  theme_classic()+
  scale_y_continuous(expand = c(0,0),limits = c(0,10))

# ggplot(cad, aes(x=))+
#   geom_bar(color="black", fill="grey", bins = 10)+
#   theme_classic()+
#   scale_y_continuous(expand = c(0,0),limits = c(0,20))


boxplot(data = cad, age ~ center)

sum <- cad %>% group_by(center) %>%
  summarize(sample_size=length(age), 
            mean = mean(age),
            sd = sd(age)) 

cad$center <- as.factor(cad$center)

summary(aov(age ~ center, data=cad))
summary(aov(center~age, data=cad))



###Chapter 12 Exercise 11  #######
lowbwt <- read_dta("inputs/lowbwt.dta")


lowbwt %>% group_by(sex) %>%
  summarize(sample_size=length(sbp), 
            mean = mean(sbp),
            sd = sd(sbp)) 



(((56-1)*11.1^2+(44-1) *11.8^2))/(56+44-2)
((46.5-47.9)-(0))/sqrt(130.2436*(1/56+1/44))

pt(-0.608934, 98 )*2

lowbwt$center <- as.factor(lowbwt$center)

summary(aov(sbp ~ sex, data=lowbwt))

###Chapter 13 Exercise 6  #######

REE <- read_table("inputs/REE.txt")

REE$difference <-REE$CF-REE$Healthy
wilcox.test(x=REE$difference)
wilcox.test(x=REE$difference, exact=F, correct = F)


wilcox_test(data=REE$difference, formula = )
wilcox_test(data= REE, group1=CFHealthy, paired=TRUE)


sqrt((13*(13+1)*(2*13+1))/24)

(7- 45.5)/14.30909


pnorm(-2.690597)*2



###Chapter 13 Exercise 12  #######
insurance <- read_dta("inputs/insurance.dta")


sum(insurance$group ==1)
sum(insurance$group ==0)

insurance$rankinv <- rank(-insurance$stage, ties.method = "average")





sum(insurance$rank[which(insurance$group==1)])
sum(insurance$rankinv[which(insurance$group==1)])



54103/82

(82*(82+969+1))/2

sqrt(((82*969*(82+969+1))/12))

(54103- 43132)/2639.284

(1-pnorm(4.156809 ))*2



wilcox.test(stage ~ group, data = insurance, 
            #alternative = "two.sided",
            #exact=F, 
            correct = T,
            ties.method = "average"
            )


###Chapter 13 Exercise 13  #######
nursing_home <- read_dta("inputs/nursing_home.dta")

boxplot(data = nursing_home, occupancy ~ region)

k <- kruskal.test(data = nursing_home, occupancy ~ region)



sum(insurance$group ==1)
sum(insurance$group ==0)

insurance$rankinv <- rank(-insurance$stage, ties.method = "average")





sum(insurance$rank[which(insurance$group==1)])
sum(insurance$rankinv[which(insurance$group==1)])



54103/82

(82*(82+969+1))/2

sqrt(((82*969*(82+969+1))/12))

(54103- 43132)/2639.284

(1-pnorm(4.156809 ))*2



wilcox.test(stage ~ group, data = insurance, 
            #alternative = "two.sided",
            #exact=F, 
            correct = T,
            ties.method = "average"
)


