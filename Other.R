library(ggplot2)
library(haven) 
library(dplyr)
library(tidyverse)


##### HW_04
# Table 4.2

life_table_2016 <- read_dta("inputs/life_table_2016.dta")
#lx is proprotion

age <- c(50, 51, 52, 53, 54, 55)
number <- c(346, 335, 324, 313, 302, 292)

hw04_2 <- data.frame(cbind(age, number))
hw04_2[,1] <- as.numeric(hw04_2[,1])
hw04_2[,2] <- as.numeric(hw04_2[,2])

hw04_2$percentdead <- (346-hw04_2$number)/346
sum(hw04_2$percentdead)
sum(c(346, 335, 324, 313, 302))

age2 <- c(50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60)
number2 <- c(346, 335, 324, 313, 302, 292, 282, 272, 262, 252, 242)
sum(c(346, 335, 324, 313, 302, 292, 282, 272, 262, 252))

hw04_22 <- data.frame(cbind(age2, number2))
hw04_22[,1] <- as.numeric(hw04_22[,1])
hw04_22[,2] <- as.numeric(hw04_22[,2])

96361/98847
77751/96361
24735/77751
hw04_22$percentdead <- (346-hw04_22$number2)/346
sum(hw04_22$percentdead)


life_table_2016_50_55 <- life_table_2016[50:55,]
life_table_2016_50_55$percentdead <- (94200-life_table_2016_50_55$lx)/94200
sum(life_table_2016_50_55$percentdead)


(life_table_2016[50,]$lx - life_table_2016[55,]$lx)/sum(life_table_2016[50:54,]$lx)

life_table_2016_51_60 <- life_table_2016[51:60,]
life_table_2016_51_60$percentdead <- (94200-life_table_2016_50_55$lx)/94200
sum(life_table_2016_50_55$percentdead)
life_table_2016[50,]$lx-life_table_2016[60,]$lx
sum(life_table_2016[50:59, ]$lx)
(life_table_2016[50,]$lx-life_table_2016[60,]$lx)/sum(life_table_2016[50:59, ]$lx)



##### HW_04
# Table 4.2

life_table_2016 <- read_dta("inputs/life_table_2016.dta")
#lx is proprotion

age <- c(51, 52, 53, 54, 55, 56)
number <- c(335, 324, 313, 302, 292, 282)

hw04_2 <- data.frame(cbind(age, number))
hw04_2[,1] <- as.numeric(hw04_2[,1])
hw04_2[,2] <- as.numeric(hw04_2[,2])
(335-282)/sum(c(335, 324, 313, 302, 292))*1000
hw04_2$percentdead <- (335-hw04_2$number)/335
sum(hw04_2$percentdead)
sum(c(335, 324, 313, 302, 292))


age2 <- c(51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61)
number2 <- c(335, 324, 313, 302, 292, 282, 272, 262, 252, 242, 232)
sum(c(335, 324, 313, 302, 292, 282, 272, 262, 252, 242))

hw04_22 <- data.frame(cbind(age2, number2))
hw04_22[,1] <- as.numeric(hw04_22[,1])
hw04_22[,2] <- as.numeric(hw04_22[,2])
335-232
(335-232)/sum(c(335, 324, 313, 302, 292, 282, 272, 262, 252, 242))*1000
hw04_22$percentdead <- (335-hw04_22$number2)/335
sum(hw04_22$percentdead)


life_table_2016_50_55 <- life_table_2016[51:56,]
life_table_2016_50_55$percentdead <- (93849-life_table_2016_50_55$lx)/93849
sum(life_table_2016_50_55$percentdead)


(life_table_2016[51,]$lx - life_table_2016[56,]$lx)
sum(life_table_2016[51:55,]$lx)
(life_table_2016[51,]$lx - life_table_2016[56,]$lx)/sum(life_table_2016[51:55,]$lx)


(life_table_2016[51,]$lx - life_table_2016[61,]$lx)
sum(life_table_2016[51:60,]$lx)
(life_table_2016[51,]$lx - life_table_2016[61,]$lx)/sum(life_table_2016[51:60,]$lx)





life_table_2016_51_60 <- life_table_2016[51:60,]
life_table_2016_51_60$percentdead <- (94200-life_table_2016_50_55$lx)/94200
sum(life_table_2016_50_55$percentdead)
life_table_2016[50,]$lx-life_table_2016[60,]$lx
sum(life_table_2016[50:59, ]$lx)
(life_table_2016[50,]$lx-life_table_2016[60,]$lx)/sum(life_table_2016[50:59, ]$lx)





#### HW02
cigarett <- read_dta("STATA/cigarett.dta")
cotinine_levels <- c("0-13", "14-49", "50-99", "100-149", "150-199", "200-249" ,"250-299", "300+")
smokers <- c(78, 133, 142, 206, 197, 220, 151, 412)
nonsmokers <- c(3300, 72, 23, 15, 7, 8, 9, 11)

cotinine <- data.frame(cbind(cotinine_levels, smokers, nonsmokers))
cotinine[,2] <- as.numeric(cotinine[,2])
cotinine[,3] <- as.numeric(cotinine[,3])

cotinine$rel_smokers <- cotinine$smokers/1539
cotinine$rel_nonsmokers <- cotinine$nonsmokers/3445

cotinine$cotinine_levels <- factor(cotinine$cotinine_levels, levels = cotinine$cotinine_levels)
ggplot(data = cotinine, aes(x=cotinine_levels, group=1))+
  geom_line(aes(x=cotinine_levels, y=rel_smokers), color = "pink")+
  geom_line(aes(x=cotinine_levels, y=rel_nonsmokers), color = "skyblue")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))+
  ylab("Relative frequency")


ggplot(data = cotinine, aes(x=cotinine_levels, group=1))+
  geom_line(aes(x=cotinine_levels, y=c(rel_smokers, rel_nonsmokers)))+
  #geom_line(aes(x=cotinine_levels, y=rel_nonsmokers), color = "skyblue")+
  theme_classic()+
  labs(color = "group")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))+
  ylab("Relative frequency")

ggsave("outputs/HW03_3b.png", device = "png", units = "cm", width = 10, height = 7)


#### HW02_6 meningitis seizure #####

seizure <- c(0.10, 0.25, 0.50, 4, 12, 12, 24, 24, 31, 36, 42, 55, 96)

mean(seizure)
median(seizure)
range(seizure)
quantile(seizure)
sd(seizure)




##### HW02_8 hc_expenditures ######
hc_expenditures <- read_dta("inputs/hc_expenditures.dta")

ggplot(hc_expenditures, aes(x=per_capita))+
  geom_histogram(color="black", fill="grey", bins=10, binwidth = 1000, boundary=500)+
  labs(x=attributes(hc_expenditures$per_capita)$label)+
  theme_classic()+
  scale_y_continuous(expand = c(0,0),limits = c(0,11))+
  scale_x_continuous(breaks=seq(0,10000,1000))



ggplot(hc_expenditures, aes(x=per_capita))+
  geom_boxplot(color="black", fill="grey")+
  labs(x=attributes(hc_expenditures$per_capita)$label)+
  theme_classic()+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  scale_y_continuous(element_blank())+
  scale_x_continuous(breaks=seq(0,10000,1000))


####### HW 02 9 ######

pop_statistics <- read_dta("inputs/pop_statistics.dta")

ggplot(pop_statistics, aes(x=lowbwt))+
  geom_boxplot(color="black", fill="grey")+
  labs(x=attributes(pop_statistics$lowbwt)$label)+
  theme_classic()+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  scale_y_continuous(element_blank())

which.min(pop_statistics$lowbwt, na.rm = TRUE)
pop_statistics[which.min(pop_statistics$lowbwt),]
pop_statistics[which.max(pop_statistics$lowbwt),]

ggplot(pop_statistics, aes(x=lowbwt))+
  geom_histogram(color="black", fill="grey", bins = 10)+
  labs(x=attributes(pop_statistics$lowbwt)$label)+
  theme_classic()+
  theme()+
  scale_y_continuous(expand = c(0,0),limits = c(0,40))

Tmax = mean(pop_statistics$lowbwt, na.rm = T)+(3*sd(pop_statistics$lowbwt, na.rm = T))
pop_statistics[which(pop_statistics$lowbwt > Tmax),]
pop_statistics[order(pop_statistics$lowbwt, decreasing = T),]

mean(pop_statistics$lowbwt, na.rm = T)
median(pop_statistics$lowbwt, na.rm = T)


####### HW 02 10 ######

nursing_home <- read_dta("inputs/nursing_home.dta")

nursing_home[which.min(nursing_home$occupancy),]
nursing_home[which.max(nursing_home$occupancy),]


attributes(nursing_home$occupancy)$label

ggplot(nursing_home, aes(x=occupancy))+
  geom_boxplot(color="black", fill="grey")+
  labs(x=attributes(nursing_home$occupancy)$label)+
  theme_classic()+
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  scale_x_continuous(expand = c(0,0), limits = c(50,100))

ggplot(nursing_home, aes(x=occupancy))+
  geom_histogram(color="black", fill="grey", bins = 10)+
  labs(x=attributes(nursing_home$occupancy)$label)+
  theme_classic()+
  theme()+
  scale_y_continuous(expand = c(0,0),limits = c(0,12))

Tmax = mean(nursing_home$occupancy, na.rm = T)+(1.5*sd(nursing_home$occupancy, na.rm = T))
nursing_home[which(nursing_home$occupancy > Tmax),]
nursing_home[order(nursing_home$occupancy, decreasing = T),]

ggplot(nursing_home, aes(x=occupancy, group = region))+
  geom_boxplot(color="black", fill="grey")+
  labs(x=attributes(nursing_home$occupancy)$label)+
  labs(y=attributes(nursing_home$region)$label)+
  theme_classic()+
#  theme(axis.text.y = region)+
  scale_x_continuous(expand = c(0,0), limits = c(60,100))

ggplot(nursing_home, aes(x=region ~occupancy))+
  geom_boxplot(color="black", fill="grey")+
  labs(x=attributes(nursing_home$occupancy)$label)+
  labs(y=attributes(nursing_home$region)$label)+
  theme_classic()+
  scale_x_continuous(expand = c(0,0), limits = c(60,100))

boxplot(data = nursing_home, occupancy ~ region,
        ylab = attributes(nursing_home$occupancy)$label,
        xlab = attributes(nursing_home$region)$label)





############### binominal ########
pbinom(10,2,0.5)


############### binominal ########


cot$cotinine <- data.frame(c(5,7,9,11,13,14,15,17,19))

cotinine_level <- c(5,7,9,11,13,14,15,17,19)
Sensitivity <- c(0.971, 0.964, 0.960, 0.954, 0.950, 0.949, 0.945, 0.939, 0.932)
Specificity <- c(0.898, 0.931, 0.946, 0.951, 0.954, 0.956, 0.960, 0.963, 0.965)
cot <- data.frame(cbind(cotinine_level, Sensitivity, Specificity))
cot[,1]<- as.numeric(cot[,1])
cot[,2]<- as.numeric(cot[,2])
cot[,3]<- as.numeric(cot[,3])
cot$False_Positive <- 1-cot$Specificity
cot$distance <- sqrt((cot$False_Positive)*(cot$False_Positive)+(1-cot$Sensitivity)*(1-cot$Sensitivity))

ggplot(cot, aes(x=False_Positive, y=Sensitivity))+
  geom_line()+
  theme_classic()+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))

ggplot(cot, aes(x=False_Positive, y=Sensitivity))+
  geom_line()+
#  theme_classic()+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))
24735/77751





###### HW 07 #####

# 
x <- c(0,1,2,3,4,5)
P_X_is_x <- c(0.671, 0.229, 0.053, 0.031, 0.010, 0.006)

HW07_1 <- data.frame(cbind(x, P_X_is_x))
HW07_1[,1] <- as.numeric(HW07_1[,1])
HW07_1[,2] <- as.numeric(HW07_1[,2])


ggplot(data = HW07_1, aes(x=x, y=P_X_is_x))+
  geom_bar(aes(x=x, y=P_X_is_x), stat="identity")+
  theme_classic()+
  scale_y_continuous(expand = c(0,0), limits = c(0,0.8))+
  scale_x_continuous(breaks = c(0,1,2,3,4,5), labels = c("0","1","2","3","4","5+"))+
  ylab("P(X=x)")

HW07_1$expected<- HW07_1$x*HW07_1$P_X_is_x
sum(HW07_1$expected)
sum(HW07_1$P_X_is_x)-HW07_1$P_X_is_x[1]

0.031/0.329
2^7

7*6*5*4/(4*3*2*1)
((7*6)/(1*2))*((0.252)^2)*((1-0.252)^5)
((7)/(1))*((0.252)^1)*((1-0.252)^6)
((1)/(1))*((0.252)^0)*((1-0.252)^7)

0.131012+0.3089642+0.3122686 

((7*6*5)/(1*2*3))*((0.252)^3)*((1-0.252)^4)
((7*6*5*4)/(1*2*3*4))*((0.252)^4)*((1-0.252)^3)

(exp(-4.5)*(4.5)^3)/(3*2)
0.011109+0.04999048+0.1124786

1-(0.011109+0.04999048+0.1124786+0.1687179)
sqrt(4.5)

(0.5-0.045)+(0.5-0.001) 




#### Midterm review ###########





