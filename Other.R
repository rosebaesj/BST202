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

  
