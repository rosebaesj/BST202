# Date July 5th 2022

library(ggplot2) #package to plot
library(haven) #package to import .dta files to R (https://cran.r-project.org/web/packages/haven/haven.pdf)
library(dplyr) #for mutate function
library(tidyverse)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#### Module 1 Homework ######
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


##### + Step 1: make a csv file from the given table #####
# the file as spaces so tsv might make errors, use csv just in case
m1hw <- read.csv("STATA/Module1.csv", sep = ",", header = TRUE)

##### + Step 2: Make ranks ####
#+ the rank function is in base package as rank() 
#+ but I actually used order() instead
#+ it's still in base package

ranked_m1hw <- m1hw[order(m1hw$percap),]
ggplot(m1hw, aes(x=percap))+
  geom_histogram(color="black", fill="grey", bins = 5)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#### Module 1 Lab ######
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

##### + get dta ####
heart <- read_dta("STATA/heart.dta")

#remove attr of columns
aheart <- mutate(heart, across(everything(), as.vector))

summary(heart)
# # # # # # # # # RESULTS # # # # # # # # # # # # # # # 
# trtment            pdi              mdi       
# Min.   :0.0000   Min.   : 50.00   Min.   : 50.0  
# 1st Qu.:0.0000   1st Qu.: 86.00   1st Qu.: 96.0  
# Median :0.0000   Median : 98.00   Median :106.5  
# Mean   :0.4841   Mean   : 94.78   Mean   :104.7  
# 3rd Qu.:1.0000   3rd Qu.:105.00   3rd Qu.:115.0  
# Max.   :1.0000   Max.   :134.00   Max.   :142.0  
# NA's   :14       NA's   :13    
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#### + draw plot and save #####
ggplot(heart, aes(x=mdi))+
  geom_histogram(color="black", fill="grey", bins=10)+
  labs(x=attributes(heart$mdi)$label)+ ## add descriptions by using labs() and attributes()
  theme_classic()+ 
  scale_y_continuous(expand = c(0,0),limits = c(0,45)) # to make y axis start from 0

ggsave("outputs/heart_histogram.png", device = "png", units = "cm", width = 10, height = 7)





#### + categorize by mdi #####
heart <- heart %>%
  mutate(mdicat = case_when(
    mdi<90 ~ 1,
    mdi>=90 ~0))
#every NA automatically gives NA for mdicat

### + description for mdicat ######
attr(heart$mdicat, which = "label") <- c("mdi categorized")

### make it as factor so that we can count easily
heart$mdicat <- as.factor(heart$mdicat)
summary(heart$mdicat)




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#### Module 2 Lab ######
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

cigarett <- read_dta("STATA/cigarett.dta")
lab2 <- read_dta("STATA/lab2.dta") #same files

### Q a) Find the mean and standard deviation of the concentrations of tar #####
mean(cigarett$tar)
# [1] 11.50286
sd(cigarett$tar) 
# [1] "concentration per cig (mg)"
### Don’t forget the units
attr(cigarett$tar, which = "label")
#[1] "concentration per cig (mg)"

### Q b) Find the median, range, and interquartile range of tar. ###
summary(cigarett$tar)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.7     9.5    13.0    11.5    16.0    19.0 

### Q c) Produce a histogram of the tar measurements. Remember to use the frequency option.
ggplot(cigarett, aes(x=tar))+
  geom_histogram(color="black", fill="grey", bins=15)+
  labs(x=attributes(cigarett$tar)$label)+ ## add descriptions by using labs() and attributes()
  theme_classic()+
  scale_y_continuous(expand = c(0,0),limits = c(0,10)) # to make y axis start from 0
ggsave("outputs/cigarett.png", device = "png", units = "cm", width = 10, height = 7)
### Describe the shape of the values.
#+ left skewed


### Q d) Which number (mean or median) do you think provides the best measure of central tendency in this case? Why?
#+ median. bc skewed

### Q e) What percentage of this data lies within 1 standard deviations from the mean?
sd1 <- c(mean(cigarett$tar)-sd(cigarett$tar), mean(cigarett$tar)+sd(cigarett$tar))
# 6.2, 16.8
cigarett <- cigarett %>%
  mutate(within1sd = case_when(
    sd1[1]<tar & tar<sd1[2] ~ 1,
    !(sd1[1]<tar & tar<sd1[2]) ~0
    ))
mean(cigarett$within1sd)
### How does this compare to what is predicted by the Empirical Rule and by Chebychev’s Inequality?
# larger than emperical rule. expected bc not normal distribution

cigarett <- cigarett %>%
  mutate(groupbycotinine = case_when(
    0<=nicotine & nicotine<=13 ~ 1,
    14<=nicotine & nicotine<=149 ~ 2,
    150<=nicotine & nicotine<=299 ~ 3
  ))

grouped <- data.frame(cotinine=c("0-13", "14-149", "150-299"), 
                      midpoint=c(6.5, 81.5, 224.5), 
                      smokers=c(78,481,568),
                      nonsmokers=c(3300,110,24))
grouped[,2]<- as.numeric(grouped[,2])
grouped[,3]<- as.numeric(grouped[,3])
grouped[,4]<- as.numeric(grouped[,4])

### Better approach
GroupedResults<-NULL #set an empty object

#get midpoint*observations for each row
grouped$mf_smokers <- grouped$midpoint*grouped$smokers
grouped$mf_nonsmokers <- grouped$midpoint*grouped$nonsmokers

#Sum and divide by total observations
GroupedResults$mean_smokers<-sum(grouped$mf_smokers)/1127
GroupedResults$mean_nonsmokers<-sum(grouped$mf_nonsmokers)/3434
# $mean_smokers
# [1] 148.3802
# 
# $mean_nonsmokers
# [1] 10.42603


# calculate (midpoint-mean)square*observations
grouped$m_x2f_smokers <- (grouped$midpoint-GroupedResults$mean_smokers)^2*grouped$smokers
grouped$m_x2f_nonsmokers <- (grouped$midpoint-GroupedResults$mean_nonsmokers)^2*grouped$nonsmokers

# sum ans divide by total observations
GroupedResults$variance_smokers <- sum(grouped$m_x2f_smokers)/(1127-1)
GroupedResults$variance_nonsmokers <- (sum(grouped$m_x2f_nonsmokers)/(3434-1))
# $variance_smokers
# [1] 6228.022
# 
# $variance_nonsmokers
# [1] 497.0566


