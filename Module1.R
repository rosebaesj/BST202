
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
  geom_histogram(color="black", fill="grey", bins=10)

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







