# Date July 5th 2022


# you need to install the packages below, using install.packages()
# I already have them on my Mac so I didn't install them
# every time you start R or Rstudio, you need to open the packages, using library()

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




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#### Module 3 Lab ######
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

lab3 <- read_dta("STATA/lab3.dta") #same files
View(lab3)


### Q a) What are the crude cancer mortality rates (per 1000) for each of 1940 and 1986,#######
#        and how do they compare?
### A 1986 has a higher death rate

sum(lab3$deaths40)/sum(lab3$pop40)
#1.201489
sum(lab3$deaths86)/sum(lab3$pop86)
#1.946644

## want to make the table?
colnames <- c("Year", "Deaths", "Total Population (1000s)", "Death Rate (1000s)")
year1940 <- c("1940", sum(lab3$deaths40), sum(lab3$pop40), sum(lab3$deaths40)/(sum(lab3$pop40)))
year1986 <- c("1986", sum(lab3$deaths86), sum(lab3$pop86), sum(lab3$deaths86)/(sum(lab3$pop86)))

answertable_a <- rbind(colnames, year1940, year1986)


#### b). For both 1940 and 1986, calculate the proportion of the total population in each age group. ####
#+ We can use Stata to generate a new variable called percent40 as follows

#Type the following commands into the Command window
#I used sum instead of the exact numbers
lab3$per40 <- lab3$pop40/sum(lab3$pop40)
lab3$per86 <- lab3$pop86/sum(lab3$pop86)
#### c) ######
# 1940s are bit younger


##### d) Compute the age-specific cancer death rates for each population. #######

lab3$ASR40 <- lab3$deaths40/lab3$pop40
# [1] 0.04302738 0.74430994 5.85330747
lab3$ASR86 <- lab3$deaths86/lab3$pop86
# [1] 0.04334692 0.59185721 7.91807393
lab
###       Is there a relationship between age and death rate?
# yes very necessary 

#### e) Does it appear to be necessary to control for the effect of age when comparing cancer death rates in the two populations? Why or why not? (We can check the trend of the age-specific rates: create a variable for the age categories in the data editor (called for example agecat) with just the numbers 1, 2, 3. Then type twoway (line agerate40 agecat) (line agerate86 agecat)to create two line graphs.)#####
# not really,,,? but a little bit because olds died a lot in 86
lab3$age_category <- c(1, 2, 3)
ggplot(lab3, aes(x=age_category))+
  geom_line(aes(y=ASR40), color = "pink")+
  geom_line(aes(y=ASR86), color = "skyblue")+
  theme_classic()
ggsave("outputs/lab3_e.png", device = "png", units = "cm", width = 10, height = 7)

#

##### f) Using the U.S. population in 1940 as a standard distribution, 
# apply the direct method of standardization. What are the age-adjusted cancer mortality rates for 1940 and 1986? (Stata commands: generate exp40=agerate40*pop40, generate exp86=agerate86*pop40,tabstat exp40 exp86, statistics(sum))
###****use the US population of 1940, keep the ASR****###
lab3$expectedD40 <- lab3$ASR40*lab3$pop40
sum(lab3$expectedD40)
lab3$expectedD86 <- lab3$ASR86*lab3$pop40
sum(lab3$expectedD86)


#### g) How does the age-adjusted death rate differ from the crude death rate in each of these populations? 
# How would you explain these differences?



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#### HW 03 ######
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Group <- c("Children", "Cases", "Total", "Paralytic", "Non-Paralytic", "Not_Polio")
Vaccine <- c(200745, 82, 57, 33, 24, 25)
Placebo <- c(201229, 162, 142, 115, 27, 20)

HW03_2 <- data.frame(rbind(Vaccine, Placebo))
colnames(HW03_2) <- Group

t_HW03_2 <- data.frame(t(HW03_2))
# When ERROR: $ operator is invalid for atomic vectors >>>>> data.frame it

t_HW03_2$Vaccine_rates<- t_HW03_2$Vaccine/200745*100000
t_HW03_2$Placebo_rates<- t_HW03_2$Placebo/201229*100000

HW03_2 <- rbind(HW03_2, Vaccine_rates, Placebo_rates)


##### Question 14 ####
Age <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
ME_pop <- c(75037, 79727, 74061, 68683, 60575, 105723, 101192, 90346, 72478, 46614, 22396)
ME_deaths <- c(1543, 148, 104, 153, 224, 413, 552, 980, 1476, 2433, 3056)
SC_pop <- c(205076, 240750, 222808, 211345, 166354, 219327, 191349, 143509, 80491, 40441, 16723)
SC_deaths <- c(4905, 446, 410, 901, 1073, 1910, 2377, 2862, 2667, 2486, 2364)
HW03_3 <- data.frame(cbind(Age, ME_pop, ME_deaths, SC_pop, SC_deaths))
for (i in 2: ncol(HW03_3)){
 HW03_3[,i] <- as.numeric( HW03_3[,i]) 
}
HW03_3[,1] <- factor(x = HW03_3[,1], level = Age)

Total <- t(c(796832, 11082, 1738173, 22401))
colnames(Total)<- c("ME_pop", "ME_deaths", "SC_pop", "SC_deaths")

HW03_3$ME_pro <- HW03_3$ME_pop/796832
HW03_3$SC_pro <- HW03_3$SC_pop/1738173


ggplot(HW03_3, aes(group = 1))+
  geom_line(aes(x= Age, y=ME_pro), color = "pink")+
  geom_line(aes(x= Age, y=SC_pro), color = "skyblue")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))+
  ylab("Proportion of Population")
ggsave("outputs/HW03_3b.png", device = "png", units = "cm", width = 10, height = 7)


HW03_3$ME_ASM <- HW03_3$ME_deaths/HW03_3$ME_pop*1000
HW03_3$SC_ASM <- HW03_3$SC_deaths/HW03_3$SC_pop*1000

ggplot(HW03_3, aes(group = 1))+
  geom_line(aes(x= Age, y=ME_ASM), color = "pink")+
  geom_line(aes(x= Age, y=SC_ASM), color = "skyblue")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))+
  ylab("Age-Specific Mortality Rates")
ggsave("outputs/HW03_3c.png", device = "png", units = "cm", width = 10, height = 7)


HW03_3$US_pop <- c(8.01, 8.11, 8.92, 9.37, 8.80, 16.21, 13.92, 11.78, 8.03, 4.84, 2.01)

HW03_3$ME_Spop <- ((HW03_3$US_pop/100)*796832)
HW03_3$SC_Spop <- ((HW03_3$US_pop/100)*1738173)

HW03_3$ME_Edeaths <- ((HW03_3$US_pop/100)*796832)*(HW03_3$ME_ASM/1000)
HW03_3$SC_Edeaths <- ((HW03_3$US_pop/100)*1738173)*(HW03_3$SC_ASM/1000)

sum(HW03_3$ME_Edeaths)/796832*1000
sum(HW03_3$SC_Edeaths)/1738173*1000











