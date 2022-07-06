# Module 1
# Date July 5th 2022

library(ggplot2) #package to plot
library(haven) #package to import .dta files to R

##### Step 1: make a csv file from the given table #####
# the file as spaces so tsv might make errors, use csv just in case

data <- read.csv("Module1.csv", sep = ",", header = TRUE)


##### Step 2: Make ranks ####
#+ the rank function is in base package as rank() 
#+ but I actually used order() instead
#+ it's still in base package

ranked_data <- data[order(data$percap),]

ggplot(data, aes(x=percap))+
  geom_histogram(color="black", fill="grey", bins = 5)




