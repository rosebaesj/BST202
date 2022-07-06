# Module 1
# Date July 5th 2022

##### Step 1: make a csv file from the given table #####
# the file as spaces so tsv might make errors, use csv just in case

data <- read.csv("Module1.csv", sep = ",", header = TRUE)


##### Step 2: Make ranks ####
#+ the rank function is in base package as rank() 
#+ but I actually used order() instead
#+ it's still in base package

ranked_data <- data[order(data$percap),]

library(ggplot2)
ggplot(data, aes(x=percap))+
  geom_histogram(color="black", fill="grey")+
  scale_x_continuous(breaks = seq(0,2500,))
  sjbalabs(x="Dietary trans fat intake (g/day)", y = "Count")+
  + #16
  theme(axis.title=element_text(size=50,face = 'bold'),
        axis.text.x = element_text(size=50),
        axis.text.y = element_text(size=50),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(colour = 'black', size=0.75, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.75, linetype='solid')
  )
plot(aofib_ddr_distribution)