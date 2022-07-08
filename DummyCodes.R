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






### grouped mean

GroupedMean<-NULL
Smf_smokers<- 0
for(i in 1:3){
  Smf_smokers <- Smf_smokers + grouped$midpoint[i]*grouped$smokers[i]
}
GroupedMean$smokers = Smf_smokers/1127

Smf_nonsmokers<- 0
for(i in 1:3){
  Smf_nonsmokers <- Smf_nonsmokers + grouped$midpoint[i]*grouped$nonsmokers[i]
}
GroupedMean$nonsmokers = Smf_nonsmokers/3434
GroupedMean
# $smokers
# [1] 148.3802
# 
# $nonsmokers
# [1] 10.42603



### grouped variance
# Better approach
GroupedVariance<-NULL




# Way around
GroupedVariance<-NULL
Smx2f_smokers <- 0
for(i in 1:3){
  Smx2f_smokers <- Smx2f_smokers + (grouped$midpoint[i]-GroupedMean$smokers)*(grouped$midpoint[i]-GroupedMean$smokers)*grouped$smokers[i]
}
GroupedVariance$smokers = Smx2f_smokers/(1127-1)

Smx2f_nonsmokers <- 0
for(i in 1:3){
  Smx2f_nonsmokers <- Smx2f_nonsmokers + (grouped$midpoint[i]-GroupedMean$nonsmokers)*(grouped$midpoint[i]-GroupedMean$nonsmokers)*grouped$nonsmokers[i]
}
GroupedVariance$nonsmokers = Smx2f_nonsmokers/(3434-1)
GroupedVariance

# $smokers
# [1] 6228.022
# 
# $nonsmokers
# [1] 497.0566

