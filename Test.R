#### for tests ####
#### Standardization ####
group <- c("65", "70", "75")

subgroup1_dist <- c(1500, 550, 120)
subgroup1_case <- c(7, 20, 120)
subgroup1_rate <- c()
subgroup1_rate <- subgroup1_case/subgroup1_dist

subgroup2_dist <- c(3385000, 3497100, 1944900)
subgroup2_case <- c(1387, 8304, 83950)
subgroup2_rate <- 
subgroup2_rate <- subgroup2_case/subgroup2_dist

total_dist <- c(6000, 5500, 2500)
total_dist<- subgroup1_dist+subgroup2_dist

total_case <- c(7,20,120)
total_case <- subgroup1_case+subgroup2_case
total_rate <- c()
total_rate <- c(51000, 37000, 12000)/100000
total_rate <-total_case/total_dist

data1 <- data.frame(subgroup1_dist, subgroup1_rate, total_dist, total_rate)
data2 <- data.frame(subgroup2_dist, subgroup2_rate, total_dist, total_rate)

### + Direct standardization###
# Direct: Standard dist
data1$direct_case <- total_dist*subgroup1_rate
sum(data1$direct_case)/sum(total_dist)

# Indirect: Standard rate
data1$indirect_case <- subgroup1_dist*total_rate
sum(data1$indirect_case)/sum(subgroup1_dist)

15300/(sum(data1$indirect_case)*1000)


# Direct: Standard dist vs total
data2$direct_case <- total_dist*subgroup2_rate
sum(data2$direct_case)/sum(total_dist)

# Indirect: Standard pop
data2$indirect_case <- subgroup2_dist*total_rate
sum(data2$indirect_case)/sum(subgroup2_dist)



options(scipen=999)



#####
data2_1 <- data.frame(subgroup2_dist, subgroup2_rate, subgroup1_dist, subgroup1_rate)

data2_1$direct_case <- subgroup2_dist*subgroup1_rate
sum(data2_1$direct_case)/sum(subgroup2_dist)

# Indirect: Standard pop
data2$indirect_case <- subgroup1_dist*subgroup2_rate
sum(data1$indirect_case)/sum(subgroup1_dist)






mean(c(2,4,6,8,10))
sd(c(2,4,6,8,10))
median(c(2,4,6,8,10))
range(c(2,4,6,8,10))

1/4+1/13-(1/4)*(1/13)

1 - (0.10 + 0.15)

4.58-3.02

5/16


48460/100000

72063/98344

0.6*0.05

1-(0.13534+0.27067+0.27067)

1-0.159

6*6*6

0.1216+0.27017+0.2851


