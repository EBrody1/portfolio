# Evan Brody 
# Association rules mining of 1985 wage data with the Apriori algorithm
#Load arules package 
library("arules")
#then load the arulesViz
library("arulesViz")
# load the data from local directory 
wages <- read.csv("C:/Users/Yissa/OneDrive/data_UMGC/630/association/wages.csv",head =TRUE, as.is = FALSE)
# check variable types and structure
str(wages)
# set catagorical variables as factors which is neccesary for the algorithm and helpful for exploration
wages$union <-factor(wages$union)
wages$occupation <-factor(wages$occupation) 
wages$sector <-factor(wages$sector) 
wages$marital_status <-factor(wages$marital_status) 
wages$race <-factor(wages$race) 
wages$sex <- factor(wages$sex)
# explore
summary(wages)
plot(wages$wage)
#distribution
hist(wages$wage)
# lets see if there really was an outlier like the first plot seemed to show
boxplot(wages$wage, main= 'Wages', ylab= 'Wages Earned', las=1)
max(wages$wage)
# remove the extreme outlier
wages<- wages[-c(which.max(wages$wage)),]
# check if the max is now smaller
max(wages$wage)
#check for missing values
any(is.na(wages))
# variables will have to be binned  for the algorithm check out the distribution to see how to break it 
hist(wages$experience, main ='Worker Experience', xlab = 'Years of Experience',las = 1)
hist(wages$education)
# age is colinear with experience and didn't help predict wages so it will be dropped
wages$age  <- NULL
# we must discretize or bin the continous variables  wage, education,experience,
wages$education<-cut(wages$education, breaks =c(0, 11, 12, 15, 16,  100), labels=c('NO_HS', 'HS', 'some_College','college', 'Grad'))
summary(wages$education)
barplot(summary(wages$education), main= 'Education Binned', las=1, ylab= 'Amount of workers', xlab= "Amount of Education")
wages$experience <- discretize(wages$experience, method="frequency", breaks=6)
summary(wages$experience)
wages$wage <- discretize(wages$wage,method="interval", breaks=6)
# check out new catagories created
barplot(summary(wages$wage), main= 'Wages Discretized', ylab = 'Frequency', xlab = 'Wages', las=1)
summary(wages$wage)
# use the model to generate rules
rules <- apriori(wages, parameter=list(minlen=2, conf=0.4))
# sort by lift
rulsort <- sort(rules, by="lift")
# remove redundant rules with built in method
rules.pruned <- rulsort[!is.redundant(rulsort, measure = "confidence")]
# find how many rules remain after pruning
length(rules.pruned)
# examine results 
summary(rules.pruned)
inspect(rules.pruned[1:30])
# filter for rules with wage in them
wg <- subset(rules.pruned, items %pin% "wage=")
inspect(wg[1:50])
summary(wg)
# other metrics to judge with
interestMeasure(wg, c("support", "chiSquare", "confidence", "conviction", "cosine", "coverage", "leverage", "lift", "oddsRatio"), wages)
# visualize the rules of wage rules
# plot metrics
plot(wg)
# 2nd view of the metrics 
plot(wg, method="matrix", engine="3d", measure="lift", control=list(reorder="support"))
# cluster of rules 
plot(wg[1:100], method="graph", control=list(type="items"))
# grouping of the rules by which variables are involved
plot(wg, method = "grouped")




