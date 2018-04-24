install.packages("plyr")
install.packages("mice")
install.packages("VIM")
library(plyr)
library(mice)
library(VIM)
options(scipen=999)  
library(ggplot2)

#Merging Datasets
Merged_data <- merge(YRBS_data,Naltrexone_data_sum, id=GEO.id, all=TRUE)
FullDataset <- merge(Merged_data,Selected_STDs_2014_, id=GEO.id, all=TRUE)

sapply(YRBS_data,mode)

#Creating new variables
FullDataset$PrescriptionRate <- (FullDataset$`Number of Prescriptions`/ FullDataset$Population)*100000

breaks4 <- quantile(FullDataset$FDP,probs=seq(0,1, by=0.25), na.rm = TRUE)
scorelabels = c("low","mod low", "mod high","high")
FullDataset$BDPS <- cut(FullDataset$FDP,breaks4,include.lowest=TRUE, labels = scorelabels)

breaks5 <- quantile(FullDataset$PrescriptionRate, probs=seq(0,1, by=0.25), na.rm = TRUE)
FullDataset$NPS <- cut(FullDataset$PrescriptionRate,breaks5,include.lowest=TRUE, labels = scorelabels)

breaks6 <- quantile(FullDataset$`Total Rate`, probs=seq(0,1, by=0.25), na.rm = TRUE)
FullDataset$STDPS <- cut(FullDataset$`Total Rate`,breaks6,include.lowest=TRUE, labels = scorelabels)

#Frequency Tables
BDPStable <- table(FullDataset$BDPS)
BDPStable
NPStable <- table(FullDataset$NPS)
NPStable

#Graphical Representations
scatterplot<-ggplot(FullDataset, aes(x=FullDataset$CDP, y=FullDataset$CMP)) + geom_point()+ geom_smooth(method="lm") 
ggplot(FullDataset, aes(x=FullDataset$CDP, y=FullDataset$`Units Reimbursed`)) + geom_point() + geom_smooth(method="lm")+coord_cartesian(xlim=c(0,100), ylim=c(0, 1000000))
ggplot(FullDataset, aes(x=FullDataset$FDP, y=FullDataset$`Total Rate`)) + geom_point() + geom_smooth(method="lm")+coord_cartesian(xlim=c(0,100), ylim=c(0, 1200))
ggplot(FullDataset, aes(x=FullDataset$CDP, y=FullDataset$FDP)) + geom_point() + geom_smooth(method="lm")+coord_cartesian(xlim=c(0,100), ylim=c(0, 100))
ggplot(FullDataset, aes(x=FullDataset$CDP, y=FullDataset$PrescriptionRate)) + geom_point() + geom_smooth(method="lm")+coord_cartesian(xlim=c(0,100), ylim=c(0, 220))
ggplot(FullDataset, aes(x=FullDataset$DBTP, y=FullDataset$PrescriptionRate)) + geom_point() + geom_smooth(method="lm")+coord_cartesian(xlim=c(0,100), ylim=c(0, 220))
ggplot(FullDataset, aes(x=FullDataset$CDN, y=FullDataset$`Number of Prescriptions`)) + geom_point() + geom_smooth(method="lm")+coord_cartesian(xlim=c(0,50000), ylim=c(0,30000))
ggplot(FullDataset, aes(x=FullDataset$`Syphiilis Rate`, y=FullDataset$CDP)) + geom_point() + geom_smooth(method="lm")+coord_cartesian(xlim=c(0,25), ylim=c(0, 100))




