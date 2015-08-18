library(Hmisc)
library(lattice)

#START OF DATA IMPORT 

CTG=read.csv("C:/Users/jgozal1/Desktop/CTG/CTG-raw.csv")
str(CTG)

prop.table(table(CTG$D))

#END OF DATA IMPORT

CTG$NSP=as.factor(CTG$NSP)

describe(CTG)

hist(CTG$LBE,main="Baseline Value (medicalexpert) Histogram", xlab="Baseline Value", ylab="Number of Fetuses", col="gold")

hist(CTG$LB,main="Baseline Value Histogram (sisporto)", xlab="Baseline Value", ylab="Number of Fetuses", col="gold")

hist(CTG$AC,main="Accelerations Histogram", xlab="Accelerations", ylab="Number of Fetuses", col="gold")

hist(CTG$FM,main="Fetal Movement Histogram", xlab="Fetal Movement", ylab="Number of Fetuses", col="gold")

hist(CTG$UC,main="Uterine Contractions Histogram", xlab="Uterine Contractions", ylab="Number of Fetuses", col="gold")

hist(CTG$ASTV,main="Percentage of time with abnormal short term variability Histogram", xlab="Percentage of time with abnormal short term variability", ylab="Number of Fetuses", col="gold")

hist(CTG$DL,main="Light Decelerations Histogram", xlab="Light Decelerations", ylab="Number of Fetuses", col="gold")

hist(CTG$DS,main="Severe Decelerations Histogram", xlab="Severe Decelerations", ylab="Number of Fetuses", col="GREY")

hist(CTG$DP,main="Prolongued Decelerations Histogram", xlab="Prolongued Decelerations", ylab="Number of Fetuses", col="grey")

hist(CTG$DR,main="Repetitive Decelerations Histogram", xlab="Repetitive Decelerations", ylab="Number of Fetuses", col="grey")

hist(CTG$A,main=" Histogram of Calm Sleep", xlab="Calm Sleep", ylab="Number of Fetuses", col="grey")

hist(CTG$E,main="Sleep Pattern Histogram", xlab="Sleep Pattern", ylab="Number of Fetuses", col="grey")

hist(CTG$NSP,main="2 Category Response Histogram", xlab="NSP", ylab="Number of Fetuses", col="grey")

boxplot(CTG$LBE, col="grey", main="Fetal Heart Rate", xlab="Fetal Heart Rate", ylab="Value",boxwex=0.2)

pairs(CTG[1:40], main="Matrix Plot CTG")

names(CTG)


CTG=read.csv("C:/Users/jgozal1/Desktop/CTG/CTG-raw.csv")

is.na(1:40)


