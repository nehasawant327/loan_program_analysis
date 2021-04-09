setwd("D:\\study\\loan_program_analysis")

library(caTools)
library(car)
library(caret)
library(class)
library(e1071)
library(ggplot2)
library(Hmisc)
library(klaR)
library(plyr)
library(ROCR)
library(pROC)
library(psych)
library(dplyr)
library(corrplot)
library(RColorBrewer)
library(lmtest)
library(pscl)
library(DMwR)

Data7a <- read.csv("7a_new_1.csv")
View(Data7a)
str(Data7a)
dim(Data7a)

##Summary of dataset
summary(Data7a)

#create Default variable
Data7a$default<-ifelse(Data7a$LoanStatus=="CHGOFF",1,0)

summary(Data7a)

## Target Rate (what is my percetage of 1s versus 0s) 
sum(Data7a$default)/222931

#checking for missing values
data7a1<-sapply(Data7a, function(x) sum(is.na(x)))
data7a1

sum(is.na(Data7a))

#delete the rows with NA values as we have 8 rows only
na.omit(Data7a)

##variables to remove- 
Data7a$BorrStreet <- NULL 
Data7a$BorrCity <- NULL
Data7a$BorrState <- NULL
Data7a$BorrZip <- NULL
Data7a$BankStreet <- NULL
Data7a$BankCity <- NULL
Data7a$BankState <- NULL
Data7a$BankZip <- NULL
Data7a$NaicsCode <- NULL
Data7a$NaicsDescription <- NULL
Data7a$FranchiseCode <- NULL
Data7a$FranchiseName <- NULL
Data7a$ProjectCounty <- NULL
Data7a$ProjectState <- NULL
Data7a$ApprovalDate<-NULL
Data7a$FirstDisbursementDate <- NULL
Data7a$ChargeOffDate <- NULL
names(Data7a)


## Checking For MultiCollinearity

Data7a2<-Data7a[c(19,5,6,10,11,16:18,7,1:4,8,9,12:15)]
names(Data7a2)
corrplot(cor(Data7a2[,2:8]))
dev.off()

#Quickplots for EDA
D1<-Data7a2
D1$ApprovalFiscalYear<-NULL
D1$AsOfDate<-NULL
D1$Program<-NULL
D1$BorrName<-NULL
D1$BankName<-NULL
D1$DeliveryMethod<-NULL
D1$subpgmdesc<-NULL
D1$SBADistrictOffice<-NULL
D1$CongressionalDistrict<-NULL
D1$LoanStatus<-NULL

levels(D1$BusinessType)
D1$corp<-ifelse(D1$BusinessType=="CORPORATION",1,0)
D1$ind<-ifelse(D1$BusinessType=="INDIVIDUAL",1,0)
D1$part<-ifelse(D1$BusinessType=="PARTNERSHIP",1,0)

D1$BusinessType<-NULL
D1$default<-as.factor(D1$default)
qplot(SBAGuaranteedApproval, InitialInterestRate , colour = default, data=D1)
qplot(TermInMonths, InitialInterestRate , colour = default, data=D1)
qplot(corp, InitialInterestRate , colour = default, data=D1)
qplot(ind, InitialInterestRate , colour = default, data=D1)
qplot(part, InitialInterestRate , colour = default, data=D1)
qplot(SBAGuaranteedApproval, TermInMonths , colour = default, data=D1)
qplot(corp, TermInMonths, colour = default, data=D1)
qplot(ind, TermInMonths , colour = default, data=D1)
qplot(part, TermInMonths , colour = default, data=D1)
dev.off()

#converts the AsofDate factor to character strings, then interprets the strings as dates.
Data7a$AsOfDate<- as.character(Data7a$AsOfDate, format = "%Y%m%d")
Data7a$AsOfDate<- as.Date(Data7a$AsOfDate, format = "%Y%m%d")

#Convert Borrower name into char
Data7a$BorrName <- as.character(Data7a$BorrName)

#convert default into Factor
Data7a$default<-as.factor(Data7a$default)
#drop Program variable
Data7a$Program<-NULL

##Splitting into train and test data

set.seed(1209)
spindex<-createDataPartition(Data7a$default, p=0.7, list = FALSE)
Data7atrain<-Data7a[spindex,]
Data7atest<-Data7a[-spindex,]




##Univariate Analysis
ggplot(Data7a, aes(Data7a$default)) + geom_bar(fill="green")


#Gross Approval
par(mfrow=c(1,3))
hist(Data7atrain$GrossApproval,xlab = "GrossApproval",main = "Histogram-GrossApproval")
boxplot(Data7a$AsOfDate,xlab = "GrossApproval",main="Boxplot-GrossApproval")
qqnorm(Data7a$AsOfDate,xlab = "GrossApproval",main = "QQ-Plot-GrossApproval")
dev.off()

#SBAGuaranteedApproval
par(mfrow=c(1,3))
hist(Data7atrain$SBAGuaranteedApproval,xlab = "SBAGuaranteedApproval",main = "Histogram-SBAGuaranteedApproval")
boxplot(Data7a$SBAGuaranteedApproval,xlab = "SBAGuaranteedApproval",main="Boxplot-SBAGuaranteedApproval")
qqnorm(Data7a$SBAGuaranteedApproval,xlab = "SBAGuaranteedApproval",main = "QQ-Plot-SBAGuaranteedApproval")
dev.off()

#InitialInterestRate
par(mfrow=c(1,3))
hist(Data7atrain$InitialInterestRate,xlab = "InitialInterestRate",main = "Histogram-InitialInterestRate")
boxplot(Data7a$InitialInterestRate,xlab = "InitialInterestRate",main="Boxplot-InitialInterestRate")
qqnorm(Data7a$InitialInterestRate,xlab = "InitialInterestRate",main = "QQ-Plot-InitialInterestRate")
dev.off()

#TermIn Months
par(mfrow=c(1,3))
hist(Data7atrain$TermInMonths  ,xlab = "TermInMonths",main = "Histogram-TermInMonths")
boxplot(Data7a$TermInMonths,xlab = "TermInMonths",main="Boxplot-TermInMonths")
qqnorm(Data7a$TermInMonths,xlab = "TermInMonths",main = "QQ-Plot-TermInMonths")
dev.off()

#GrossChargeOffAmount
par(mfrow=c(1,3))
hist(Data7atrain$GrossChargeOffAmount, xlab = "GrossChargeOffAmount",main = "Histogram-GrossChargeOffAmount")
boxplot(Data7a$GrossChargeOffAmount,xlab = "GrossChargeOffAmount",main="Boxplot-GrossChargeOffAmount")
qqnorm(Data7a$GrossChargeOffAmount,xlab = "GrossChargeOffAmount",main = "QQ-Plot-GrossChargeOffAmount")
dev.off()

#RevolverStatus
par(mfrow=c(1,3))
hist(Data7atrain$RevolverStatus, xlab = "RevolverStatus",main = "Histogram-RevolverStatus")
boxplot(Data7a$RevolverStatus,xlab = "RevolverStatus",main="Boxplot-RevolverStatus")
qqnorm(Data7a$RevolverStatus,xlab = "RevolverStatus",main = "QQ-Plot-RevolverStatus")
dev.off()

#jobssupported
par(mfrow=c(1,3))
hist(Data7atrain$JobsSupported, xlab = "JobsSupported",main = "Histogram-JobsSupported")
boxplot(Data7a$JobsSupported,xlab = "JobsSupported",main="Boxplot-JobsSupported")
qqnorm(Data7a$JobsSupported,xlab = "JobsSupported",main = "QQ-Plot-JobsSupported")
dev.off()

#BusinessType
t3<-table(Data7atrain$BusinessType)
pt3<-prop.table(t3)
t3
pt3
par(mfrow=c(1,2))
barplot(t3,main = "Bar-graph-BusinessType",xlab = "Levels",ylab = "Frequency")
barplot(pt3,main = "Relative Frequency-graph-BusinessType",xlab = "Levels",ylab = "Relative Frequency")
dev.off()

#Loanstatus
t3<-table(Data7a$LoanStatus)
pt3<-prop.table(t3)
t3
pt3
par(mfrow=c(1,2))
barplot(t3,main = "Bar-graph-LoanStatus",xlab = "Levels",ylab = "Frequency")
barplot(pt3,main = "Relative Frequency-graph-LoanStatus",xlab = "Levels",ylab = "Relative Frequency")
dev.off()

#default
t3<-table(Data7a$default)
pt3<-prop.table(t3)
t3
pt3
par(mfrow=c(1,2))
barplot(t3,main = "Bar-graph-default",xlab = "Levels",ylab = "Frequency")
barplot(pt3,main = "Relative Frequency-graph-default",xlab = "Levels",ylab = "Relative Frequency")
dev.off()


###########################################################################

## BiVariate Analysis:

# GrossApproval vs Default
par(mfrow=c(1,2))
plot(Data7atrain$GrossApproval,col=brewer.pal(3,"Set1")[Data7atrain$default],pch=20,main="Gross Approval against Default",ylab = "GrossApproval")
boxplot(Data7atrain$GrossApproval~Data7atrain$default,main="Gross Approval against Default",xlab = "Default",ylab = "GrossApproval",col=brewer.pal(3,"Set1")[Data7atrain$default])
aov_GrossApproval_Default<-aov(Data7atrain$GrossApproval~Data7atrain$default)
summary(aov_GrossApproval_Default)
dev.off()


# SBAGuaranteedApproval vs Default
par(mfrow=c(1,2))
plot(Data7atrain$SBAGuaranteedApproval,col=brewer.pal(3,"Set1")[Data7atrain$default],pch=20,main="SBAGuaranteedApproval against Default",ylab = "SBAGuaranteedApproval")
boxplot(Data7atrain$SBAGuaranteedApproval~Data7atrain$default,main="SBAGuaranteedApproval against Default",xlab = "Default",ylab = "SBAGuaranteedApproval",col=brewer.pal(3,"Set1")[Data7atrain$default])
aov_SBAGuaranteedApproval_Default<-aov(Data7atrain$SBAGuaranteedApproval~Data7atrain$default)
summary(aov_SBAGuaranteedApproval_Default)
dev.off()

# DeliveryMethod vs Default
par(mfrow=c(1,2))
plot(Data7atrain$DeliveryMethod,col=brewer.pal(3,"Set1")[Data7atrain$default],pch=20,main="DeliveryMethod against Default",ylab = "DeliveryMethod")
boxplot(Data7atrain$DeliveryMethod~Data7atrain$default,main="DeliveryMethod against Default",xlab = "Default",ylab = "DeliveryMethod",col=brewer.pal(3,"Set1")[Data7atrain$default])
aov_DeliveryMethod_Default<-aov(Data7atrain$DeliveryMethod~Data7atrain$default)
summary(aov_DeliveryMethod_Default)
dev.off()

# ApprovalFiscalYear vs Default
par(mfrow=c(1,2))
plot(Data7atrain$ApprovalFiscalYear,col=brewer.pal(3,"Set1")[Data7atrain$default],pch=20,main="ApprovalFiscalYear against Default",ylab = "ApprovalFiscalYear")
boxplot(Data7atrain$DeliveryMethod~Data7atrain$default,main="DeliveryMethod against Default",xlab = "Default",ylab = "ApprovalFiscalYear",col=brewer.pal(3,"Set1")[Data7atrain$default])
aov_ApprovalFiscalYear_Default<-aov(Data7atrain$ApprovalFiscalYear~Data7atrain$default)
summary(aov_ApprovalFiscalYear_Default)
dev.off()

#TermInMonths vs Default
par(mfrow=c(1,2))
plot(Data7atrain$TermInMonths,col=brewer.pal(3,"Set1")[Data7atrain$default],pch=20,main="TermInMonths against Default",ylab = "TermInMonths")
boxplot(Data7atrain$TermInMonths~Data7atrain$default,main="TermInMonths against Default",xlab = "Default",ylab = "TermInMonths",col=brewer.pal(3,"Set1")[Data7atrain$default])
aov_TermInMonths_Default<-aov(Data7atrain$TermInMonths~Data7atrain$default)
summary(aov_TermInMonths_Default)
dev.off()

#InitialInterestRate vs Default
par(mfrow=c(1,2))
plot(Data7atrain$InitialInterestRate,col=brewer.pal(3,"Set1")[Data7atrain$default],pch=20,main="InitialInterestRate against Default",ylab = "InitialInterestRate")
boxplot(Data7atrain$InitialInterestRate~Data7atrain$default,main="InitialInterestRate against Default",xlab = "Default",ylab = "InitialInterestRate",col=brewer.pal(3,"Set1")[Data7atrain$default])
aov_InitialInterestRate_Default<-aov(Data7atrain$InitialInterestRate~Data7atrain$default)
summary(aov_InitialInterestRate_Default)
dev.off()

#BusinessType vs Default
par(mfrow=c(1,2))
plot(Data7atrain$BusinessType,col=brewer.pal(3,"Set1")[Data7atrain$default],pch=20,main="BusinessType against Default",ylab = "BusinessType")
dev.off()

#LoanStatus vs Default
par(mfrow=c(1,2))
plot(Data7atrain$LoanStatus,col=brewer.pal(3,"Set1")[Data7atrain$default],pch=20,main="LoanStatus against Default",ylab = "LoanStatus",xlab = "Default")
dev.off()

#GrossChargeOffAmount vs Default
par(mfrow=c(1,2))
plot(Data7atrain$GrossChargeOffAmount,col=brewer.pal(3,"Set1")[Data7atrain$default],pch=20,main="GrossChargeOffAmount against Default",ylab = "GrossChargeOffAmount",xlab = "Default")
boxplot(Data7atrain$GrossChargeOffAmount~Data7atrain$default,main="GrossChargeOffAmount against Default",xlab = "Default",ylab = "GrossChargeOffAmount",col=brewer.pal(3,"Set1")[Data7atrain$default])
aov_GrossChargeOffAmount_Default<-aov(Data7atrain$GrossChargeOffAmount~Data7atrain$default)
summary(aov_GrossChargeOffAmount_Default)
dev.off()

#RevolverStatus vs Default
par(mfrow=c(1,2))
plot(Data7atrain$RevolverStatus,col=brewer.pal(3,"Set1")[Data7atrain$default],pch=20,main="RevolverStatus against Default",ylab = "RevolverStatus",xlab = "Default")
boxplot(Data7atrain$RevolverStatus~Data7atrain$default,main="RevolverStatus against Default",xlab = "Default",ylab = "RevolverStatus",col=brewer.pal(3,"Set1")[Data7atrain$default])
aov_RevolverStatus_Default<-aov(Data7atrain$RevolverStatus~Data7atrain$default)
summary(aov_RevolverStatus_Default)
dev.off()

#JobsSupported vs Default
par(mfrow=c(1,2))
plot(Data7atrain$JobsSupported,col=brewer.pal(3,"Set1")[Data7atrain$default],pch=20,main="JobsSupported against Default",ylab = "JobsSupported",xlab = "Default")
boxplot(Data7atrain$JobsSupported~Data7atrain$default,main="JobsSupported against Default",xlab = "Default",ylab = "JobsSupported",col=brewer.pal(3,"Set1")[Data7atrain$default])
aov_JobsSupported_Default<-aov(Data7atrain$JobsSupported~Data7atrain$default)
summary(aov_JobsSupported_Default)
dev.off()

#SMOTE
prop.table(table(Data7a$default))
prop.table(table(Data7atrain$default))
prop.table(table(Data7atest$default))
## The train and test data have almost same percentage of default as the base data
## Apply SMOTE on Training and test data set
?SMOTE
Data7atrain$AsOfDate<- NULL
Data7atrain$BorrName<-NULL
Data7atest$AsOfDate<- NULL
Data7atest$BorrName<-NULL
Data7atrainSMOTE<-SMOTE(default~., Data7atrain, perc.over = 250,perc.under = 150)

Data7atestSMOTE<-SMOTE(default~., Data7atest, perc.over = 250,perc.under = 150)

write.csv(Data7atrainSMOTE, "Data7atrain.csv")
write.csv(Data7atestSMOTE, "Data7atest.csv")

ifelse(n <- sapply(Data7atrainSMOTE, function(x) length(levels(x))) == 1, "DROP", "NODROP")


Data7atrainSMOTE<-Data7atrainSMOTE[c(16,2,3,7,8,15,13,14,1,4:6,9:12)]
Data7atestSMOTE<-Data7atestSMOTE[c(16,2,3,7,8,15,13,14,1,4:6,9:12)]

#model 1

LM1_Train <-glm(Data7atrainSMOTE$default~ Data7atrainSMOTE$GrossChargeOffAmount+
                  Data7atrainSMOTE$TermInMonths+ 
                  Data7atrainSMOTE$InitialInterestRate + 
                  Data7atrainSMOTE$SBAGuaranteedApproval+ 
                  Data7atrainSMOTE$JobsSupported+ 
                  Data7atrainSMOTE$RevolverStatus, data = Data7atrainSMOTE, family = binomial(link = 'logit'))
summary(LM1_Train)
vif(LM1_Train)
AIC(LM1_Train)

Predict_1 = predict(LM1_Train, type="response", newdata=Data7atestSMOTE)
table(Data7atrainSMOTE$default, Predict_1 > 0.5)

odds1<-exp(coef(LM1_Train)) # odds ratio
write.csv(odds1,"odds1.csv")
read.csv("odds1.csv")
Probability=odds1/(1+odds1)
Probability

predictROC = predict(LM1_Train, newdata = Data7atestSMOTE)
pred = prediction(predictROC, Data7atrainSMOTE$default)
perf = performance(pred, "tpr", "tnr")
plot(perf)
dev.off()

print(performance(pred,"auc")@y.values[[1]])


#Model 2
LM2_Train <-glm(Data7atrainSMOTE$default~ Data7atrainSMOTE$TermInMonths+ 
                  Data7atrainSMOTE$InitialInterestRate + 
                  Data7atrainSMOTE$SBAGuaranteedApproval+ 
                  Data7atrainSMOTE$JobsSupported+ Data7atrainSMOTE$RevolverStatus, 
                data = Data7atrainSMOTE, family = binomial(link = 'logit'))
summary(LM2_Train)
vif(LM2_Train)
AIC(LM2_Train)

Predict_2 = predict(LM2_Train, type="response", newdata=Data7atestSMOTE)
table(Data7atrainSMOTE$default, Predict_2 > 0.5)

odds2<-exp(coef(LM2_Train)) # odds ratio
write.csv(odds2,"odds2.csv")
read.csv("odds2.csv")
Probability2=odds2/(1+odds2)
Probability2

predictROC = predict(LM2_Train, newdata = Data7atestSMOTE)
pred = prediction(predictROC, Data7atrainSMOTE$default)
perf = performance(pred, "tpr", "tnr")
plot(perf)
dev.off()

print(performance(pred,"auc")@y.values[[1]])

#Model 3
LM3_Train <-glm(Data7atrainSMOTE$default~ Data7atrainSMOTE$TermInMonths+ 
                  Data7atrainSMOTE$InitialInterestRate + Data7atrainSMOTE$SBAGuaranteedApproval
                + Data7atrainSMOTE$JobsSupported, data = Data7atrain, family = binomial(link = 'logit'))
summary(LM3_Train)

vif(LM3_Train)
AIC(LM3_Train)
Predict_3 = predict(LM3_Train, type="response", newdata=Data7atestSMOTE)
table(Data7atrainSMOTE$default, Predict_3 > 0.5)

odds3<-exp(coef(LM3_Train)) # odds ratio
write.csv(odds3,"odds3.csv")
read.csv("odds3.csv")
Probability3=odds3/(1+odds3)
Probability3

predictROC = predict(LM3_Train, newdata = Data7atestSMOTE)
pred = prediction(predictROC, Data7atrainSMOTE$default)
perf = performance(pred, "tpr", "tnr")

plot(perf)
dev.off()

print(performance(pred,"auc")@y.values[[1]])


#Log Likelihood results and pseudo R-Square

lrtest(LM1_Train)
lrtest(LM2_Train)
lrtest(LM3_Train)


pR2(LM1_Train)
pR2(LM2_Train)
pR2(LM3_Train)

