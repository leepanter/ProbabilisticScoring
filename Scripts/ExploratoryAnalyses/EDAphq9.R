# EDAphq9.R

# This script performs some basic analysis on the PHQ9 responses

# NOTE: PLEASE RUN phq9DataSubsetImport.R
# Which can be found in the following directory:
#  /Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/DataManagement/PHQ9/phq9DataSubsetImport.R

# Load Packages
library(dplyr)
library(ggplot2)

dat=phq9

resp1=dat$PHQ9_1-1
resp2=dat$PHQ9_2-1
resp3=dat$PHQ9_3-1
resp4=dat$PHQ9_4-1
resp5=dat$PHQ9_5-1
resp6=dat$PHQ9_6-1
resp7=dat$PHQ9_7-1
resp8=dat$PHQ9_8-1
resp9=dat$PHQ9_9-1

respVector=cbind(resp1,
             resp2,
             resp3,
             resp4,
             resp5,
             resp6,
             resp7,
             resp8,
             resp9)
respTot=c()
for(i in 1:2495){
  respTot[i]=sum(respVector[i,1:9])
}
dat$respTot=respTot
dat$SubjectNo=1:2495

g=ggplot(dat, aes(respTot))+
  geom_histogram(bins = 50)+
  geom_hline(yintercept = 0)+
  xlab("PHQ9 Total Score")+
  ylab("Frequency")
g

age=dat$Age

g=ggplot(dat, aes(age))+
  geom_histogram(bins=50)+
  geom_hline(yintercept = 0)+
  xlab("Age")+
  ylab("Frequency")
g

gender=dat$Gender
gender=as.factor(gender)
dat$gender.factor=gender

g=ggplot(dat, aes(x=gender.factor, y=respTot, fill=gender.factor))+
  geom_violin()+
  xlab("gender label")+
  ylab("Response Total")
g

# We Will try to replicate the first weight value

# We first calculate the probability of answering a "0"
tot.responses=2495*9
response.df=data.frame(respVector)
pr.answer.0 =(length(response.df$resp1[which(response.df$resp1==0)])+
  length(response.df$resp2[which(response.df$resp2==0)])+
  length(response.df$resp3[which(response.df$resp3==0)])+
  length(response.df$resp4[which(response.df$resp4==0)])+
  length(response.df$resp5[which(response.df$resp5==0)])+
  length(response.df$resp6[which(response.df$resp6==0)])+
  length(response.df$resp7[which(response.df$resp7==0)])+
  length(response.df$resp8[which(response.df$resp8==0)])+
  length(response.df$resp9[which(response.df$resp9==0)]))/tot.responses
#0.531329

# Next we subset the data to non-clinically depressed individuals
dat.c0=subset(dat, respTot<7)


# then calculate the probability that these individuals answered a zero on question one
tot.responses.test=1356*9

resp1.test=dat.c0$PHQ9_1-1
resp2.test=dat.c0$PHQ9_2-1
resp3.test=dat.c0$PHQ9_3-1
resp4.test=dat.c0$PHQ9_4-1
resp5.test=dat.c0$PHQ9_5-1
resp6.test=dat.c0$PHQ9_6-1
resp7.test=dat.c0$PHQ9_7-1
resp8.test=dat.c0$PHQ9_8-1
resp9.test=dat.c0$PHQ9_9-1

respVector.test=cbind(resp1.test,
                 resp2.test,
                 resp3.test,
                 resp4.test,
                 resp5.test,
                 resp6.test,
                 resp7.test,
                 resp8.test,
                 resp9.test)

response.test.df=data.frame(respVector.test)
pr.answer.0.test =(length(response.test.df$resp1.test[which(response.test.df$resp1.test==0)])+
                length(response.test.df$resp2.test[which(response.test.df$resp2.test==0)])+
                length(response.test.df$resp3.test[which(response.test.df$resp3.test==0)])+
                length(response.test.df$resp4.test[which(response.test.df$resp4.test==0)])+
                length(response.test.df$resp5.test[which(response.test.df$resp5.test==0)])+
                length(response.test.df$resp6.test[which(response.test.df$resp6.test==0)])+
                length(response.test.df$resp7.test[which(response.test.df$resp7.test==0)])+
                length(response.test.df$resp8.test[which(response.test.df$resp8.test==0)])+
                length(response.test.df$resp9.test[which(response.test.df$resp9.test==0)]))/tot.responses.test


























