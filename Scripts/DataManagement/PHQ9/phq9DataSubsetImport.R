# phq9DataSubsetImport.R

# Imports PHQ9 Data Subst from original data set

WD="/Users/lee/Documents/GitHub/ProbabilisticScoring/Data"
setwd(WD)

phq9=read.csv(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Data/PHQ9subset.csv")

dat.in=phq9

dat.in=dat.in[,2:32]
dat.in=dat.in[,-c(3,4,5,6,7,9,10,11,12,13,14,15,16,17,18,19,20,21)]
dat.in=dat.in[,-13]
colnames(dat.in)[3:12]=c("qTot", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9")

dat.in$Q1=dat.in$Q1-1
dat.in$Q2=dat.in$Q2-1
dat.in$Q3=dat.in$Q3-1
dat.in$Q4=dat.in$Q4-1
dat.in$Q5=dat.in$Q5-1
dat.in$Q6=dat.in$Q6-1
dat.in$Q7=dat.in$Q7-1
dat.in$Q8=dat.in$Q8-1
dat.in$Q9=dat.in$Q9-1

resp.list=list()

for(i in 1:2495){
  resp.list[[i]]=c(dat.in$Q1[i],
                   dat.in$Q2[i],
                   dat.in$Q3[i],
                   dat.in$Q4[i],
                   dat.in$Q5[i],
                   dat.in$Q6[i],
                   dat.in$Q7[i],
                   dat.in$Q8[i],
                   dat.in$Q9[i])
}

Qstring=c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9")

tot.vec=c()
for(i in 1:2495){
  tot.vec[i]=sum(unlist(resp.list[[i]]))
}

dat.in$qTot=tot.vec

phq9Subset=dat.in

save(phq9Subset, file = "phq9Subset.Rdata")

save(dat.in, file = "datin.Rdata")

rm(dat.in)
rm(i)
rm(WD)
