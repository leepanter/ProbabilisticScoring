# functionsDataImportProcessing.R

# ## phq9DataSubsetImport
#
# - File Dependencies:
#   - PHQ9subset.csv
# - subsetted PHQ9 data exported from Excel
#
# - File Exports:
#   - phq9Subset.Rdata
# - Rdata file containing final formatted variables
# - datin.Rdata
# - Redundant Rdata file for experimentation and nomenclature convinience
#
# - Goals:
#   - Save important variables from full PHQ9 data for analysis
# - Calculate and integrate variables necessary for downstream analysis
#
# - Processes:
#   - Reduces data to Question Responses with values 0-3
# - Calculates sum of responses for each subject
# - Adds a string identifier for each question

# phq9DataSubsetImport.R

# Imports PHQ9 Data Subst from original data set

WD="/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/DataManagement/PHQ9"
setwd(WD)

####	Libraries and Prelims	 ####
set.seed(123)

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

Qnum=c(1,2,3,4,5,6,7,8,9)
Qstring=c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9")

Anum=c(0,1,2,3)
Astring=c("0", "1", "2", "3")

tot.vec=c()
for(i in 1:2495){
  tot.vec[i]=sum(unlist(resp.list[[i]]))
}

dat.in$qTot=tot.vec

####	Addition of Supervision Variable	 ####

# Integrating outcome classifiers

# C1 is for  qTot < 7
# C2 is for 7 <= qTot < 10
# C3 is for 10 <= qTot

ClassString=c("C1", "C2", "C3")
ClassNum=c(1,2,3)

dat.in$sumClassString=NA_character_
dat.in$sumClassNum=NA_integer_

dat.in[which(dat.in$qTot < 7),13]=ClassString[1]
dat.in[which(dat.in$qTot < 7),14]=ClassNum[1]
dat.in[which(7 <= dat.in$qTot & dat.in$qTot < 10),13] = ClassString[2]
dat.in[which(7 <= dat.in$qTot & dat.in$qTot < 10),14] = ClassNum[2]
dat.in[which(10 <= dat.in$qTot),13]=ClassString[3]
dat.in[which(10 <= dat.in$qTot),14]=ClassNum[3]

sum.val=c(5,6,7,8,9,10,11,12)

perc.index.val=c(round( length(which(dat.in$qTot==5))/2495,digits=4),
                 round( length(which(dat.in$qTot==6))/2495,digits=4),
                 round( length(which(dat.in$qTot==7))/2495,digits=4),
                 round( length(which(dat.in$qTot==8))/2495,digits=4),
                 round( length(which(dat.in$qTot==9))/2495,digits=4),
                 round(length(which(dat.in$qTot==10))/2495,digits=4),
                 round(length(which(dat.in$qTot==11))/2495,digits=4),
                 round(length(which(dat.in$qTot==12))/2495,digits=4))

perm.vals=data.frame(sum.val, perc.index.val)

sample.perc=c()

sample.perc[1]=perm.vals$perc.index.val[1]/4
sample.perc[2]=perm.vals$perc.index.val[2]/3
sample.perc[3]=perm.vals$perc.index.val[3]/2
sample.perc[4]=perm.vals$perc.index.val[4]/3
sample.perc[5]=perm.vals$perc.index.val[5]/3
sample.perc[6]=perm.vals$perc.index.val[6]/2
sample.perc[7]=perm.vals$perc.index.val[7]/3
sample.perc[8]=perm.vals$perc.index.val[8]/4

perm.vals$sample.perc= round(sample.perc, digits = 4)

n.obs=c()

n.obs[1]=length(which(dat.in$qTot==5))
n.obs[2]=length(which(dat.in$qTot==6))
n.obs[3]=length(which(dat.in$qTot==7))
n.obs[4]=length(which(dat.in$qTot==8))
n.obs[5]=length(which(dat.in$qTot==9))
n.obs[6]=length(which(dat.in$qTot==10))
n.obs[7]=length(which(dat.in$qTot==11))
n.obs[8]=length(which(dat.in$qTot==12))

perm.vals$n.obs=n.obs


perm.vals$sample.nos=NA_integer_

for(i in 1:8){
  perm.vals$sample.nos[i] =
    round(perm.vals$sample.perc[i]*perm.vals$n.obs[i], digits = 0)
}


# index.sample.5s=sample(which(dat.in$qTot==5) , 2, replace=FALSE)
# index.sample.6s=sample(which(dat.in$qTot==6) , 3, replace=FALSE)
# index.sample.7s=sample(which(dat.in$qTot==7) , 2, replace=FALSE)
# index.sample.8sdown=sample(which(dat.in$qTot==8) , 1, replace=FALSE)
# index.sample.8sup=sample(which(dat.in$qTot==8) , 1, replace=FALSE)
# index.sample.9sdown=sample(which(dat.in$qTot==9) , 1, replace=FALSE)
# index.sample.9sup=sample(which(dat.in$qTot==9) , 1, replace=FALSE)
# index.sample.10s=sample(which(dat.in$qTot==10) , 1, replace=FALSE)
# index.sample.11s=sample(which(dat.in$qTot==11) , 1, replace=FALSE)
# index.sample.12s=sample(which(dat.in$qTot==12) , 1, replace=FALSE)


index.sample.5s=sample(which(dat.in$qTot==5) , 2*21, replace=FALSE)
index.sample.6s=sample(which(dat.in$qTot==6) , 3*21, replace=FALSE)
index.sample.7s=sample(which(dat.in$qTot==7) , 2*21, replace=FALSE)
index.sample.8sdown=sample(which(dat.in$qTot==8) , 1*21, replace=FALSE)
index.sample.8sup=sample(which(dat.in$qTot==8) , 1*21, replace=FALSE)
index.sample.9sdown=sample(which(dat.in$qTot==9) , 1*21, replace=FALSE)
index.sample.9sup=sample(which(dat.in$qTot==9) , 1*21, replace=FALSE)
index.sample.10s=sample(which(dat.in$qTot==10) , 1*21, replace=FALSE)
index.sample.11s=sample(which(dat.in$qTot==11) , 1*21, replace=FALSE)
index.sample.12s=sample(which(dat.in$qTot==12) , 1*21, replace=FALSE)




dat.in$SupOutString=dat.in$sumClassString
dat.in$SupOutNum=dat.in$sumClassNum

dat.in$SupOutNum[index.sample.5s]=ClassNum[2]
dat.in$SupOutString[index.sample.5s]=ClassString[2]

dat.in$SupOutNum[index.sample.6s]=ClassNum[2]
dat.in$SupOutString[index.sample.6s]=ClassString[2]

dat.in$SupOutNum[index.sample.7s]=ClassNum[1]
dat.in$SupOutString[index.sample.7s]=ClassString[1]

dat.in$SupOutNum[index.sample.8sdown]=ClassNum[1]
dat.in$SupOutString[index.sample.8sdown]=ClassString[1]

dat.in$SupOutNum[index.sample.8sup]=ClassNum[3]
dat.in$SupOutString[index.sample.8sup]=ClassString[3]

dat.in$SupOutNum[index.sample.9sdown]=ClassNum[1]
dat.in$SupOutString[index.sample.9sdown]=ClassString[1]

dat.in$SupOutNum[index.sample.9sup]=ClassNum[3]
dat.in$SupOutString[index.sample.9sup]=ClassString[3]

dat.in$SupOutNum[index.sample.10s]=ClassNum[2]
dat.in$SupOutString[index.sample.10s]=ClassString[2]

dat.in$SupOutNum[index.sample.11s]=ClassNum[2]
dat.in$SupOutString[index.sample.11s]=ClassString[2]

dat.in$SupOutNum[index.sample.12s]=ClassNum[2]
dat.in$SupOutString[index.sample.12s]=ClassString[2]

phq9Subset=dat.in

save(phq9Subset, file = "phq9Subset.Rdata")

phq9=phq9Subset


rm(dat.in)
rm(i)
rm(WD)
rm(index.sample.10s)
rm(index.sample.11s)
rm(index.sample.12s)
rm(index.sample.5s)
rm(index.sample.6s)
rm(index.sample.7s)
rm(index.sample.8sdown)
rm(index.sample.8sup)
rm(index.sample.9sdown)
rm(index.sample.9sup)
rm(perm.vals)
rm(phq9Subset)
rm(n.obs)
rm(perc.index.val)
rm(sample.perc)
rm(sum.val)












# functionCVsplit.R

# Description: This script defines the function CVsplit, which takes as arguments:

####	Arguments
# dat.in: Data set to partition
# N.set: Number of sets (total, including test set) into which to partition data

####  Return (Type=list())
# format: list(dat.train.out, dat.test.out, N.obs)

# out.list <-- Contains the elements below
# dat.train.out: a list of training data sets (in order of selection)
# dat.test.out: a list of testing data sets (in order of selection)
# N.obs: a numerical value indicating the number of observations in a training/test set

####  Call:
#  obj = CVsplit(mydat, N)



####	Begin Script	 ####
CVsplit=function(dat.in, N.set){

  if(N.set==2496)
  {return(list(dat.in))}

  else{
    N.dat.in.rows=dim(dat.in)[1]
    N.dat.in.cols=dim(dat.in)[2]
    N.obs=floor(N.dat.in.rows/N.set)
    i=1
    index.sample.set=1:N.dat.in.rows
    index.fixed.set=1:N.dat.in.rows

    index.vec=c()
    index.list=list()
    dat.train.out=list()
    dat.test.out=list()

    while(i<=N.set){
      index.list[[i]]=sample(index.sample.set, N.obs, replace = FALSE)
      index.vec=append(index.vec, unique(index.list[[i]]))
      dat.test.out[[i]]=dat.in[index.list[[i]],]
      dat.train.out[[i]]=dat.in[-index.list[[i]],]
      index.sample.set=index.fixed.set[-index.vec[i]]
      i=i+1
    }

    out.list=list(dat.train.out,
                  dat.test.out,
                  N.obs)
    return(out.list)
  }
}
####	End Script	 ####