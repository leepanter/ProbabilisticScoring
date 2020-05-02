####	MasterCVanalysis2.R	 ####

#-------------------------------------------------------------------------#
####	Description:	 ####
# This script will combine the constituent functions, data, and variables needed in order to perform a cross-validation examination of Probabilistic Scoring.


####	Script Dependencies	 ####

# Package Dependencies:
library(ggplot2)
library(boot)
library(reshape)

# Set Working Directory
WD="/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis"
# WD="/home/lpanter/PScoreUpdate1/GCloudUpdate"
setwd(WD)

# Data Dependencies:
# PHQ9 Data
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/DataManagement/PHQ9/phq9DataSubsetImport.R")
# source(file = "phq9DataSubsetImport.R")

# Variable Dependencies:
set.seed(123)
options(warn = -1)

## File Dependencies
# CVInitialSetup.R
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis/CVInitialSetups.R")
# source(file="CVInitialSetups.R")


## functions:
# Weight Calculations
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis/functionsWeightCalculations.R")
# source(file = "functionsWeightCalculations.R")

# Probabilistic Score Calculations
source(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis/functionsProbScoreCalc.R")
# source(file = "functionsProbScoreCalc.R")

# Scoring Analysis
source(file="/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis/functionsScoringAnalysis.R")
# source(file="functionsScoringAnalysis.R")
#-------------------------------------------------------------------------#
####	Begin Script	 ####
#-------------------------------------------------------------------------#



#-------------------------------------------------------------------------#
####	Create Bootsrap Sample Distributions	 ####
#-------------------------------------------------------------------------#
## This portion of code will subsample the given data.  Each subsample will include all colums of data, and the number of rows in each sub-sample will range from one to 2495.  Each subsample will be stored in its own list item so that a subsample distribution of weights may be generated in the following step.


# number.samples=100
# sample.length=number.samples+2
# df.set.info=df.train.set.info
# colnames(df.set.info)=c("df.k.sets", "N.obs.train.set")
# sample.vec.k.sets=df.set.info$df.k.sets
# sample.vec.k.sets=sample.vec.k.sets[-c(1,1245)]
# N.set.arg=sort(sample(sample.vec.k.sets, number.samples, replace = FALSE))
# N.set.arg=sort(c(N.set.arg,1247, 1248))

df.set.info=df.k.final
colnames(df.set.info)=c("df.k.sets", "N.obs.train", "N.obs.test" )

sample.length=100
N.set.arg=df.set.info$df.k.sets

boot.sample.i=list()
for(i in 1:sample.length){
  boot.sample.i[[i]]=CVsplit(phq9, N.set.arg[i])
}

boot.i.train.length=c()
for(i in 1:sample.length){
  boot.i.train.length[i]=boot.sample.i[[i]][[3]]
}

boot.sample.train.i=list()
boot.sample.test.i=list()
boot.sample.Nset.i=list()
for(i in 1:sample.length){
  boot.sample.train.i[[i]]=boot.sample.i[[i]][[1]]
  boot.sample.test.i[[i]] =boot.sample.i[[i]][[2]]
  boot.sample.Nset.i[[i]] =boot.sample.i[[i]][[3]]
}

#-------------------------------------------------------------------------#
####	Weight Bootstrap Distributions	 ####
#-------------------------------------------------------------------------#
## We will now perform the weight calculations for each of the bootstrap subsample.

boot.weights.i=list()
for(i in 1:sample.length){
  boot.weights.i.j=list()
  for(j in 1:N.set.arg[i]){
    boot.weights.i.j[[j]]=ReformatWeights(PCVeval_overQnum(boot.sample.train.i[[i]][[j]]))
    for(k in 1:3){
      boot.weights.i.j[[j]][[k]]=round(boot.weights.i.j[[j]][[k]], digits = 4)
    }
  }
  boot.weights.i[[i]]=boot.weights.i.j
}

length.boot.weights=c()
for(i in 1:sample.length){
  length.boot.weights[i]=length(boot.weights.i[[i]])
}

# we now re-format these weights so that a distribution for each weight-variable may be obtained.
weight.out.i=list()
for(i in 1:sample.length){
  weight.out.i.k=list()
  for(k in 1:3){
    weight.out.i.k.j=list()
    for(j in 1:4){
      weight.out.i.k.j.l=list()
      for(l in 1:9){
        weight.out.i.k.j.l.h=c()
        for(h in 1:N.set.arg[i]){
          weight.out.i.k.j.l.h=append(weight.out.i.k.j.l.h,
                                      boot.weights.i[[i]][[h]][[k]][l,j])
        }
        weight.out.i.k.j.l[[l]]=weight.out.i.k.j.l.h
      }
      weight.out.i.k.j[[j]]=weight.out.i.k.j.l
    }
    weight.out.i.k[[k]]=weight.out.i.k.j
  }
  weight.out.i[[i]]=weight.out.i.k
}

# We now calculate the average of each weight distribution at each level of training observation number
mean.weight.i=list()
for(i in 1:sample.length){
  mean.weight.i.j=list()
  for(j in 1:3){
    mean.weight.i.j.k=list()
    for(k in 1:4){
      mean.weight.i.j.k.l=c()
      for(l in 1:9){
        mean.weight.i.j.k.l[l]=mean(weight.out.i[[i]][[j]][[k]][[l]])
      }
      mean.weight.i.j.k[[k]]=mean.weight.i.j.k.l
    }
    mean.weight.i.j[[j]]=mean.weight.i.j.k
  }
  mean.weight.i[[i]]=mean.weight.i.j
}

CVk_train.weights=list()

for(k in 1:sample.length){
  CVk_j.train.weights=list()
  for(j in 1:3){
    CVk_j_i.train.weights=list()
    for(i in 1:4){
      CVk_j_i.train.weights[[i]]=round(mean.weight.i[[k]][[j]][[i]], digits=4)
    }
    CVk_j.train.weights[[j]]=CVk_j_i.train.weights
  }
  CVk_train.weights[[k]]=CVk_j.train.weights
}

CVk.train.weights.Reformat=list()
for(k in 1:sample.length){
  C1=data.frame()
  C2=data.frame()
  C3=data.frame()
  for(i in 1:9){
    for(j in 1:4){
      C1[i,j]=CVk_train.weights[[1]][[1]][[j]][i]
      C2[i,j]=CVk_train.weights[[1]][[2]][[j]][i]
      C3[i,j]=CVk_train.weights[[1]][[3]][[j]][i]
    }

    colnames(C1)=c("A0","A1","A2","A3")
    colnames(C2)=c("A0","A1","A2","A3")
    colnames(C3)=c("A0","A1","A2","A3")

    CVk_j.train.weights.Reformat=list(C1, C2, C3)
  }
  CVk.train.weights.Reformat[[k]]=CVk_j.train.weights.Reformat
}

ESeqSub=list()
for(i in 1:sample.length){
  ESeqSub_sample=list()
  for(j in 1:length.boot.weights[i]){
    ESeqSub_sample[[j]]=EvalSeqSubject(CVk.train.weights.Reformat[[i]],boot.sample.i[[i]][[2]][[j]],1)
  }
  ESeqSub[[i]]=ESeqSub_sample
}

CVk.probClasses.Convg=list()
for(i in 1:sample.length){
  CVk.probClasses.Convg_sample=list()
  for(j in 1:length.boot.weights[i]){
    CVk.probClasses.Convg_sample_Nset=list()
    for(k in 1:boot.sample.Nset.i[[i]]){
      CVk.probClasses.Convg_sample_Nset[[k]]=convg(ESeqSub[[i]][[j]][[k]], 0.75)
    }
    CVk.probClasses.Convg_sample[[j]]=CVk.probClasses.Convg_sample_Nset
  }
  CVk.probClasses.Convg[[i]]=CVk.probClasses.Convg_sample
}

unlist.boot.sample.Nset=unlist(boot.sample.Nset.i)
Class.out.Sup3=list()
for(i in 1:sample.length){
 Class.out.Sup3_sample=list()
 for(j in 1:length.boot.weights[i]){
   Class.out.Sup3_sample_k=c()
   for(k in 1:unlist.boot.sample.Nset[i]){
     Class.out.Sup3_sample_k[k]=CVk.probClasses.Convg[[i]][[j]][[k]][[3]]
   }
   Class.out.Sup3_sample[[j]]=Class.out.Sup3_sample_k
 }
 Class.out.Sup3[[i]]=Class.out.Sup3_sample
}

####  We will now determine the P-score algorithm class outcome as determined normally

Pscore.CVk.train.weights=list()
for(i in 1:sample.length){
  Pscore.CVk.train.weights_sample=list()
  for(j in 1:length.boot.weights[i]){
    Pscore.CVk.train.weights_sample[[j]]=ReformatWeights(PCVeval_overQnum(boot.sample.train.i[[i]][[j]]))
  }
  Pscore.CVk.train.weights[[i]]=Pscore.CVk.train.weights_sample
}

Pscore.CVk.train.weights.round=list()
for(i in 1:sample.length){
  Pscore.CVk.train.weights.round_sample=list()
  for(j in length.boot.weights[i]){
    Pscore.CVk.train.weights.round_sample_class=list()
    for(k in 1:3){
      Pscore.CVk.train.weights.round_sample_class[[k]]=round(Pscore.CVk.train.weights[[i]][[j]][[k]], digits = 4)
    }
    Pscore.CVk.train.weights.round_sample[[j]]=Pscore.CVk.train.weights.round_sample_class
  }
  Pscore.CVk.train.weights.round[[i]]=Pscore.CVk.train.weights.round_sample
}

Pscore.CVK.probSequences=list()
for(i in 1:sample.length){
  Pscore.CVK.probSequences_sample=list()
  for(j in 1:length.boot.weights[i]){
    Pscore.CVK.probSequences_sample[[j]]=EvalSeqSubject(Pscore.CVk.train.weights[[i]][[j]],boot.sample.test.i[[i]][[j]],1)
  }
  Pscore.CVK.probSequences[[i]]=Pscore.CVK.probSequences_sample
}

Pscore.CVk.probClasses.Convg=list()
unlist.boot.sample.Nset=unlist(boot.sample.Nset.i)
for(i in 1:sample.length){
  Pscore.CVk.probClasses.Convg_sample=list()
  for(j in 1:length.boot.weights[i]){
    Pscore.CVk.probClasses.Convg_sample_k=list()
    for(k in 1:unlist.boot.sample.Nset[i]){
      Pscore.CVk.probClasses.Convg_sample_k[[k]]=convg(Pscore.CVK.probSequences[[i]][[j]][[k]], 0.75)
    }
    Pscore.CVk.probClasses.Convg_sample[[j]]=Pscore.CVk.probClasses.Convg_sample_k
  }
  Pscore.CVk.probClasses.Convg[[i]]=Pscore.CVk.probClasses.Convg_sample
}

Class.out.Pscore=list()
for(i in 1:sample.length){
  Class.out.Pscore_sample=list()
  for(j in 1:length.boot.weights[i]){
    Class.out.Pscore_sample_k=c()
    for(k in 1:unlist.boot.sample.Nset[i]){
      Class.out.Pscore_sample_k[k]=Pscore.CVk.probClasses.Convg[[i]][[j]][[k]][[3]]
    }
    Class.out.Pscore_sample[[j]]=Class.out.Pscore_sample_k
  }
  Class.out.Pscore[[i]]=Class.out.Pscore_sample
}



Class.out.Pscore_sample=list()
Class.out.Sup3_sample=list()
for(i in 1:sample.length){
  Class.out.Pscore_sample[[i]]=unlist(Class.out.Pscore[[i]])
  Class.out.Sup3_sample[[i]]=unlist(Class.out.Sup3[[i]])
}

accuracy.Sup3=c()
for(i in 1:sample.length){
  accuracy.Sup3[i]=length(which(Class.out.Pscore_sample[[i]]==Class.out.Sup3_sample[[i]]))/length(Class.out.Sup3_sample[[i]])
}


plot(accuracy.Sup3~length.boot.weights)

df=data.frame(accuracy.Sup3, length.boot.weights)

write.csv(df, file = "/Users/lee/Desktop/df_acc_100_sup2.csv")

lmod=lm(accuracy.Sup3~length.boot.weights, data = df)
lmods=summary(lmod)

plot(accuracy.Sup3~length.boot.weights,
     xlab="Training Data Length",
     ylab = "Accuracy vs Probabilistically Outcom SUP2")
abline(a=lmods$coefficients[1,1], b=lmods$coefficients[2,1])









# We will now calculate the variation of each value weight value at each level of training observation number
var.weight.i=list()
for(i in 1:sample.length){
  var.weight.i.j=list()
  for(j in 1:3){
    var.weight.i.j.k=list()
    for(k in 1:4){
      var.weight.i.j.k.l=c()
      for(l in 1:9){
        var.weight.i.j.k.l[l]=var(weight.out.i[[i]][[j]][[k]][[l]])
      }
      var.weight.i.j.k[[k]]=var.weight.i.j.k.l
    }
    var.weight.i.j[[j]]=var.weight.i.j.k
  }
  var.weight.i[[i]]=var.weight.i.j
}



## Choose a couple of Variance sequences to plot:
### in all three classes
###  Responses 2 and  4,
### for questions 3, 6, and 9 in

# X.Class.Response.Qnum
X.1.2.3=c()
X.1.2.6=c()
X.1.2.9=c()
X.1.4.3=c()
X.1.4.6=c()
X.1.4.9=c()
X.2.2.3=c()
X.2.2.6=c()
X.2.2.9=c()
X.2.4.3=c()
X.2.4.6=c()
X.2.4.9=c()
X.3.2.3=c()
X.3.2.6=c()
X.3.2.9=c()
X.3.4.3=c()
X.3.4.6=c()
X.3.4.9=c()

# var.weight.i[[Nsetval]][[class]][[response]][[qnumber]]

for(i in 1:n.minus.one+1){
  X.1.2.3[i]=var.weight.i[[i]][[1]][[2]][3]
  X.1.2.6[i]=var.weight.i[[i]][[1]][[2]][6]
  X.1.2.9[i]=var.weight.i[[i]][[1]][[2]][9]
  X.1.4.3[i]=var.weight.i[[i]][[1]][[4]][3]
  X.1.4.6[i]=var.weight.i[[i]][[1]][[4]][6]
  X.1.4.9[i]=var.weight.i[[i]][[1]][[4]][9]
  X.2.2.3[i]=var.weight.i[[i]][[2]][[2]][3]
  X.2.2.6[i]=var.weight.i[[i]][[2]][[2]][6]
  X.2.2.9[i]=var.weight.i[[i]][[2]][[2]][9]
  X.2.4.3[i]=var.weight.i[[i]][[2]][[4]][3]
  X.2.4.6[i]=var.weight.i[[i]][[2]][[4]][6]
  X.2.4.9[i]=var.weight.i[[i]][[2]][[4]][9]
  X.3.2.3[i]=var.weight.i[[i]][[3]][[2]][3]
  X.3.2.6[i]=var.weight.i[[i]][[3]][[2]][6]
  X.3.2.9[i]=var.weight.i[[i]][[3]][[2]][9]
  X.3.4.3[i]=var.weight.i[[i]][[3]][[4]][3]
  X.3.4.6[i]=var.weight.i[[i]][[3]][[4]][6]
  X.3.4.9[i]=var.weight.i[[i]][[3]][[4]][9]
}
df.var.X=data.frame(rbind(X.1.2.3,
                          X.1.2.6,
                          X.1.2.9,
                          X.1.4.3,
                          X.1.4.6,
                          X.1.4.9,
                          X.2.2.3,
                          X.2.2.6,
                          X.2.2.9,
                          X.2.4.3,
                          X.2.4.6,
                          X.2.4.9,
                          X.3.2.3,
                          X.3.2.6,
                          X.3.2.9,
                          X.3.4.3,
                          X.3.4.6,
                          X.3.4.9))

colnames(df.var.X)=N.set.arg
class.id=c(rep("1", times=6),
           rep("2", times=6),
           rep("3", times=6))
response=rep( c(rep("2", times=3) , rep("4", times=3)), times=3)
qnum=rep(c("3", "6", "9"), times=6)
df.var.X$class.id=class.id
df.var.X$response=response
df.var.X$qnum=qnum
df.var.re=melt(df.var.X, id=c("class.id","response","qnum"))
colnames(df.var.re)=c("class.id", "response", "qnum", "train.count", "variation")
p=ggplot(df.var.re, aes(x=train.count, y=variation))+
  geom_point(aes(shape=class.id, color=qnum))+
  geom_line()
p


# We will now classify new observations based upon the weights generated on various sample sizes

subject.evaluated.sequence=list()
for(i in 1:sample.length){
  subject.evaluated.sequence.Nsetarg=list()
  for(j in 1:N.set.arg[i]){

  }


    EvalSeqSubject(mean.weight.i[[i]], boot.sample.test.i, boot.sample.Nset.i[[i]])
}

#-------------------------------------------------------------------------#
####	End Script	 ####
#-------------------------------------------------------------------------#



#-------------------------------------------------------------------------#
#####	Post-Script	#####

####  Notes:

####  Compilation Errors:

####  Execution Errors:

####  Next Scripts to Consider:

#-------------------------------------------------------------------------#
