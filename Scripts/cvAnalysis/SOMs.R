# SOMs.R
#-------------------------------------------------------------------------#
####	Description:	 ####



####	Script Dependencies	 ####

# Package Dependencies:
library(factoextra)
library(tidyverse)
library(cluster)
library(dendextend)
library(ggplot2)
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/kohonen/kohonen_2.0.19.tar.gz"
# install.packages(packageurl, repos = NULL, type = "source")
library(kohonen)


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

# load data
dat=phq9
dat.kmeans=dat[,4:12]


#-------------------------------------------------------------------------#
# SOM Clustering ----------------------------------------------------------
#-------------------------------------------------------------------------#

dat.som=scale(dat.kmeans)

#-------------------------------------------------------------------------#
# 12 X 9 grid
#-------------------------------------------------------------------------#

#plot(som.out108, type = 'changes')
#plot(som.out108)
#plot(som.out108, type = 'count')
#plot(hclust(dist(som.out108$codes)))

# seeds 220


set.seed(2220)
out.grid1=somgrid(xdim = 12, ydim=9, topo = "rectangular")
som.out1=som(dat.som, grid = out.grid1, rlen = 1000)
somOut.1=som.out1$unit.classif
class.cuts1=cutree(hclust(dist(som.out1$codes)),3)

ones1=which(class.cuts1==1)
twos1=which(class.cuts1==2)
threes1=which(class.cuts1==3)

classify.cuts.1=function(in.cut){
  if(in.cut %in% ones1){
    out.class="C1"
  }  else if (in.cut %in% twos1){
    out.class="C3"
  } else {out.class="C2"}
  return(out.class)
}


class.som1=c()
for(i in 1:2495){
  class.som1[i]=classify.cuts.1(som.out1$unit.classif[i])
}

dat$class.som1=as.factor(class.som1)

table.som1=dat %>% group_by(class.som1) %>% tally()
table.som1


plot(som.out1, type = 'codes', bgcol = rainbow(3)[class.cuts1])
add.cluster.boundaries(som.out1, class.cuts1)

mean.total.table1=aggregate(x=dat$qTot,
                           by=list(som.out1[["unit.classif"]]),
                           FUN=mean)

median.total.table1=aggregate(x=dat$qTot,
                             by=list(som.out1[["unit.classif"]]),
                             FUN=median)


#-------------------------------------------------------------------------#
# 12 X 9 grid TAXICAB Metric
#-------------------------------------------------------------------------#

#plot(som.out108, type = 'changes')
#plot(som.out108)
#plot(som.out108, type = 'count')
#plot(hclust(dist(som.out108$codes)))

# seeds 220


set.seed(2220)
out.grid1.tx=somgrid(xdim = 12, ydim=9, topo = "rectangular")
som.out1.tx=som(dat.som, grid = out.grid1.tx, rlen = 1000)
somOut.1.tx=som.out1.tx$unit.classif
class.cuts1.tx=cutree(hclust(dist(som.out1.tx$codes,
                                  method = "manhattan")),3)

ones1.tx=which(class.cuts1.tx==1)
twos1.tx=which(class.cuts1.tx==2)
threes1.tx=which(class.cuts1.tx==3)

classify.cuts.1.tx=function(in.cut){
  if(in.cut %in% ones1.tx){
    out.class="C1"
  }  else if (in.cut %in% twos1.tx){
    out.class="C3"
  } else {out.class="C2"}
  return(out.class)
}


class.som1.tx=c()
for(i in 1:2495){
  class.som1.tx[i]=classify.cuts.1.tx(som.out1.tx$unit.classif[i])
}

dat$class.som1.tx=as.factor(class.som1.tx)

table.som1.tx=dat %>% group_by(class.som1.tx) %>% tally()
table.som1.tx


plot(som.out1.tx, type = 'codes', bgcol = rainbow(3)[class.cuts1.tx])
add.cluster.boundaries(som.out1.tx, class.cuts1.tx)

mean.total.table1.tx=aggregate(x=dat$qTot,
                           by=list(som.out1.tx[["unit.classif"]]),
                           FUN=mean)

median.total.table1.tx=aggregate(x=dat$qTot,
                             by=list(som.out1.tx[["unit.classif"]]),
                             FUN=median)






#-------------------------------------------------------------------------#
set.seed(1110)
out.grid2=somgrid(xdim = 15, ydim= 15, topo = "hexagonal")
som.out2=som(dat.som, grid = out.grid2,
            rlen=1000)
somOut2=som.out2$unit.classif
class.cuts2=cutree(hclust(dist(som.out2$codes)),3)

ones2=  which(class.cuts2==1)
twos2=  which(class.cuts2==2)
threes2=which(class.cuts2==3)

classify.cuts2=function(in.cut){
  if(in.cut %in% ones2){
    out.class="C1"
  }  else if (in.cut %in% twos2){
    out.class="C3"
  } else {out.class="C2"}
  return(out.class)
}

class.som2=c()
for(i in 1:2495){
  class.som2[i]=classify.cuts2(som.out2$unit.classif[i])
}

dat$class.som2=as.factor(class.som2)

table.som2=dat %>% group_by(class.som2) %>% tally()
table.som2

plot(som.out2, type = 'codes', bgcol = rainbow(3)[class.cuts2])
add.cluster.boundaries(som.out2, class.cuts2)
plot(som.out2, type = 'changes')

mean.total.table2=aggregate(x=dat$qTot,
                           by=list(som.out2[["unit.classif"]]),
                           FUN=mean)

median.total.table2=aggregate(x=dat$qTot,
                             by=list(som.out2[["unit.classif"]]),
                             FUN=median)

#-------------------------------------------------------------------------#
####  Same as SOM2, but taxicab metric
#-------------------------------------------------------------------------#
set.seed(1110)
out.grid2.tx=somgrid(xdim = 15, ydim= 15, topo = "hexagonal")
som.out2.tx=som(dat.som, grid = out.grid2.tx,
             rlen=1000)
somOut2.tx=som.out2.tx$unit.classif
class.cuts2.tx=cutree(hclust(dist(som.out2.tx$codes,
                                  method = "manhattan")),3)

ones2.tx=  which(class.cuts2.tx==1)
twos2.tx=  which(class.cuts2.tx==2)
threes2.tx=which(class.cuts2.tx==3)

classify.cuts2.tx=function(in.cut){
  if(in.cut %in% ones2.tx){
    out.class="C1"
  }  else if (in.cut %in% twos2.tx){
    out.class="C3"
  } else {out.class="C2"}
  return(out.class)
}

class.som2.tx=c()
for(i in 1:2495){
  class.som2.tx[i]=classify.cuts2.tx(som.out2.tx$unit.classif[i])
}

dat$class.som2.tx=as.factor(class.som2.tx)

table.som2.tx=dat %>% group_by(class.som2.tx) %>% tally()
table.som2.tx

plot(som.out2.tx, type = 'codes', bgcol = rainbow(3)[class.cuts2.tx])
add.cluster.boundaries(som.out2.tx, class.cuts2.tx)
plot(som.out2.tx, type = 'changes')

mean.total.table2.tx=aggregate(x=dat$qTot,
                            by=list(som.out2.tx[["unit.classif"]]),
                            FUN=mean)

median.total.table2.tx=aggregate(x=dat$qTot,
                              by=list(som.out2.tx[["unit.classif"]]),
                              FUN=median)

#-------------------------------------------------------------------------#
####  3X9 Mapping
#-------------------------------------------------------------------------#

set.seed(123)
out.grid3=somgrid(xdim = 3, ydim= 9, topo = "rectangular")

som.out3=som(dat.som, grid = out.grid3,
             rlen=1000)
# dat$somOut=som.out$unit.classif
somOut3=som.out3$unit.classif
class.cuts3=cutree(hclust(dist(som.out3$codes)),3)

ones3=  which(class.cuts3==1)
twos3=  which(class.cuts3==2)
threes3=which(class.cuts3==3)

classify.cuts3=function(in.cut){
  if(in.cut %in% ones3){
    out.class="C1"
  }  else if (in.cut %in% twos3){
    out.class="C3"
  } else {out.class="C2"}
  return(out.class)
}

class.som3=c()
for(i in 1:2495){
  class.som3[i]=classify.cuts3(som.out3$unit.classif[i])
}

dat$class.som3=as.factor(class.som3)

table.som3=dat %>% group_by(class.som3) %>% tally()
table.som3

plot(som.out3, type = 'codes', bgcol = rainbow(3)[class.cuts3])
add.cluster.boundaries(som.out3, class.cuts3)
plot(som.out3, type = 'changes')

mean.total.table3=aggregate(x=dat$qTot,
                           by=list(som.out3[["unit.classif"]]),
                           FUN=mean)

median.total.table3=aggregate(x=dat$qTot,
                             by=list(som.out3[["unit.classif"]]),
                             FUN=median)

median.total.table3test=aggregate(x=dat$Q1,
                              by=list(som.out3[["unit.classif"]]),
                              FUN=median)
median.total.table3test
#-------------------------------------------------------------------------#
####  3X9 Mapping, with taxicab metric
#-------------------------------------------------------------------------#

set.seed(123)
out.grid3.tx=somgrid(xdim = 3, ydim= 9, topo = "rectangular")
som.out3.tx=som(dat.som, grid = out.grid3.tx,
             rlen=1000)
somOut3.tx=som.out3.tx$unit.classif
class.cuts3.tx=cutree(hclust(dist(som.out3.tx$codes,
                                  method = "manhattan")),3)

ones3.tx=  which(class.cuts3.tx==1)
twos3.tx=  which(class.cuts3.tx==2)
threes3.tx=which(class.cuts3.tx==3)

classify.cuts3.tx=function(in.cut){
  if(in.cut %in% ones3.tx){
    out.class="C1"
  }  else if (in.cut %in% twos3.tx){
    out.class="C3"
  } else {out.class="C2"}
  return(out.class)
}

class.som3.tx=c()
for(i in 1:2495){
  class.som3.tx[i]=classify.cuts3.tx(som.out3.tx$unit.classif[i])
}

dat$class.som3.tx=as.factor(class.som3.tx)

table.som3.tx=dat %>% group_by(class.som3.tx) %>% tally()
table.som3.tx

plot(som.out3.tx, type = 'codes', bgcol = rainbow(3)[class.cuts3.tx])
add.cluster.boundaries(som.out3.tx, class.cuts3.tx)
plot(som.out3.tx, type = 'changes')

mean.total.table3.tx=aggregate(x=dat$qTot,
                            by=list(som.out3.tx[["unit.classif"]]),
                            FUN=mean)

median.total.table3.tx=aggregate(x=dat$qTot,
                              by=list(som.out3.tx[["unit.classif"]]),
                              FUN=median)



#-------------------------------------------------------------------------#
# SOM CVk analysis -----------------------------------------------------
#-------------------------------------------------------------------------#

# number.samples=50
# sample.length=number.samples+2
# df.set.info=df.train.set.info
# colnames(df.set.info)=c("df.k.sets", "N.obs.train.set")
# sample.vec.k.sets=df.set.info$df.k.sets
# sample.vec.k.sets=sample.vec.k.sets[-c(1,1245)]
# N.set.arg=sort(sample(sample.vec.k.sets, number.samples, replace = FALSE))
# N.set.arg=sort(c(N.set.arg,1247, 1248))

df.set.info=df.k.final
colnames(df.set.info)=c("df.k.sets", "N.obs.train", "N.obs.test" )
sample.length=50
N.set.arg=sample(df.set.info$df.k.sets, sample.length, replace = FALSE)

accuracy.ksets=c()
traditional.accuracy.ksets=c()
N.obs.k=c()
boot.sample.i=list()

for(i in 1:sample.length){
  boot.sample.i[[i]]=CVsplit(dat, N.set.arg[i])
}

# Initialized Global Environment Empties
training.set.length=c()
test.set.length=c()
N.j=c()
P.score.train.weights=list()

for(i in 1:sample.length){
  # Define Static Variables
  training.set.length[i]=dim(boot.sample.i[[i]][[1]][[1]])[1]
  test.set.length[i]=dim(boot.sample.i[[i]][[2]][[1]])[1]
  N.j[i]=length(boot.sample.i[[i]][[1]])

  # Initialized Fixed-i environment variables
  P.score.train.weights.i=list()

  for(j in 1:N.j[i]){
    P.score.train.weights.i[[j]]=ReformatWeights(PCVeval_overQnum(boot.sample.i[[i]][[1]][[j]]))
  }

  for(k in 1:3){
    P.score.train.weights.i[[j]][[k]]=round(P.score.train.weights.i[[j]][[k]], digits = 4)
  }
  P.score.train.weights[[i]]=P.score.train.weights.i
}

P.score.sequences=list()
for(i in 1:sample.length){
  P.score.sequences.i=list()
  for(j in 1:N.j[i]){
    P.score.sequences.i[[j]]=EvalSeqSubject(P.score.train.weights[[i]][[j]],
                                            boot.sample.i[[i]][[2]][[j]],1)
  }
  P.score.sequences[[i]]=P.score.sequences.i
}

P.score.probClass=list()
for(i in 1:sample.length){
  P.score.probClass.i=list()
  for(j in 1:N.j[i]){
    P.score.probClass.i.j=list()
    for(k in 1:boot.sample.i[[i]][[3]]){
      P.score.probClass.i.j[[k]]=convg(P.score.sequences[[i]][[j]][[k]], 0.75)[[3]]
    }
    P.score.probClass.i[[j]]=P.score.probClass.i.j
  }
  P.score.probClass[[i]]=P.score.probClass.i
}



len.i=c()
for(i in 1:sample.length){
  len.i[i]=dim(boot.sample.i[[i]][[2]])[1]
}

pscore.convert=function(in.arg){
  out.arg=NA
  if(is.na(in.arg)){
    out.arg="NA"
  } else {
    if(in.arg==1){
      out.arg="C1"
    } else if(in.arg==2){
      out.arg="C2"
    } else {
      out.arg="C3"
    }
  }
  return(out.arg)
}

for(i in 1:sample.length){
  outcome.pscore.i=c()
  outcome.pscore.num.i=c()
  for(j in 1:N.j[i]){
    outcome.pscore.i.j=c()
    outcome.pscore.num.i.j=c()
    for(k in 1:length(P.score.probClass[[i]][[j]])){
      outcome.pscore.i.j[k]=pscore.convert(P.score.probClass[[i]][[j]][[k]])
    }
    boot.sample.i[[i]][[2]][[j]]$pscore.factor=NA
    boot.sample.i[[i]][[2]][[j]]$pscore.factor=outcome.pscore.i.j
  }
}


pscore.i=list()
tradit.i=list()
som1   =list()
som2   =list()
som3   =list()
som1.tx=list()
som2.tx=list()
som3.tx=list()
for(i in 1:sample.length){
  pscore.i.j=c()
  tradit.i.j=c()
  som1.i.j   =c()
  som2.i.j   =c()
  som3.i.j   =c()
  som1.i.j.tx=c()
  som2.i.j.tx=c()
  som3.i.j.tx=c()
  for(j in 1:N.j[i]){
    pscore.i.j=append(pscore.i.j,
                      boot.sample.i[[i]][[2]][[j]][["pscore.factor"]])
    tradit.i.j=append(tradit.i.j,
                      boot.sample.i[[i]][[2]][[j]][["SupOutString"]])
    som1.i.j=append(som1.i.j,
                    as.character(boot.sample.i[[i]][[2]][[j]][["class.som1"]]))
    som2.i.j=append(som2.i.j,
                    as.character(boot.sample.i[[i]][[2]][[j]][["class.som2"]]))
    som3.i.j=append(som3.i.j,
                    as.character(boot.sample.i[[i]][[2]][[j]][["class.som3"]]))
    som1.i.j.tx=append(som1.i.j.tx,
                  as.character(boot.sample.i[[i]][[2]][[j]][["class.som1.tx"]]))
    som2.i.j.tx=append(som2.i.j.tx,
                  as.character(boot.sample.i[[i]][[2]][[j]][["class.som2.tx"]]))
    som3.i.j.tx=append(som3.i.j.tx,
                  as.character(boot.sample.i[[i]][[2]][[j]][["class.som3.tx"]]))
  }
  pscore.i[[i]]=pscore.i.j
  tradit.i[[i]]=tradit.i.j
  som1[[i]]   =som1.i.j
  som2[[i]]   =som2.i.j
  som3[[i]]   =som3.i.j
  som1.tx[[i]]=som1.i.j.tx
  som2.tx[[i]]=som2.i.j.tx
  som3.tx[[i]]=som3.i.j.tx
}

acc.pscore.som1   =c()
acc.tradit.som1   =c()
acc.pscore.som2   =c()
acc.tradit.som2   =c()
acc.pscore.som3   =c()
acc.tradit.som3   =c()
acc.pscore.som1.tx=c()
acc.tradit.som1.tx=c()
acc.pscore.som2.tx=c()
acc.tradit.som2.tx=c()
acc.pscore.som3.tx=c()
acc.tradit.som3.tx=c()
length.train=c()


for(i in 1:sample.length){
  acc.pscore.som1[i]=length(which(som1[[i]]==pscore.i[[i]]))/length(pscore.i[[i]])
  acc.tradit.som1[i]=length(which(som1[[i]]==tradit.i[[i]]))/length(pscore.i[[i]])
  acc.pscore.som2[i]=length(which(som2[[i]]==pscore.i[[i]]))/length(pscore.i[[i]])
  acc.tradit.som2[i]=length(which(som2[[i]]==tradit.i[[i]]))/length(pscore.i[[i]])
  acc.pscore.som3[i]=length(which(som3[[i]]==pscore.i[[i]]))/length(pscore.i[[i]])
  acc.tradit.som3[i]=length(which(som3[[i]]==tradit.i[[i]]))/length(pscore.i[[i]])
  acc.pscore.som1.tx[i]=length(which(som1.tx[[i]]==pscore.i[[i]]))/length(pscore.i[[i]])
  acc.tradit.som1.tx[i]=length(which(som1.tx[[i]]==tradit.i[[i]]))/length(pscore.i[[i]])
  acc.pscore.som2.tx[i]=length(which(som2.tx[[i]]==pscore.i[[i]]))/length(pscore.i[[i]])
  acc.tradit.som2.tx[i]=length(which(som2.tx[[i]]==tradit.i[[i]]))/length(pscore.i[[i]])
  acc.pscore.som3.tx[i]=length(which(som3.tx[[i]]==pscore.i[[i]]))/length(pscore.i[[i]])
  acc.tradit.som3.tx[i]=length(which(som3.tx[[i]]==tradit.i[[i]]))/length(pscore.i[[i]])
  length.train[i]=dim(boot.sample.i[[i]][[1]][[1]])[1]
}

accuracy.df.som=data.frame(length.train,
                       acc.pscore.som1,
                       acc.tradit.som1,
                       acc.pscore.som2,
                       acc.tradit.som2,
                       acc.pscore.som3,
                       acc.tradit.som3,
                       acc.pscore.som1.tx,
                       acc.tradit.som1.tx,
                       acc.pscore.som2.tx,
                       acc.tradit.som2.tx,
                       acc.pscore.som3.tx,
                       acc.tradit.som3.tx)

mean.acc.trad=c(mean(acc.tradit.som1),
                mean(acc.tradit.som2),
                mean(acc.tradit.som3),
                mean(acc.tradit.som1.tx),
                mean(acc.tradit.som2.tx),
                mean(acc.tradit.som3.tx))
mean.acc.psco=c(mean(acc.pscore.som1),
                mean(acc.pscore.som2),
                mean(acc.pscore.som3),
                mean(acc.pscore.som1.tx),
                mean(acc.pscore.som2.tx),
                mean(acc.pscore.som3.tx))
mean.acc.trad
mean.acc.psco
#Som2 is the winner

acc.df.som2=data.frame(length.train,
                       acc.pscore.som2,
                       acc.tradit.som2,
                       acc.pscore.som2.tx,
                       acc.tradit.som2.tx)

save(acc.df.som2, file = "/Users/lee/Desktop/Sup3NEW/accDFsom2.Rdata")

write.csv(acc.df.som2, file = "/Users/lee/Desktop/Sup3NEW/accDFsom2.csv")

ones=rep("PS" , times=sample.length)
twos=rep("TR" , times=sample.length)
thre=rep("PSx", times=sample.length)
four=rep("TRx", times=sample.length)

method=as.factor(c(ones,twos, thre, four))
method.PS=as.factor(c(ones, ones, twos, twos))
method.X=as.factor(c(ones, ones, thre, thre))

len.train=rep(length.train, times=4)

acc.som2=c(acc.pscore.som2,
           acc.tradit.som2,
           acc.pscore.som2.tx,
           acc.tradit.som2.tx)

mean(acc.pscore.som2)
mean(acc.tradit.som2)
mean(acc.pscore.som2.tx)
mean(acc.tradit.som2.tx)


df.som2=data.frame(method, len.train, acc.som2)

lmod.som2=lm(acc.som2~len.train*method, data=df.som2)
(lmods.som2=summary(lmod.som2))

coef.lmod.som2=coef(lmod.som2)

(coef.int.1=coef.lmod.som2[1])
(coef.int.2=coef.lmod.som2[1]+coef.lmod.som2[3])
(coef.int.3=coef.lmod.som2[1]+coef.lmod.som2[4])
(coef.int.4=coef.lmod.som2[1]+coef.lmod.som2[5])

(coef.slp.1=coef.lmod.som2[2])
(coef.slp.2=coef.lmod.som2[2]+coef.lmod.som2[6])
(coef.slp.3=coef.lmod.som2[2]+coef.lmod.som2[7])
(coef.slp.4=coef.lmod.som2[2]+coef.lmod.som2[8])


p=ggplot(acc.df.som2, aes(x=length.train))+
  geom_point(aes(y=acc.pscore.som2, color="firebrick"))+
  geom_point(aes(y=acc.tradit.som2, color="hotpink4"))+
  geom_point(aes(y=acc.pscore.som2.tx, color="blue"))+
  geom_point(aes(y=acc.tradit.som2.tx, color="mediumpurple"))+
  geom_abline(intercept = coef.int.1,
              slope = coef.slp.1)+
  geom_abline(intercept = coef.int.2,
              slope = coef.slp.2)+
  geom_abline(intercept = coef.int.3,
              slope = coef.slp.3)+
  geom_abline(intercept = coef.int.4,
              slope = coef.slp.4)+
  theme(legend.position = "none")
p


pPS=ggplot(acc.df.som2, aes(x=length.train))+
  geom_point(aes(y=acc.pscore.som2, color="firebrick"))+
  geom_point(aes(y=acc.tradit.som2, color="hotpink4"))+
  geom_abline(intercept = coef.int.1,
              slope = coef.slp.1)+
  geom_abline(intercept = coef.int.3,
              slope = coef.slp.3)+
  theme(legend.position = "none")
pPS

pX=ggplot(acc.df.som2, aes(x=length.train))+
  geom_point(aes(y=acc.pscore.som2, color="firebrick"))+
  geom_point(aes(y=acc.pscore.som2.tx, color="blue"))+
  geom_abline(intercept = coef.int.2,
              slope = coef.slp.2)+
  # geom_abline(intercept = coef.int.1,
  #             slope = coef.slp.1)+
  theme(legend.position = "none")
pX

pX=ggplot(acc.df.som2, aes(x=length.train))+
  geom_point(aes(y=acc.tradit.som2, color="firebrick"))+
  geom_point(aes(y=acc.tradit.som2.tx, color="blue"))+
  # geom_abline(intercept = coef.int.3,
  #             slope = coef.slp.3)+
  geom_abline(intercept = coef.int.4,
              slope = coef.slp.4)+
  theme(legend.position = "none")
pX


p1=ggplot(accuracy.df.som, aes(x=length.train))+
  geom_point(aes(y=acc.tradit.som1, color="firebrick"), shape=15)+
  geom_point(aes(y=acc.pscore.som1, color="blue"), shape=18)+
  theme(legend.position = "none")
p1

p1.tx=ggplot(accuracy.df.som, aes(x=length.train))+
  geom_point(aes(y=acc.tradit.som1.tx, color="hotpink4"), shape=15)+
  geom_point(aes(y=acc.pscore.som1.tx, color="mediumpurple"), shape=18)+
  theme(legend.position = "none")
p1.tx

p2=ggplot(accuracy.df.som, aes(x=length.train))+
  geom_point(aes(y=acc.tradit.som2, color="firebrick"), shape=15)+
  geom_point(aes(y=acc.pscore.som2.tx, color="mediumpurple"), shape=18)+
  theme(legend.position = "none")
p2

p2.tx=ggplot(accuracy.df.som, aes(x=length.train))+
  geom_point(aes(y=acc.tradit.som2.tx, color="hotpink4"), shape=15)+
  geom_point(aes(y=acc.pscore.som2.tx, color="mediumpurple"), shape=18)+
  theme(legend.position = "none")
p2.tx

p3=ggplot(accuracy.df.som, aes(x=length.train))+
  geom_point(aes(y=acc.tradit.som3, color="firebrick"), shape=15)+
  geom_point(aes(y=acc.pscore.som3.tx, color="mediumpurple"), shape=18)+
  theme(legend.position = "none")
p3

p3.tx=ggplot(accuracy.df.som, aes(x=length.train))+
  geom_point(aes(y=acc.tradit.som3.tx, color="hotpink4"), shape=15)+
  geom_point(aes(y=acc.pscore.som3.tx, color="mediumpurple"), shape=18)+
  theme(legend.position = "none")
p3.tx



p=ggplot(accuracy.df.som, aes(x=length.train))+
  geom_point(aes(y=acc.tradit.som1,    color="firebrick"), shape=15)+
  geom_point(aes(y=acc.pscore.som1), shape=18)+
  geom_point(aes(y=acc.tradit.som2,     color="hotpink4"), shape=15)+
  geom_point(aes(y=acc.pscore.som2,     color="hotpink4"), shape=18)+
  geom_point(aes(y=acc.tradit.som3,         color="blue"), shape=15)+
  geom_point(aes(y=acc.pscore.som3,         color="blue"), shape=18)+
  geom_point(aes(y=acc.tradit.som1.tx, color="mediumpurple"), shape=15)+
  geom_point(aes(y=acc.pscore.som1.tx, color="mediumpurple"), shape=18)+
  geom_point(aes(y=acc.tradit.som2.tx,      color="orchid2"), shape=15)+
  geom_point(aes(y=acc.pscore.som2.tx,      color="orchid2"), shape=18)+
  geom_point(aes(y=acc.tradit.som3.tx, color="deepskyblue4"), shape=15)+
  geom_point(aes(y=acc.pscore.som3.tx, color="deepskyblue4"), shape=18)+
  theme(legend.position = "none")
p



