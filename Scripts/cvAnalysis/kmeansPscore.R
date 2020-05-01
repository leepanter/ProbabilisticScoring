# kmeansPscore.R
#-------------------------------------------------------------------------#
####	Description:	 ####
# This script will generate a clustering of the PHQ9 data using a K-means approach.  It will produce three clusters that will then be ordered according to the depression classification ranking based on empirical properties of the clusters.


####	Script Dependencies	 ####

# Package Dependencies:
library(factoextra)
library(tidyverse)
library(cluster)
library(dendextend)
library(ggplot2)
packageurl <- "https://cran.r-project.org/src/contrib/Archive/kohonen/kohonen_2.0.19.tar.gz"
install.packages(packageurl, repos = NULL, type = "source")
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

# K Means  ----------------------------------------------------------------

k.means.out1=kmeans(dat.kmeans, centers=3, iter.max = 50, nstart = 100)
k.means.out=kmeans(dat.kmeans, centers=3, iter.max = 1000, nstart = 2000)

dat$kmeans=k.means.out$cluster
kmeans.factor=c()
for(i in 1:2495){
  if(dat$kmeans[i]==1){
    kmeans.factor[i]="C2"
  } else if(dat$kmeans[i]==2){
    kmeans.factor[i]="C1"
  } else{kmeans.factor[i]="C3"}
}

dat$kmeans.factor=kmeans.factor

accuracy.traditional.comp.kmeans=length(which(dat$kmeans.factor==dat$sumClassString))/2495

phq9$kmeans=dat$kmeans
phq9$kmeans.factor=dat$kmeans.factor

# fviz_cluster(k.means.out, data = dat.kmeans)
#
# pQ1=ggplot(dat, aes(x=Q1, y=qTot, color=kmeans.factor))+
#   geom_point()
# pQ1
# pQ2=ggplot(dat, aes(x=Q2, y=qTot, color=kmeans.factor))+
#   geom_point()
# pQ2
# pQ3=ggplot(dat, aes(x=Q3, y=qTot, color=kmeans.factor))+
#   geom_point()
# pQ3
# pQ4=ggplot(dat, aes(x=Q4, y=qTot, color=kmeans.factor))+
#   geom_point()
# pQ4
# pQ5=ggplot(dat, aes(x=Q5, y=qTot, color=kmeans.factor))+
#   geom_point()
# pQ5
# pQ6=ggplot(dat, aes(x=Q6, y=qTot, color=kmeans.factor))+
#   geom_point()
# pQ6
# pQ7=ggplot(dat, aes(x=Q7, y=qTot, color=kmeans.factor))+
#   geom_point()
# pQ7
# pQ8=ggplot(dat, aes(x=Q8, y=qTot, color=kmeans.factor))+
#   geom_point()
# pQ8
# pQ9=ggplot(dat, aes(x=Q9, y=qTot, color=kmeans.factor))+
#   geom_point()
# pQ9
#
# gridExtra::grid.arrange(pQ1, pQ2, pQ3,
#                         pQ4, pQ5, pQ6,
#                         pQ7, pQ8, pQ9, nrow=3)



#-------------------------------------------------------------------------#
# Kmeans CVk analysis -----------------------------------------------------
#-------------------------------------------------------------------------#

# number.samples=100
# sample.length=number.samples+2
# df.set.info=df.train.set.info
# colnames(df.set.info)=c("df.k.sets", "N.obs.train.set")
# sample.vec.k.sets=df.set.info$df.k.sets
# sample.vec.k.sets=sample.vec.k.sets[-c(1,1245)]
# N.set.arg=sort(sample(sample.vec.k.sets, sample.length, replace = FALSE))
# N.set.arg=sort(c(N.set.arg,1247, 1248))

df.set.info=df.k.final
colnames(df.set.info)=c("df.k.sets", "N.obs.train", "N.obs.test" )

sample.length=100
N.set.arg=df.set.info$df.k.sets


accuracy.ksets=c()
traditional.accuracy.ksets=c()
N.obs.k=c()
boot.sample.i=list()

for(i in 1:sample.length){
  boot.sample.i[[i]]=CVsplit(phq9, N.set.arg[i])
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
    P.score.probClass.i.j
    for(k in 1:boot.sample.i[[i]][[3]]){
      P.score.probClass.i.j[[k]]=convg(P.score.sequences[[i]][[j]][[k]], 0.75)
    }
    P.score.probClass.i[[j]]=P.score.probClass.i.j
  }
  P.score.probClass[[i]]=P.score.probClass.i
}

outcome.Pscore=list()
outcome.kmeans=list()
for(i in 1:sample.length){
  outcome.Pscore.i=list()
  outcome.kmeans.i=list()
  for(j in 1:N.j[i]){
    outcome.Pscore.i.j=c()
    outcome.kmeans.i.j=c()
    for(k in 1:boot.sample.i[[i]][[3]]){
      outcome.Pscore.i.j[k]=P.score.probClass[[i]][[j]][[k]][[3]]
      outcome.kmeans.i.j[k]=boot.sample.i[[i]][[2]][[j]][k,17]
    }
    outcome.Pscore.i[[j]]=outcome.Pscore.i.j
    outcome.kmeans.i[[j]]=outcome.kmeans.i.j
  }
  outcome.Pscore[[i]]=outcome.Pscore.i
  outcome.kmeans[[i]]=outcome.kmeans.i
}

unlist.outcome.Pscore=list()
unlist.outcome.kmeans=list()
average.class.denom=c()
for(i in 1:sample.length){
  unlist.outcome.Pscore[[i]]=unlist(outcome.Pscore[[i]])
  unlist.outcome.kmeans[[i]]=unlist(outcome.kmeans[[i]])
  average.class.denom[i]=length(unlist.outcome.Pscore[[i]])
}


accuracy.outcome.kmeans=c()
for(i in 1:sample.length){
  accuracy.outcome.kmeans[i]=length(which(unlist.outcome.kmeans[[i]]==unlist.outcome.Pscore[[i]]))/average.class.denom[i]
}

training.set.length=c()
for(i in 1:sample.length){
  training.set.length[i]=dim(boot.sample.i[[i]][[1]][[1]])
}

plot(accuracy.outcome.kmeans~training.set.length)




N.obs.k=c()
for(k in 1:n.minus.one+1){
  N.obs.k[k]=length(boot.sample.i[[k]][[1]])
}

accuracy.df=data.frame(N.obs.k, accuracy.ksets)


accuracy.lm=lm(accuracy.ksets~N.obs.k, data = accuracy.df)
(accuracy.lms=summary(accuracy.lm))

accuracy.plot=ggplot(accuracy.df, aes(x=N.obs.k, y=accuracy.ksets))+
  xlab("Observations in Traing Set")+
  ylab("Accuracy of Prob.Scoring")+
  geom_abline(intercept = as.numeric(coef(accuracy.lm))[[1]],
              slope=as.numeric(coef(accuracy.lm))[[2]])+
  geom_point()

accuracy.plot

zeros=rep(0, times=n.minus.one+1)
ones=rep(1, times=n.minus.one+1)
ID=as.factor(c(ones, zeros))
N.obs.train.k.lmFE=rep(N.obs.k, times=2)
accuracy.out.lmFE=c(accuracy.ksets,traditional.accuracy.ksets)
accuracy.df.lmFE=data.frame(N.obs.train.k.lmFE, ID, accuracy.out.lmFE)

accuracy.lmFE=lm(accuracy.out.lmFE~ID+N.obs.train.k.lmFE+ID:N.obs.train.k.lmFE,
                 data = accuracy.df.lmFE)
(accuracy.lmFEs=summary(accuracy.lmFE))

accuracy.df.lmFE.re=reshape::melt(accuracy.df.lmFE, id.vars=c("N.obs.train.k.lmFE","ID"))

trad.intercept=as.numeric(coef(accuracy.lmFE))[1]+as.numeric(coef(accuracy.lmFE))[2]
trad.slope=as.numeric(coef(accuracy.lmFE))[3]+as.numeric(coef(accuracy.lmFE))[4]

prob.intercept=as.numeric(coef(accuracy.lmFE))[1]
prob.slope=as.numeric(coef(accuracy.lmFE))[3]


p=ggplot2::ggplot(accuracy.df.lmFE.re, aes(x = N.obs.train.k.lmFE, y = accuracy.out.lmFE,
                                           group = ID))+
  geom_point()+
  geom_abline(intercept = trad.intercept, slope = trad.slope)+
  geom_abline(intercept = prob.intercept, slope = prob.slope)


p



# Heirarchical Clustering -------------------------------------------------

d=dist(dat.kmeans)
hc1=hclust(d, method = "complete")
plot(hc1)
sub_grp=cutree(hc1, k=3)
table(sub_grp)

dat=dat %>%
    mutate(hcluster = sub_grp)

hcluster.factor=c()
for(i in 1:2495){
  if(dat$hcluster[i]==1){
    hcluster.factor[i]="C3"
  } else if(dat$hcluster[i]==2){
    hcluster.factor[i]="C1"
  } else{hcluster.factor[i]="C2"}
}

dat$hcluster.factor=hcluster.factor

length(which(dat$hcluster.factor==dat$SupOutString))/2495





#-------------------------------------------------------------------------#
# SOM Clustering ----------------------------------------------------------
#-------------------------------------------------------------------------#

dat.som=scale(dat.kmeans)

out.grid=somgrid(xdim = 12, ydim=9, topo = "rectangular")

som.out=som(dat.som, grid = out.grid)

plot(som.out, type = 'changes')

plot(som.out)
plot(som.out, type = 'mapping')
plot(som.out, type = 'count')
plot(som.out, type = 'dist.neighbours')
plot(som.out, type = 'codes')
plot(som.out, type = 'quality')

dat$somOut.108=som.out$unit.classif
somOut.108=som.out$unit.classif


plot(hclust(dist(som.out$codes)))
class.cuts=cutree(hclust(dist(som.out$codes)),3)
plot(som.out, type = 'codes', bgcol = rainbow(3)[class.cuts])
add.cluster.boundaries(som.out, class.cuts)

#-------------------------------------------------------------------------#
#good seeds: 3
dat.som=scale(dat.kmeans)
set.seed(3)
out.grid=somgrid(xdim = 10, ydim= 10, topo = "rectangular")

som.out=som(dat.som, grid = out.grid,
            rlen=1000)

plot(som.out, type = 'changes')

plot(som.out)
plot(som.out, type = 'mapping')
plot(som.out, type = 'count')
plot(som.out, type = 'dist.neighbours')
plot(som.out, type = 'codes')
plot(som.out, type = 'quality')


dat$somOut.108=som.out$unit.classif
somOut.108=som.out$unit.classif


plot(hclust(dist(som.out$codes)))
class.cuts=cutree(hclust(dist(som.out$codes)),3)
plot(som.out, type = 'codes', bgcol = rainbow(3)[class.cuts])
add.cluster.boundaries(som.out, class.cuts)

mydata <- som.out$codes
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}
plot(wss)


