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
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/kohonen/kohonen_2.0.19.tar.gz"
# install.packages(packageurl, repos = NULL, type = "source")
# library(kohonen)


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


# Eda for Class ids
table.sum.class = dat %>% group_by(sumClassString) %>% tally()
table.sum.class

# KMEANS Int -> factor function
kmeans.convert=function(in.arg){
  if(in.arg==1){
    out.arg="C2"
  } else if(in.arg==2){
    out.arg="C1"
  } else{out.arg="C3"}
  return(out.arg)
}

pscore.convert=function(in.arg){
  out.arg=NA
  if(is.na(in.arg)){
    out.arg="NA"
  } else {
      if(in.arg==1){
        out.arg="C2"
      } else if(in.arg==2){
        out.arg="C1"
      } else {
        out.arg="C3"
      }
    }
  return(out.arg)
}

#-------------------------------------------------------------------------#
# K Means  ----------------------------------------------------------------
#-------------------------------------------------------------------------#

# function to compute total within-cluster sum of square
wss <- function(k) {
  kmeans(dat.kmeans, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

# plot(k.values, wss_values,
#      type="b", pch = 19, frame = FALSE,
#      xlab="Number of clusters K",
#      ylab="Total within-clusters sum of squares")


k.means.out1=kmeans(dat.kmeans, centers=3, iter.max = 50, nstart = 100)
k.means.out=kmeans(dat.kmeans, centers=3, iter.max = 1000, nstart = 2000)

dat$kmeans=k.means.out$cluster

table.kmeans = dat %>% group_by(kmeans) %>% tally()
table.kmeans

kmeans.factor=c()
for(i in 1:2495){
  if(dat$kmeans[i]==1){
    kmeans.factor[i]="C3"
  } else if(dat$kmeans[i]==2){
    kmeans.factor[i]="C1"
  } else{kmeans.factor[i]="C2"}
}
dat$kmeans.factor=kmeans.factor

table.kmeans.factor = dat %>% group_by(kmeans.factor) %>% tally()
table.kmeans.factor


# phq9$kmeans=dat$kmeans
# phq9$kmeans.factor=dat$kmeans.factor

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
# Heirarchical Clustering -------------------------------------------------
#-------------------------------------------------------------------------#

d=dist(dat.kmeans)
hc1=hclust(d, method = "complete")
# plot(hc1)
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


table.hclust.factor = dat %>% group_by(hcluster.factor) %>% tally()
table.hclust.factor

# Hcluster #2 ------------------------------------------------------------#
d2=dist(dat.kmeans, method="manhattan")
hc2=hclust(d2, method = "complete")
sub_grp2=cutree(hc2, k=3)
table(sub_grp2)

hcluster.factor2=c()
for(i in 1:2495){
  if(sub_grp2[i]==1){
    hcluster.factor2[i]="C3"
  } else if(sub_grp2[i]==2){
    hcluster.factor2[i]="C1"
  } else{hcluster.factor2[i]="C2"}
}
dat$hcluster.factor2=hcluster.factor2


table.hclust.factor2 = dat %>% group_by(hcluster.factor2) %>% tally()
table.hclust.factor2





#-------------------------------------------------------------------------#
# Kmeans CVk analysis -----------------------------------------------------
#-------------------------------------------------------------------------#

number.samples=100
sample.length=number.samples+2
df.set.info=df.train.set.info
colnames(df.set.info)=c("df.k.sets", "N.obs.train.set")
sample.vec.k.sets=df.set.info$df.k.sets
sample.vec.k.sets=sample.vec.k.sets[-c(1,1245)]
N.set.arg=sort(sample(sample.vec.k.sets, number.samples, replace = FALSE))
N.set.arg=sort(c(N.set.arg,1247, 1248))

# df.set.info=df.k.final
# colnames(df.set.info)=c("df.k.sets", "N.obs.train", "N.obs.test" )
# sample.length=100
# N.set.arg=df.set.info$df.k.sets

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
  len.i[i]=length(boot.sample.i[[i]][[2]])
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
kmeans.i=list()
hclust.i=list()
#som108.i=list()
#sommap.i=list()
for(i in 1:sample.length){
  pscore.i.j=c()
  tradit.i.j=c()
  kmeans.i.j=c()
  hclust.i.j=c()
  #som108.i.j=c()
  #sommap.i.j=c()
  for(j in 1:N.j[i]){
    pscore.i.j=append(pscore.i.j,
                      boot.sample.i[[i]][[2]][[j]][["pscore.factor"]])
    tradit.i.j=append(tradit.i.j,
                      boot.sample.i[[i]][[2]][[j]][["SupOutString"]])
    kmeans.i.j=append(kmeans.i.j,
                      boot.sample.i[[i]][[2]][[j]][["kmeans.factor"]])
    hclust.i.j=append(hclust.i.j,boot.sample.i[[i]][[2]][[j]][["hcluster.factor"]])
    #som108.i.j=append(som108.i.j,as.character(boot.sample.i[[i]][[2]][[j]][["class.som108"]]))
    #sommap.i.j=append(sommap.i.j,
                      #as.character(boot.sample.i[[i]][[2]][[j]][["class.som"]]))
  }
  pscore.i[[i]]=pscore.i.j
  tradit.i[[i]]=tradit.i.j
  kmeans.i[[i]]=kmeans.i.j
  hclust.i[[i]]=hclust.i.j
  #som108.i[[i]]=som108.i.j
  #sommap.i[[i]]=sommap.i.j
}

acc.pscore.kmeans=c()
acc.tradit.kmeans=c()
acc.pscore.hclust=c()
acc.tradit.hclust=c()
#acc.pscore.som108=c()
#acc.tradit.som108=c()
#acc.pscore.sommap=c()
#acc.tradit.sommap=c()
length.train=c()

for(i in 1:sample.length){
  acc.pscore.kmeans[i]=length(which(kmeans.i[[i]]==pscore.i[[i]]))/length(pscore.i[[i]])
  acc.tradit.kmeans[i]=length(which(kmeans.i[[i]]==tradit.i[[i]]))/length(tradit.i[[i]])

  acc.pscore.hclust[i]=length(which(hclust.i[[i]]==pscore.i[[i]]))/length(pscore.i[[i]])
  acc.tradit.hclust[i]=length(which(hclust.i[[i]]==tradit.i[[i]]))/length(tradit.i[[i]])


  # acc.pscore.som108[i]=length(which(som108.i[[i]]==pscore.i[[i]]))/length(pscore.i[[i]])
  # acc.tradit.som108[i]=length(which(som108.i[[i]]==tradit.i[[i]]))/length(tradit.i[[i]])
  #
  # acc.pscore.sommap[i]=length(which(sommap.i[[i]]==pscore.i[[i]]))/length(pscore.i[[i]])
  # acc.tradit.sommap[i]=length(which(sommap.i[[i]]==tradit.i[[i]]))/length(tradit.i[[i]])

  length.train[i]=dim(boot.sample.i[[i]][[1]][[1]])[1]
}

accuracy.df=data.frame(length.train,
                       acc.pscore.kmeans,
                       acc.tradit.kmeans,
                       acc.pscore.hclust,
                       acc.tradit.hclust,
                       # acc.pscore.som108,
                       # acc.tradit.som108,
                       # acc.pscore.sommap,
                       # acc.tradit.sommap
                       )

p=ggplot(accuracy.df, aes(x=length.train))+
  geom_point(aes(y=acc.pscore.kmeans,    color="firebrick"), shape=18)+
  geom_point(aes(y=acc.tradit.kmeans,     color="hotpink4"), shape=15)+
  geom_point(aes(y=acc.pscore.hclust,         color="blue"), shape=18)+
  geom_point(aes(y=acc.tradit.hclust, color="mediumpurple"), shape=15)+
  #geom_point(aes(y=acc.pscore.som108,      color="orchid2"), shape=18)+
  #geom_point(aes(y=acc.tradit.som108, color="deepskyblue4"), shape=15)+
  #geom_point(aes(y=acc.pscore.sommap,     color="seagreen"), shape=18)+
  #geom_point(aes(y=acc.tradit.sommap,  color="saddlebrown"), shape=15)+
  theme(legend.position = "none")
p




