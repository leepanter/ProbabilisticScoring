dat.temp = phq9 %>% group_by(as.factor(qTot)) %>% tally()
colnames(dat.temp)=c("SumofAnswers", "NumberofObservations")

nobs.permuted=c(0,0,0,
                0,42,63,
                42,42,42,
                21,21,21,
                0,0,0,
                0,0,0,
                0,0,0,
                0,0,0,
                0,0,0,
                0)

dat.temp$nobs.permuted=nobs.permuted

p=ggplot(dat.temp, aes(x=SumofAnswers))+
  geom_point(aes(y=NumberofObservations))+
  geom_point(aes(y=nobs.permuted, color="blue"))+
  geom_vline(xintercept = 8)+
  geom_vline(xintercept = 11)+
  geom_vline(xintercept = 1)+
  geom_hline(yintercept = 0)+
  theme(legend.position = "none")

p

number.samples=100
sample.length=number.samples+2
df.set.info=df.train.set.info
colnames(df.set.info)=c("df.k.sets", "N.obs.train.set")
sample.vec.k.sets=df.set.info$df.k.sets
sample.vec.k.sets=sample.vec.k.sets[-c(1,1245)]
N.set.arg=sort(sample(sample.vec.k.sets, number.samples, replace = FALSE))
N.set.arg=sort(c(N.set.arg,1247, 1248))

index.N.set.arg.df.set.info=c()
df.k.sets.index.N.set.arg=c()
N.obs.train.set.index.N.set.arg=c()
N.obs.test.set.index.N.set.arg=c()
for(i in 1:length(N.set.arg)){
  index.N.set.arg.df.set.info[i]=which(df.set.info$df.k.sets==N.set.arg[i])
  df.k.sets.index.N.set.arg[i]=df.set.info$df.k.sets[index.N.set.arg.df.set.info[i]]
  N.obs.train.set.index.N.set.arg[i]=df.set.info$N.obs.train.set[index.N.set.arg.df.set.info[i]]
  N.obs.test.set.index.N.set.arg[i]=N.obs.train.set.index.N.set.arg[i]/(df.k.sets.index.N.set.arg[i]-1)
}

df.out=data.frame(df.k.sets.index.N.set.arg,
                  N.obs.train.set.index.N.set.arg,
                  N.obs.test.set.index.N.set.arg)
colnames(df.out)=c("K", "TrainObs", "TestObs")

p=ggplot(df.out, aes(x=K))+
  geom_point(aes(y=TrainObs, color="blue"))+
  geom_point(aes(y=TestObs, color="red"))
p




dat.new=read.csv(file = "/Users/lee/Desktop/Sup2/df_acc_100_sup2.csv")
dat.orig=read.csv(file = "/Users/lee/Desktop/Sup1/accuracyDFlmFEre.csv")
dat.orig=subset(dat.orig, ID==0)
datsavemyass=data.frame(dat.new$length.boot.weights, dat.new$accuracy.Sup3, dat.orig$value)
colnames(datsavemyass)=c("TrainLen", "PscoreSimAccuracy", "Traditaccuracy")

library(ggplot2)
p=ggplot(datsavemyass, aes(x=TrainLen))+
  geom_point(aes(y=PscoreSimAccuracy))

df.in1=read.csv(file = "/Users/lee/Desktop/Sup3/accuracyDFUnSup100.csv")
df.in1=df.in1[,2:6]
colnames(df.in1)=c("TrainLen", "PScoreKmeans", "TraditKmeans",
                   "PScoreHclust", "TraditHclust")
str(df.in1)


df.in=read.csv(file = "/Users/lee/Desktop/Sup3/accuracyDFUnSup1002.csv")
df.in$ID=as.factor(df.in$ID)
colnames(df.in)=c("TrainLen", "ID", "Kmeans", "Hclust")

lmodKmeans=lm(Kmeans~TrainLen*ID, data=df.in)
(lmodKmeanss=summary(lmodKmeans))

pKmeans=ggplot(df.in1, aes(x=TrainLen))+
  geom_point(aes(y=PScoreKmeans, color="blue"))+
  geom_point(aes(y=TraditKmeans, color="red"))+
  geom_abline(intercept = coef(lmodKmeans)[1], slope = coef(lmodKmeans)[2])+
  geom_abline(intercept = coef(lmodKmeans)[1]+coef(lmodKmeans)[3],
              slope = coef(lmodKmeans)[2]+coef(lmodKmeans)[4])

pKmeans


lmodHclust=lm(Hclust~TrainLen*ID, data=df.in)
(lmodHclusts=summary(lmodHclust))

pHclust=ggplot(df.in1, aes(x=TrainLen))+
  geom_point(aes(y=PScoreHclust, color="blue"))+
  geom_point(aes(y=TraditHclust, color="red"))+
  geom_abline(intercept = coef(lmodHclust)[1], slope = coef(lmodHclust)[2])+
  geom_abline(intercept = coef(lmodHclust)[1]+coef(lmodHclust)[3],
              slope = coef(lmodHclust)[2]+coef(lmodHclust)[4])

pHclust

df.temp=data.frame(k, len.train, len.test)
twos=which(df.temp$len.test==2)
y.min.two=df.temp$len.train[min(twos)]
x.min.two=df.temp$k[min(twos)]
y.max.two=df.temp$len.train[max(twos)]
x.max.two=df.temp$k[max(twos)]
