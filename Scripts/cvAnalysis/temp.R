df2=df
str(df2)
df2$ID=as.factor(df2$ID)
colnames(df2)=c("TrainLength", "Accuracy", "Model")

zeros=rep(0, times=sample.length)
ones=rep(1, times=sample.length)
ID=c(zeros, ones)
len.train=c(length.train.boot.weights,length.train.boot.weights)
accuracy=c(accuracy.Sup3,accuracy.trad)
df2=data.frame(len.train, as.factor(ID), accuracy)

lmod=lm(accuracy~len.train*ID, data=df2)
(lmods=summary(lmod))

p=ggplot(df, aes(x=length.train.boot.weights))+
  geom_point(aes(y=accuracy.Sup3))+
  geom_point(aes(y=accuracy.trad))+
  geom_abline(intercept = coef(lmod)[1], slope = coef(lmod)[2])+
  geom_abline(intercept = coef(lmod)[1]+coef(lmod)[3], slope = coef(lmod)[2]+coef(lmod)[4])
p

