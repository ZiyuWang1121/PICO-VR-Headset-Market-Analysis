
setwd("~/Documents/STAT3613")

vr=read.csv("vr.csv")

library(psych)
library(GPArotation)
library(reshape)
library(ggplot2)


vr[,4:17]


#cor() correlation matrix
cor(x=vr[,4:17])

#principal() principal component factor analysis
#r= correlation matrix or raw data
#nfactors= number of factors
#rotate="none" no rotation
#covar=T to use covariance matrix
fit<-principal(r=vr[,4:17], nfactors=14,rotate="none")#,cov=T)
fit
#print(fit, digits=3, cutoff=.3, sort=TRUE)

#scree() scree plot
#rx= correlation matrix or raw data
#factor=FALSE do not draw scree for factors
scree(rx=vr[,4:17],factor=FALSE)

# b

#scree plot

#fa() factor analysis
#r= data set
#nfactors= number of factors
#rotate="none" no rotation
#fm="ml" maximum likelihood estimation

fit1<-fa(r=vr[,4:17],nfactors=2,rotate="none",fm="ml")
print(fit1,digits=3)
fit2<-fa(r=vr[,4:17],nfactors=3,rotate="none",fm="ml")
print(fit2,digits=3)
fit3<-fa(r=vr[,4:17],nfactors=6,rotate="none",fm="ml")
print(fit3,digits=3)

fit$uniquenesses
fit$loadings
fit$communalities

fit$STATISTIC
fit$PVAL

# 2 factor FA
fitr<-fa(r=vr[,4:17],nfactors=2,rotate="varimax",fm="ml",scores="regression")
fitr
print(fitr,digits=3)
fitr$loadings
fitr$communalities

ld<-data.frame(fitr$loadings[,1:fitr$factors])
ggplot(data=ld,aes(x=ML2,y=ML1))+
  geom_point()+
  geom_text(aes(label=rownames(ld),vjust=1))+
  geom_vline(aes(xintercept=0))+
  geom_hline(aes(yintercept=0))+
  coord_cartesian(xlim = c(-1,1),ylim=c(-1,1)) 

#-----------------------cluster
#rerun K-means with seeds from the Ward's method
dist<-dist(vr[,4:17],method="euclidean")^2
fit2 <- hclust(dist, method="ward.D")

#observation
ggplot(mapping=aes(x=1:length(fit2$height),y=fit2$height))+
  geom_line()+
  geom_point()+
  labs(x="stage",y="height")

#Dendrogram
par(mar=c(4,4,1,1))
plot(fit2,hang=-1,main="",axes=FALSE)
axis(side = 2, at = seq(0, 500, 20))


ward.sol<-cutree(fit2,k=4)
tb<-aggregate(vr[,4:17],by=list(ward=ward.sol),FUN=mean)
#use the centers as seeds
# 4 cluster, so tb must be table with 4 rows
fit1<-kmeans(x=vr[,4:17],centers=tb[,2:15],algorithm="Hartigan-Wong")
fit1

#cluster centers
# extract the means for you
tb<-fit1$centers

#tb: cluster means
# combine tb and cluster column with value from 1 to 4

tb<-data.frame(cbind(tb,cluster=1:4))
tbm<-melt(tb,id.vars='cluster')
tbm$cluster<-factor(tbm$cluster)

ggplot(tbm, 
       aes(x = variable, y = value, group = cluster, colour = cluster)) + 
  geom_line(aes(linetype=cluster))+
  geom_point(aes(shape=cluster)) +
  geom_hline(yintercept=3) +
  labs(x=NULL,y="mean")

# ward's method
#vr$gender, vr$x1, both work
tbm<-data.frame(table(vr$x1,ward.sol))
tbm$cluster<-factor(tbm$ward.sol)
ggplot(tbm, aes(fill=Var1, y=Freq, x=ward.sol)) +
  geom_bar(position="fill", stat="identity")

pt<-table(product$age,product$product)

# e
#factor scores
sc<-data.frame(fitr$scores)
sc=data.frame(cbind(fitr$scores,fit1$cluster))
colnames(sc)=c("ML2","ML1","clusters")

ggplot(data=sc,aes(x=ML2,y=ML1, color=as.factor(clusters)))+
  geom_point()+
  geom_vline(aes(xintercept=0))+
  geom_hline(aes(yintercept=0))+
  geom_text(aes(label=1:60,vjust=2))+
  coord_cartesian(xlim=c(-3,3),ylim=c(-3,3)) +
  scale_color_manual(values=c("Red","green","blue","purple"))


cluster=fit1$cluster
group=cbind(vr,cluster)

#2-way frequency table
pt<-table(group$glass,group$x14)
pt





######## LOGISTIC ########################################

lrvr<-vr

for (i in c(19,20,22:29)) {
  lrvr[,i]<-as.factor(vr[,i])
}
#lrvr$Q4<-scale(lrvr$Q4)
#lrvr[,4:18]<-scale(lrvr[,4:18])
#lrvr$age<-scale(lrvr$age)

lrdata<-lrvr[,4:17]
lrdata<-lrdata

lrdata$a1<-lrvr[,22]
lrall<-glm(formula="a1 ~ .", data=lrdata_all, family=binomial(logit))
summary(lrall)
# x1 significant

lrdata$a1<-lrvr[,22]
logreg1<-glm(formula="a1 ~ .", data=lrdata, family=binomial(logit))
summary(logreg1)
# x1 significant

logstep1<-step(logreg1)
summary(logstep1)
#1,3,5,6,11 included, 3,11 significant

lrdata$a1<-lrvr[,23]
logreg2<-glm(formula="a1 ~ .", data=lrdata, family=binomial(logit))
summary(logreg2)
#no vars significant

logstep2<-step(logreg2)
summary(logstep2)
#x1,5,7,10 picked, x5 significant

lrdata$a1<-lrvr[,24]
logreg3<-glm(formula="a1 ~ .", data=lrdata, family=binomial(logit))
summary(logreg3)
#no vars significant

logstep3<-step(logreg3)
summary(logstep3)
#3,5,9,11,14 left, 11 significant

lrdata$a1<-lrvr[,25]
logreg4<-glm(formula="a1 ~ .", data=lrdata, family=binomial(logit))
summary(logreg4)
#1 significant

logstep4<-step(logreg4)
summary(logstep4)
#1,3,4,6,8 left, 1,3,8 significant

lrdata$a1<-lrvr[,26]
logreg5<-glm(formula="a1 ~ .", data=lrdata, family=binomial(logit))
summary(logreg5)
# no vars signf.

logstep5<-step(logreg5)
summary(logstep5)
# x2 significant

lrdata$a1<-lrvr[,27]
logreg6<-glm(formula="a1 ~ .", data=lrdata, family=binomial(logit))
summary(logreg6)
#no vars signf

logstep6<-step(logreg6)
summary(logstep6)
#x3,4,12 left, 3,4 sign

lrdata$a1<-lrvr[,28]
logreg7<-glm(formula="a1 ~ .", data=lrdata, family=binomial(logit))
summary(logreg7)
# no vars significant
logstep7<-step(logreg7)
summary(logstep7)
#4,11 left, 4 significant

lrdata$a1<-lrvr[,29]
logreg8<-glm(formula="a1 ~ .", data=lrdata, family=binomial(logit))
summary(logreg8)
# 8 significant

logstep8<-step(logreg8)
summary(logstep8)
#1,6,7,8,13 left, 7 and 8 significant

summary(logreg1) #1
summary(logreg2)
summary(logreg3)
summary(logreg4) #1
summary(logreg5)
summary(logreg6)
summary(logreg7)
summary(logreg8) #8

summary(logstep8) #1,6,7,8,13,S:7,8
summary(logstep7) #4,11, S: 4
summary(logstep6) #3,4,12, S:3, 4
summary(logstep5) #2,8,11, S: 2
summary(logstep4) #1,3,4,6,8, S: 1,3,8
summary(logstep3) #3,5,9,11,14, S: 11
summary(logstep2) #1,5,7,10, S: 5
summary(logstep1) #1,3,5,6,11, S:3,11

dataincome<-data.frame(income=vr$income,
                       a1=as.factor(vr$X1599_8_128G_5),
                       a2=as.factor(vr$X1599_8_128G_10),
                       a3=as.factor(vr$X1599_8_256G_5),
                       a4=as.factor(vr$X1599_8_256G_10),
                       a5=as.factor(vr$X1999_8_128G_5),
                       a6=as.factor(vr$X1999_8_256G_5),
                       a7=as.factor(vr$X1999_8_128G_10),
                       a8=as.factor(vr$X1999_8_256G_10))

logincome1<-glm("a1~income",data=dataincome,family=binomial(logit))
summary(logincome1)
logincome2<-glm("a2~income",data=dataincome,family=binomial(logit))
summary(logincome2)
logincome3<-glm("a3~income",data=dataincome,family=binomial(logit))
summary(logincome3)
logincome4<-glm("a4~income",data=dataincome,family=binomial(logit))
summary(logincome4)
logincome5<-glm("a5~income",data=dataincome,family=binomial(logit))
summary(logincome5)
logincome6<-glm("a6~income",data=dataincome,family=binomial(logit))
summary(logincome6)
logincome7<-glm("a7~income",data=dataincome,family=binomial(logit))
summary(logincome7)
logincome8<-glm("a8~income",data=dataincome,family=binomial(logit))
summary(logincome8)

# => Income is not significant in predicting willingess to buy any of the products



################ LOGISTIC 2 #######################


lrdata<-vr[,c(3,18,19,20,21,30,31,32,33,34,35,36)]

lrdata$interest<-as.factor(lrdata$interest)
lrdata$gender<-as.factor(lrdata$gender)
lrdata$glass<-as.factor(lrdata$glass)
lrdata$PC<-as.factor(lrdata$PC)
lrdata$Cellphone<-as.factor(lrdata$Cellphone)
lrdata$VR.equip<-as.factor(lrdata$VR.equip)
lrdata$PS<-as.factor(lrdata$PS)
lrdata$Xbox<-as.factor(lrdata$Xbox)
lrdata$Switch<-as.factor(lrdata$Switch)
lrdata$NGD<-as.factor(lrdata$NGD)

lrdata$a1<-as.factor(lrvr[,22])
logreg1<-glm(formula="a1 ~ .", data=lrdata, family=binomial(logit))
summary(logreg1)
#Cellphone1

logstep1<-step(logreg1)
summary(logstep1)
#Cellphone1

lrdata$a1<-lrvr[,23]
logreg2<-glm(formula="a1 ~ .", data=lrdata, family=binomial(logit))
summary(logreg2)
#no vars significant

logstep2<-step(logreg2)
summary(logstep2)
#no vars significant

lrdata$a1<-lrvr[,24]
logreg3<-glm(formula="a1 ~ .", data=lrdata, family=binomial(logit))
summary(logreg3)
#no vars significant

logstep3<-step(logreg3)
summary(logstep3)
#no vars significants

lrdata$a1<-lrvr[,25]
logreg4<-glm(formula="a1 ~ .", data=lrdata, family=binomial(logit))
summary(logreg4)
#no vars significant

logstep4<-step(logreg4)
summary(logstep4)
#no vars significant

lrdata$a1<-lrvr[,26]
logreg5<-glm(formula="a1 ~ .", data=lrdata, family=binomial(logit))
summary(logreg5)
# no vars signf.

logstep5<-step(logreg5)
summary(logstep5)
#Cdllphone

lrdata$a1<-lrvr[,27]
logreg6<-glm(formula="a1 ~ .", data=lrdata, family=binomial(logit))
summary(logreg6)
#no vars signf

logstep6<-step(logreg6)
summary(logstep6)
#no vars signf

lrdata$a1<-lrvr[,28]
logreg7<-glm(formula="a1 ~ .", data=lrdata, family=binomial(logit))
summary(logreg7)
#Age significant??

logstep7<-step(logreg7)
summary(logstep7)
#age, cellphone

lrdata$a1<-lrvr[,29]
logreg8<-glm(formula="a1 ~ .", data=lrdata, family=binomial(logit))
summary(logreg8)
# no vars significant

logstep8<-step(logreg8)
summary(logstep8)
# no vars significant

summary(logreg1) #1
summary(logreg2)
summary(logreg3)
summary(logreg4) #1
summary(logreg5)
summary(logreg6)
summary(logreg7)
summary(logreg8) #8

summary(logstep8) 
summary(logstep7) #age, cellphone
summary(logstep6) 
summary(logstep5) 
summary(logstep4) #cellphone
summary(logstep3) 
summary(logstep2) 
summary(logstep1) 

