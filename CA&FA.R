
setwd("C:/Users/HP/Desktop/ио©н/STAT3613/GP")


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


ward.sol<-cutree(fit2,k=3)
tb<-aggregate(vr[,4:17],by=list(ward=ward.sol),FUN=mean)
#use the centers as seeds
# 3 cluster, so tb must be table with 3 rows
fit1<-kmeans(x=vr[,4:17],centers=tb[,2:15],algorithm="Hartigan-Wong")
fit1

#cluster centers
# extract the means for you
tb<-fit1$centers

#tb: cluster means
# combine tb and cluster column with value from 1 to 4

tb<-data.frame(cbind(tb,cluster=1:3))
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
