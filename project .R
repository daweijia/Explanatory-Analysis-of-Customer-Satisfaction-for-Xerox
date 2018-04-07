setwd("/Users/jiadawei/Desktop/Marketing Project")
library(readxl)
Xeroxdb <- read_xlsx("Xerox Data File 1 - Enterprise Accts Only.xlsx")
Xeroxdb.pca <- Xeroxdb
Xeroxdb.pca <- Xeroxdb.pca[,-c(1:6)]
Xeroxdb.pca <- Xeroxdb.pca[,-c(18:23)]
Xeroxdb.pca[is.na(Xeroxdb.pca)==TRUE]=0
pca.out = prcomp(Xeroxdb.pca,  scale=T)
plot(pca.out) #pc1 pc2
pca.var = pca.out$sdev^2
pca.pve = data.frame(pve=pca.var/sum(pca.var))
plot(pca.pve$pve)
plot(cumsum(pca.pve$pve))

scores = as.data.frame(predict(pca.out, Xeroxdb.pca))
rot = as.data.frame(pca.out$rotation) 
cor(scores$PC1, Xeroxdb.pca$Recommend) #+
cor(scores$PC2, Xeroxdb.pca$Recommend) #+
library(ggplot2)
ggplot(scores, aes(PC1, PC2)) + geom_text(aes(label=rownames(scores)),hjust=0, vjust=0)
clus.out = kmeans(scores[,1:2], 2)
cluster = as.character(clus.out$cluster)
ggplot(scores, aes(PC1, PC2)) +
  geom_text(aes(label=cluster, color=cluster),hjust=0, vjust=0)
clus.out2 = kmeans(scores[,1:2], 3)
cluster2 = as.character(clus.out2$cluster)
ggplot(scores, aes(PC1, PC2)) +
  geom_text(aes(label=cluster2, color=cluster2),hjust=0, vjust=0)

seg1c <- which(cluster==1)
seg2c <- which(cluster==2)
seg1 <- Xeroxdb[seg1c,]
seg2 <- Xeroxdb[seg2c,]
median(seg1$Recommend) #8
median(seg2$Recommend) #5

#why 8 and 5?
library('glmnet')
seg1[is.na(seg1)==TRUE]=0
seg1.num <- seg1[,7:90]
lassoFit = glmnet(model.matrix(~.,data=seg1.num[,-59]),seg1.num$Recommend,alpha=1)
plot(lassoFit,xvar='lambda',sub='The X-Axis is the penalty parameters (logged)')
predict(lassoFit,s = .1, type = 'coefficients')

seg2[is.na(seg2)==TRUE]=0
seg2.num <- seg2[,7:90]
lassoFit2 = glmnet(model.matrix(~.,data=seg2.num[,-59]),seg2.num$Recommend,alpha=1)
plot(lassoFit2,xvar='lambda',sub='The X-Axis is the penalty parameters (logged)')
predict(lassoFit2,s = .08, type = 'coefficients')

save(Xeroxdb,file = "xeroxdb.rda")

write.csv(seg1,file = "seg1.csv")
write.csv(seg2,file = "seg2.csv")

summary(seg1)
summary(seg2)
