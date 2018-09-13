### open closed data
setwd("/Users/justin/Desktop/Justin/2018-1/Multivariate_Statistical_Analysis/Data")
data=read.csv("open_closed.csv")
pairs(data)
cor(data)

pca = prcomp(data, scale=T)
summary(pca)
plot(pca, type='l')

data_s=scale(data)
plot(as.matrix(data_s) %*% pca$rotation[,1] ,pca$x[,1])
abline(0,1)


### Hepathlon data


library(HSAUR2)

heptathlon$hurdles = with(heptathlon, max(hurdles)-hurdles)
heptathlon$run200m = with(heptathlon, max(run200m)-run200m)
heptathlon$run800m = with(heptathlon, max(run800m)-run800m)
score = which(colnames(heptathlon) == "score")

round(cor(heptathlon[,-score]), 2)
plot(heptathlon[,-score])
library(psych)
pairs.panels(heptathlon[,-score])

idx = heptathlon$hurdles==min(heptathlon$hurdles)
heptathlon[idx,]
pairs(heptathlon[,-score],col=idx+1, pch=idx+1)

heptathlon2 = heptathlon[-grep("PNG", rownames(heptathlon)),]
round(cor(heptathlon2[,-score]), 2)
pairs.panels(heptathlon2[,-score])


heptathlon_pca = prcomp(heptathlon2[, -score], scale = TRUE)
print(heptathlon_pca)
summary(heptathlon_pca)

heptathlon_pca$rotation
plot(heptathlon_pca,type="l")
cor(heptathlon2$score, heptathlon_pca$x[,1])
plot(heptathlon2$score, heptathlon_pca$x[,1])

tmp = heptathlon[,-score]
rownames(tmp) = abbreviate(gsub(" \\(.*", "", rownames(tmp)))
biplot(prcomp(tmp, scale=TRUE), col=c("black", "darkgray"),
       xlim = c(-0.5, 0.7), cex=0.7)
