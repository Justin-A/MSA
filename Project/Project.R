library(HSAUR2)
library(leaps)
library(psych)

rm(list = ls())
setwd('C:/R')

#쓰레기 배출데이터
hope<-read.csv("hope.csv")
#쓰레기 처리데이터
equ<-read.csv("newequip.csv")

names<-as.character(hope$자치구)
rownames(hope)<-make.names(names, unique=TRUE)
rownames(equ)<-make.names(names, unique=TRUE)

hope2014<-hope[hope$기간==2014,];hope2014<-hope2014[,-c(1,2,6,7,8)]
hope2015<-hope[hope$기간==2015,];hope2015<-hope2015[,-c(1,2,6,7,8)]
hope2016<-hope[hope$기간==2016,];hope2016<-hope2016[,-c(1,2,6,7,8)]

equ2014<-equ[equ$기간==2014,];equ2014<-equ2014[,c(-1,-2)]
equ2015<-equ[equ$기간==2015,];equ2015<-equ2015[,c(-1,-2)]
equ2016<-equ[equ$기간==2016,];equ2016<-equ2016[,c(-1,-2)]


equ2<-equ[,-c(1,2)]
hope2<-hope[,-c(1,2,6,7,8)]

final<-cbind(hope2,equ2)
final2016<-cbind(hope2016,equ2016)
rownames(final)

dim(final2016)
cor(final2016)
pairs.panels()
pairs(data2) #PCA는 상호간 관계가 없으면 할 수 가 없음

#data.14 <- data.14[,!names(data.14) %in% c("기간","자치구")]

#####코릴레이션 낮은값 빼내는 [for-loop] - 생략
c <- 0
for (i in 1:73) {
  if (abs(cor(data.14[,3],data.14[,i])) < 0.5) {
    print(colnames(data.14)[i])
    c <- c+1
    print(c)
  } 
}

####### 변수선택+회귀분석

step(lm(배출량~., hope2), direction="both")
fit2<-lm(formula = 배출량 ~ 면적 + 출생률 + 사망건수.명. + 사망률 + 
     관광호텔업.호텔수 + 관광호텔업.객실수 + 저체중.BMI.18.5. + 
     체질량지수.BMI..평균. + culture2 + market + 휴게음식점 + 
     유흥주점 + 위탁급식영업, data = hope2)
summary(fit2)


###### 시각ㅎ
library(ggplot2)
library(reshape2)



final2<- subset(final, select=c(배출량, 자치.인원, 업체.인원))
final2$지원인원 <- apply(final2[,2:3],1,sum)
final2$area<-rownames(final2);final2<-final2[,-c(2,3)]
final2<- head(final2[order(final2$배출량, decreasing = T),],10)
final3<-melt(final2, id.vars="area")

final20162<- subset(final2016, select=c(배출량, 자치.인원, 업체.인원))
final20162$지원인원 <- apply(final20162[,2:3],1,sum)
final20162$area<-rownames(final20162);final20162<-final20162[,-c(2,3)]
final20162<- head(final20162[order(final20162$배출량, decreasing = T),],10)
final20163<-melt(final20162, id.vars="area")

ggplot(final3, aes(x=area, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  theme(axis.text.x = element_text(face="bold", 
                                   size=8, angle=90)

)

#pca 회귀
pca <- prcomp(hope2[,-3],scale=T)
summary(pca)
plot(pca,type='l')
a <- pca$x[,1:5]
a <- cbind(a,hope2[,3])
head(a)
colnames(a)[6] <- "배출량"
a <- data.frame(a)
fit.1 <- lm(배출량~., data=a)
summary(fit.1)

#biplot
biplot(pca, cex=0.7)


data.p<-data.frame(pca$rotation[,c(1:5)])
data.p$variable<-rownames(data.p)


#data.p$variable<-substr(rownames(data.p),1,3)
#data.p[data.p$variable=="중구.",]$variable <-"중구"
#geom_text(aes(label=PC1), vjust=1.6, color="white", size=3.5)
ggplot(aes(x= variable , y= PC1), data= data.p) + 
  geom_bar(stat= "identity", fill= "skyblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(face="bold", size=8, angle=90)
          
        
  )

ggplot(aes(x= variable , y= PC2), data= data.p) + 
  geom_bar(stat= "identity", fill= "skyblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(face="bold", size=8, angle=90)
        
        
  )

data.p2<-pca$x[,1:5]
data.p2<-cbind(hope2, data.p2)
data.p2$자치구<-substr(rownames(data.p2),1,3)
data.p2[data.p2$자치구=="중구.",]$자치구 <-"중구"
colnames(data.p2)
ggplot(data=data.p2, aes(x=PC1, y= PC2)) + 
  geom_point(aes(col=factor(자치구), size= 배출량))+
  theme_minimal()+
  labs(col="자치구", size= "배출량") + ggtitle("PC2~PC1") +
  geom_text(aes(label=자치구), vjust=1.6, color="black", size=3.5)

summary(pca)

qqnorm(data.p2[,"PC1"])     
qqline(data.p2[,"PC1"], col = "blue", lwd=1.5)



#PC1, PC2 간 상관관계가 없는 건 당연(다만 이상치 같은것을 보기위해 보는 그래프)
plot(pca$x[,1], pca$x[,2]) 
pc1 <- pca$x[, 1]
pc2 <- pca$x[, 2]
cor(pc1, pc2)




