x <- c(1, 3, 6, 12, 20)
d <- dist(x) ; d # methid = "euclidean", "maximum", "manhattan" ...
?dist()
hc <- hclust(d) # Cluster Result, method = "single", "complete", "average", "ward.D", "ward.D2" ...
hc <- hclust(d, method = "single") # ward.D -> distance, ward.D2 -> distance^2
plot(hc)

USArrests # USs Crime Data
head(USArrests)
d <- dist(USArrests)
hc1 <- hclust(d, method = "single") ; plot(hc1)
hc2 <- hclust(d, method = "complete") ; plot(hc2)
hc3 <- hclust(d, method = "average") ; plot(hc3)
hc4 <- hclust(d, method = "ward.D") ; plot(hc4)
hc5 <- hclust(d, method = "ward.D2") ; plot(hc5)

cluster2 <- cutree(hc2, h = 90) # height, 그룹의 개수를 기준으로 끊어서 표현가능 (여기선 height= 90)
table(cluster2) ; plot(cluster2)
plot(USArrests[, 1], USArrests[, 2], col = cluster2, pch = cluster2)
text(USArrests[,1])

for(i in 1:3){
  for (j in (i+1):4){
    plot(USArrests[,i], USArrests[,j],
         xlab = names(USArrests)[i],
         ylab = names(USArrests)[j],
         type = "n")
    text(USArrests[,i], USArrests[,j],
         rownames(USArrests),
         col = cluster2,
         cex = 0.7)
    }
}
