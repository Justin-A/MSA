Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")
setwd("~/Desktop/Justin/2018-1/Multivariate_Statistical_Analysis/Data")

# 1
data <- read.csv("Bulls.csv")
pca <- prcomp(data[, c(-1, -2)], scale = FALSE)
pca ; summary(pca)
pca2 <- prcomp(data[, c(-1, -2)], scale = TRUE)
pca2 ; summary(pca2)

# 2
par(mfcol = c(1,2))
screeplot(pca)
screeplot(pca, type = "l")

screeplot(pca2)
screeplot(pca2, type = "l")

# 3
pca$rotation[,c(1,2)]
barplot(pca$rotation[,1], col = "lightblue", main = "PC1")
barplot(pca$rotation[,2], col = "lightblue", main = "PC2")

pca2$rotation[,c(1,2)]
barplot(pca2$rotation[,1], col = "lightblue", main = "PC1")
barplot(pca2$rotation[,2], col = "lightblue", main = "PC2")

# 4
biplot(pca2, main = "Biplot")

# 5
library(ggplot2)
y <- data.frame(data$Breed, pca$x[, c(1,2)])
y$data.Breed <- as.factor(y$data.Breed)
ggplot(y, aes(x = PC1, y = PC2, group = data.Breed)) +
  geom_point(aes(shape = data.Breed, colour = data.Breed), size = 6)

y <- data.frame(data$Breed, pca2$x[, c(1,2)])
y$data.Breed <- as.factor(y$data.Breed)
ggplot(y, aes(x = PC1, y = PC2, group = data.Breed)) +
  geom_point(aes(shape = data.Breed, colour = data.Breed), size = 6)
# 6
y <- data.frame(data$Breed, pca$x[, c(1,2)])
qqnorm(y$PC1) ; qqline(y$PC1)

y <- data.frame(data$Breed, pca2$x[, c(1,2)])
qqnorm(y$PC1) ; qqline(y$PC1)
