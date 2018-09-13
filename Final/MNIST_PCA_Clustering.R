setwd("~/Desktop")

data <- read.csv("mnist_1000.csv")
label=data$label # 실제 label 분리 저장
data=as.matrix(data[,-785]) # pixel 자료 분리 저장
View(data)
# 1
# 1번부터 9번까지 매트릭스로 추출하여 그림을 그려보았습니다.
pic1 <- matrix(data[1,], nrow = 28, ncol = 28, byrow = FALSE) ; pic1
pic2 <- matrix(data[2,], nrow = 28, ncol = 28, byrow = FALSE) ; pic2
pic3 <- matrix(data[3,], nrow = 28, ncol = 28, byrow = FALSE) ; pic3
pic4 <- matrix(data[4,], nrow = 28, ncol = 28, byrow = FALSE) ; pic4
pic5 <- matrix(data[5,], nrow = 28, ncol = 28, byrow = FALSE) ; pic5
pic6 <- matrix(data[6,], nrow = 28, ncol = 28, byrow = FALSE) ; pic6
pic7 <- matrix(data[7,], nrow = 28, ncol = 28, byrow = FALSE) ; pic7
pic8 <- matrix(data[8,], nrow = 28, ncol = 28, byrow = FALSE) ; pic8
pic9 <- matrix(data[9,], nrow = 28, ncol = 28, byrow = FALSE) ; pic9
image(1:28, 1:28, pic1, col=gray((0:255)/255))
image(1:28, 1:28, pic2, col=gray((0:255)/255))
image(1:28, 1:28, pic3, col=gray((0:255)/255))
image(1:28, 1:28, pic4, col=gray((0:255)/255))
image(1:28, 1:28, pic5, col=gray((0:255)/255))
image(1:28, 1:28, pic6, col=gray((0:255)/255))
image(1:28, 1:28, pic7, col=gray((0:255)/255))
image(1:28, 1:28, pic8, col=gray((0:255)/255))
image(1:28, 1:28, pic9, col=gray((0:255)/255))

# 2
pca <- prcomp(data)
pca$x
summary(pca)
summary(pca)[[6]][3,][100] # 92.691% 까지 설명 가능

# 3
# 차원축소를 통한 그림 확인
dim = 100
restr = pca$x[,1:dim] %*% t(pca$rotation[,1:dim])

re_pic1 <- matrix(restr[1,], nrow = 28, ncol = 28, byrow = FALSE)
re_pic2 <- matrix(restr[2,], nrow = 28, ncol = 28, byrow = FALSE)
re_pic3 <- matrix(restr[3,], nrow = 28, ncol = 28, byrow = FALSE)
re_pic4 <- matrix(restr[4,], nrow = 28, ncol = 28, byrow = FALSE)
re_pic5 <- matrix(restr[5,], nrow = 28, ncol = 28, byrow = FALSE)
re_pic6 <- matrix(restr[6,], nrow = 28, ncol = 28, byrow = FALSE)
re_pic7 <- matrix(restr[7,], nrow = 28, ncol = 28, byrow = FALSE)
re_pic8 <- matrix(restr[8,], nrow = 28, ncol = 28, byrow = FALSE)
re_pic9 <- matrix(restr[9,], nrow = 28, ncol = 28, byrow = FALSE)
image(1:28, 1:28, re_pic1, col=gray((0:255)/255))
image(1:28, 1:28, re_pic2, col=gray((0:255)/255))
image(1:28, 1:28, re_pic3, col=gray((0:255)/255))
image(1:28, 1:28, re_pic4, col=gray((0:255)/255))
image(1:28, 1:28, re_pic5, col=gray((0:255)/255))
image(1:28, 1:28, re_pic6, col=gray((0:255)/255))
image(1:28, 1:28, re_pic7, col=gray((0:255)/255))
image(1:28, 1:28, re_pic8, col=gray((0:255)/255))
image(1:28, 1:28, re_pic9, col=gray((0:255)/255))
# 실제 그림보다 흐릿해진 이미지를 확인할 수 있다.

# 4
redata <- c(re_pic1, re_pic2, re_pic3, re_pic4, re_pic5, re_pic6, re_pic7, re_pic8, re_pic9)
redata2 <- as.matrix(redata, nrow = 9)
dim(redata2) <- c(9,784)

set.seed(1)
library("cluster")
kmeans1 <- kmeans(redata2, 3, nstart = 10) 
kmeans1$cluster
label[1:10]
# KMEANS : 1 1 1 3 3 3 2 2 2
# 실제 라벨 : 1 0 1 4 0 0 7 3 5 3
# 비교해보았을때 3개가 일치하였다.