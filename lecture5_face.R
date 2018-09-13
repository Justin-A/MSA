setwd("/Users/justin/Desktop/Justin/2018-1/Multivariate_Statistical_Analysis/Data")

data = read.csv("face100.csv" , stringsAsFactors=F)
dim(data) 

# extract the images data
im.train = data$Image
data$Image = NULL

# each image is a vector of 96*96 pixels (96*96 = 9216).
im.train2 = c()
for (i in 1:dim(data)[1]){
  im.train2 = rbind(im.train2, as.integer(unlist(strsplit(im.train[i], " "))))
} # 숫자들을 전부 나눈 후 integer + unlist, 9216 각 픽셀 값

# show picture 
idx=22
im = matrix(data=rev(im.train2[idx,]), nrow=96, ncol=96)
image(1:96, 1:96, im, col=gray((0:255)/255))

# 사진을 압축하여 진행
# Apply PCA
pca = prcomp(im.train2, scale = TRUE) ## using correlation matrix

print(pca)
summary(pca)
plot(pca, type = "l")

pca$x
dim(pca$x)

# Image reconstruction
dim = 10     
restr = pca$x[,1:dim] %*% t(pca$rotation[,1:dim]) # 10차원으로 재구성

# unscale and uncenter the data
if(all(pca$scale != FALSE)){
  restr = scale(restr, center = FALSE , scale=1/pca$scale)
}
if(all(pca$center != FALSE)){
  restr = scale(restr, center = -1 * pca$center, scale=FALSE)
}

# plot your original image and reconstructed image
idx=22
par(mfcol=c(1,2), mar=c(1,1,2,1))
im = matrix(data=rev(im.train2[idx,]), nrow=96, ncol=96)
image(1:96, 1:96, im, col=gray((0:255)/255))

rst = matrix(data=rev(restr[idx,]), nrow=96, ncol=96)
image(1:96, 1:96, rst, col=gray((0:255)/255))
