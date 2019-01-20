


data <- iris
newiris <- data
newiris$Species <- NULL  #对训练数据去掉分类标记 

kc <- kmeans(newiris, 8)  #分类模型训练 
fitted(kc)  #查看具体分类情况 
table(iris$Species, kc$cluster)  #查看分类概括 


windows()
#聚类结果可视化 
plot(newiris[c("Sepal.Length", "Sepal.Width")], col = kc$cluster, pch = as.integer(iris$Species))
#不同的颜色代表不同的聚类结果，不同的形状代表训练数据集的原始分类情况。 
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8, cex=2)

