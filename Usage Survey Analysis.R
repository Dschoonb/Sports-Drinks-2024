library(FactoMineR)
library(ggplot2)
library(factoextra)
library(readxl)
library(cluster)

# All Users ---------------------------------------------------------------
All<- read_excel("Usage Survey Results.xlsx", 
                                   sheet = "All")
#Distance Matrix
All.clust <- as.matrix(All)
All.clust.d <- dist(All.clust, method = "binary")

#Hierarchical Cluster Analysis
All.clust.cd <- hclust(All.clust.d, method="ward.D2")
plot(All.clust.cd, cex = 0.8, hang= -1)
rect.hclust(All.clust.cd, k=3, border="red")

  #Clusters to Column
All.df <- as.data.frame(All)
All.groups <- cutree(All.clust.cd, k=3)
All.df$Cluster <- All.groups

  #Perform factor analysis
All.clust.means <- aggregate(as.matrix(All.df[,-1]) ~ Cluster, All.df, FUN = mean) #means for each cluster
row.names(All.clust.means) <- All.clust.means$Cluster
All.clust.means$Cluster <- NULL

All.pca <- PCA(All.clust.means, axes = c(1,2))
fviz_pca_var(All.pca, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

#Kmeans
kfit <- kmeans(All.clust.d, 3)
clusplot(as.matrix(All.clust.d), kfit$cluster, color=T, shade=T, labels=2, lines=0)


# Exercise Users ---------------------------------------------------------------
EU<- read_excel("Usage Survey Results.xlsx", 
                 sheet = "Exercisers")

EU.clust <- as.matrix(EU)
EU.clust.d <- dist(EU.clust, method = "binary")
EU.clust.cd <- hclust(EU.clust.d, method="ward.D2")
plot(EU.clust.cd, hang= -1)
rect.hclust(EU.clust.cd, k=4, border="red")
