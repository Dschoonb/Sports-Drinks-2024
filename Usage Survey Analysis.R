library(FactoMineR)
library(ggplot2)
library(factoextra)
library(readxl)
library(cluster)

# All Users ---------------------------------------------------------------
All<- read_excel("Usage Survey Results.xlsx", 
                                   sheet = "All")


All$Cluster
All.clust <- as.matrix(All)
All.clust.d <- dist(All.clust, method = "binary")
All.clust.cd <- hclust(All.clust.d, method="ward.D2")
plot(All.clust.cd, cex = 0.8, hang= -1)
All.groups <- cutree(All.clust.cd, k=3)
rect.hclust(All.clust.cd, k=3, border="red")

All.groups <- as.data.frame(All.groups)
All.df <- as.data.frame(All)
row.names(All.df) <- All.df$`Random ID`
All.df$`Random ID` <- NULL 
All.df$Cluster <- All.groups$All.groups

EBar.pca <- PCA(All.df, axes = c(1,2))

kfit <- kmeans(All.clust.d, 3)
clusplot(as.matrix(All.clust.d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

All.df$poop <- kfit$cluster
  kfit <- as.data.frame(kfit$cluster)
write.csv(data, file="ClusteredRespondents.csv")



# Exercise Users ---------------------------------------------------------------
EU<- read_excel("Usage Survey Results.xlsx", 
                 sheet = "Exercisers")

EU.clust <- as.matrix(EU)
EU.clust.d <- dist(EU.clust, method = "binary")
EU.clust.cd <- hclust(EU.clust.d, method="ward.D2")
plot(EU.clust.cd, hang= -1)
rect.hclust(EU.clust.cd, k=4, border="red")
