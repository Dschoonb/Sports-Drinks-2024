library(FactoMineR)
library(ggplot2)
library(factoextra)
library(readxl)
library(cluster)

# All Users ---------------------------------------------------------------
All<- read_excel("Usage Survey Results.xlsx", 
                                   sheet = "All")
All <- All[,-c(2:6)]

#Distance Matrix
All.clust <- as.matrix(All)
All.clust.d <- dist(All.clust, method = "binary")

#Hierarchical Cluster Analysis
All.clust.cd <- hclust(All.clust.d, method="ward.D2")
plot(All.clust.cd, cex = 0.8, hang= -1)
rect.hclust(All.clust.cd, k=5, border="red")

  #Clusters to Column
All.df <- as.data.frame(All)
All.groups <- cutree(All.clust.cd, k=5)
All.df$Cluster <- All.groups
write.csv(All.df, "All.csv", row.names=FALSE)

#Kmeans
kfit <- kmeans(All.clust.d, 5)
clusplot(as.matrix(All.clust.d), kfit$cluster, color=T, shade=T, labels=2, lines=0)


# Exercise Users ---------------------------------------------------------------
EU<- read_excel("Usage Survey Results.xlsx", 
                 sheet = "Exercisers")

#Distance Matrix
EU.clust <- as.matrix(EU)
EU.clust.d <- dist(EU.clust, method = "binary")

#Hierarchical Cluster Analysis
EU.clust.cd <- hclust(EU.clust.d, method="ward.D2")
plot(EU.clust.cd, cex = 0.8, hang= -1)
rect.hclust(EU.clust.cd, k=5, border="red")

#Clusters to Column
EU.df <- as.data.frame(EU)
EU.groups <- cutree(EU.clust.cd, k=5)
EU.df$Cluster <- EU.groups
write.csv(EU.df, "EU.csv", row.names=FALSE)

#Kmeans
kfit <- kmeans(EU.clust.d, 5)
clusplot(as.matrix(EU.clust.d), kfit$cluster, color=T, shade=T, labels=2, lines=0)
