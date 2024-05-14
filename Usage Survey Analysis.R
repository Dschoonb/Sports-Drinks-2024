library(FactoMineR)
library(ggplot2)
library(factoextra)
library(readxl)
library(cluster)

# All Users ---------------------------------------------------------------
All<- read_excel("Usage Survey Results_4R.xlsx", 
                                   sheet = "All")
All <- All[,-c(2:6)]

#Distance Matrix
All.clust <- as.matrix(All)
All.clust.d <- dist(All.clust, method = "binary")

#Hierarchical Cluster Analysis
All.clust.cd <- hclust(All.clust.d, method="ward.D2")
All.hca<- plot(All.clust.cd, cex = 0.8, hang= -1)
rect.hclust(All.clust.cd, k=3, border="red")

  #Clusters to Column
All.df <- as.data.frame(All)
All.groups <- cutree(All.clust.cd, k=3)
All.df$Cluster <- All.groups
write.csv(All.df, "All.csv", row.names=FALSE)

# Exercise Users---------------------------------------------------------------
# All
EU<- read_excel("Usage Survey Results_4R.xlsx", 
                 sheet = "Exercisers")

EU <- EU[,-c(2:6)]

#Distance Matrix
EU.clust <- as.matrix(EU)
EU.clust.d <- dist(EU.clust, method = "binary")

#Hierarchical Cluster Analysis
EU.clust.cd <- hclust(EU.clust.d, method="ward.D2")
plot(EU.clust.cd, cex = 0.8, hang= -1)
rect.hclust(EU.clust.cd, k=4, border="red")

#Clusters to Column
EU.df <- as.data.frame(EU)
EU.groups <- cutree(EU.clust.cd, k=4)
EU.df$Cluster <- EU.groups
write.csv(EU.df, "EU.csv", row.names=FALSE)



# Activity Wise -----------------------------------------------------------
#Team Sports
TS<- read_excel("ActivityWise.xlsx", sheet = "Team Sports-R")

#Distance Matrix
TS.clust <- as.matrix(TS)
TS.clust.d <- dist(TS.clust, method = "binary")

#Hierarchical Cluster Analysis
TS.clust.cd <- hclust(TS.clust.d, method="ward.D2")
plot(TS.clust.cd, cex = 0.8, hang= -1)
rect.hclust(TS.clust.cd, k=3, border="red")

#Clusters to Column
TS.df <- as.data.frame(TS)
TS.groups <- cutree(TS.clust.cd, k=3)
TS.df$Cluster <- TS.groups
write.csv(TS.df, "TS.csv", row.names=FALSE)



#Individual Sports
IS<- read_excel("ActivityWise.xlsx", sheet = "Ind Sports-R")

#Distance Matrix
IS.clust <- as.matrix(IS)
IS.clust.d <- dist(IS.clust, method = "binary")

#Hierarchical Cluster Analysis
IS.clust.cd <- hclust(IS.clust.d, method="ward.D2")
plot(IS.clust.cd, cex = 0.8, hang= -1)
rect.hclust(IS.clust.cd, k=2, border="red")

#Clusters to Column
IS.df <- as.data.frame(IS)
IS.groups <- cutree(IS.clust.cd, k=2)
IS.df$Cluster <- IS.groups
write.csv(IS.df, "IS.csv", row.names=FALSE)



#Extreme Sports
EX<- read_excel("ActivityWise.xlsx", sheet = "Xtreme Sports-R")

#Distance Matrix
EX.clust <- as.matrix(EX)
EX.clust.d <- dist(EX.clust, method = "binary")

#Hierarchical Cluster Analysis
EX.clust.cd <- hclust(EX.clust.d, method="ward.D2")
plot(EX.clust.cd, cex = 0.8, hang= -1)
rect.hclust(EX.clust.cd, k=5, border="red")

#Clusters to Column
EX.df <- as.data.frame(EX)
EX.groups <- cutree(EX.clust.cd, k=5)
EX.df$Cluster <- EX.groups
write.csv(EX.df, "EX.csv", row.names=FALSE)



#Cardio
Cardio<- read_excel("ActivityWise.xlsx", sheet = "Cardio-R")

#Distance Matrix
Cardio.clust <- as.matrix(Cardio)
Cardio.clust.d <- dist(Cardio.clust, method = "binary")

#Hierarchical Cluster Analysis
Cardio.clust.cd <- hclust(Cardio.clust.d, method="ward.D2")
plot(Cardio.clust.cd, cex = 0.8, hang= -1)
rect.hclust(Cardio.clust.cd, k=3, border="red")

#Clusters to Column
Cardio.df <- as.data.frame(Cardio)
Cardio.groups <- cutree(Cardio.clust.cd, k=3)
Cardio.df$Cluster <- Cardio.groups
write.csv(Cardio.df, "Cardio.csv", row.names=FALSE)



#Yoga
Yoga<- read_excel("ActivityWise.xlsx", sheet = "Yoga-R")

#Distance Matrix
Yoga.clust <- as.matrix(Yoga)
Yoga.clust.d <- dist(Yoga.clust, method = "binary")

#Hierarchical Cluster Analysis
Yoga.clust.cd <- hclust(Yoga.clust.d, method="ward.D2")
plot(Yoga.clust.cd, cex = 0.8, hang= -1)
rect.hclust(Yoga.clust.cd, k=3, border="red")

#Clusters to Column
Yoga.df <- as.data.frame(Yoga)
Yoga.groups <- cutree(Yoga.clust.cd, k=3)
Yoga.df$Cluster <- Yoga.groups
write.csv(Yoga.df, "Yoga.csv", row.names=FALSE)




# Motivation Wise ---------------------------------------------------------
#Hydration
hyd<- read_excel("MotivationWise.xlsx", 
                sheet = "Hydration-R")

#Distance Matrix
hyd.clust <- as.matrix(hyd)
hyd.clust.d <- dist(hyd.clust, method = "binary")

#Hierarchical Cluster Analysis
hyd.clust.cd <- hclust(hyd.clust.d, method="ward.D2")
plot(hyd.clust.cd, cex = 0.8, hang= -1)
rect.hclust(hyd.clust.cd, k=5, border="red")

#Clusters to Column
hyd.df <- as.data.frame(hyd)
hyd.groups <- cutree(hyd.clust.cd, k=5)
hyd.df$Cluster <- hyd.groups
write.csv(hyd.df, "hyd.csv", row.names=FALSE)




#Vit and min
vit<- read_excel("MotivationWise.xlsx", 
                sheet = "Vitamins and Minerals-R")

#Distance Matrix
vit.clust <- as.matrix(vit)
vit.clust.d <- dist(vit.clust, method = "binary")

#Hierarchical Cluster Analysis
vit.clust.cd <- hclust(vit.clust.d, method="ward.D2")
plot(vit.clust.cd, cex = 0.8, hang= -1)
rect.hclust(vit.clust.cd, k=3, border="red")

#Clusters to Column
vit.df <- as.data.frame(vit)
vit.groups <- cutree(vit.clust.cd, k=3)
vit.df$Cluster <- vit.groups
write.csv(vit.df, "vit.csv", row.names=FALSE)




#Carbs
Carb<- read_excel("MotivationWise.xlsx", 
                sheet = "Carbs-R")

#Distance Matrix
Carb.clust <- as.matrix(Carb)
Carb.clust.d <- dist(Carb.clust, method = "binary")

#Hierarchical Cluster Analysis
Carb.clust.cd <- hclust(Carb.clust.d, method="ward.D2")
plot(Carb.clust.cd, cex = 0.8, hang= -1)
rect.hclust(Carb.clust.cd, k=2, border="red")

#Clusters to Column
Carb.df <- as.data.frame(Carb)
Carb.groups <- cutree(Carb.clust.cd, k=2)
Carb.df$Cluster <- Carb.groups
write.csv(Carb.df, "Carb.csv", row.names=FALSE)




#Electro
Electro<- read_excel("MotivationWise.xlsx", 
                sheet = "Electrolytes-R")

#Distance Matrix
Electro.clust <- as.matrix(Electro)
Electro.clust.d <- dist(Electro.clust, method = "binary")

#Hierarchical Cluster Analysis
Electro.clust.cd <- hclust(Electro.clust.d, method="ward.D2")
plot(Electro.clust.cd, cex = 0.8, hang= -1)
rect.hclust(Electro.clust.cd, k=5, border="red")

#Clusters to Column
Electro.df <- as.data.frame(Electro)
Electro.groups <- cutree(Electro.clust.cd, k=5)
Electro.df$Cluster <- Electro.groups
write.csv(Electro.df, "Electro.csv", row.names=FALSE)





#Meal
Meal<- read_excel("MotivationWise.xlsx", 
                sheet = "Meal-R")

#Distance Matrix
Meal.clust <- as.matrix(Meal)
Meal.clust.d <- dist(Meal.clust, method = "binary")

#Hierarchical Cluster Analysis
Meal.clust.cd <- hclust(Meal.clust.d, method="ward.D2")
plot(Meal.clust.cd, cex = 0.8, hang= -1)
rect.hclust(Meal.clust.cd, k=3, border="red")

#Clusters to Column
Meal.df <- as.data.frame(Meal)
Meal.groups <- cutree(Meal.clust.cd, k=3)
Meal.df$Cluster <- Meal.groups
write.csv(Meal.df, "Meal.csv", row.names=FALSE)





#Perf
Parf<- read_excel("MotivationWise.xlsx", 
                sheet = "Performance-R")

#Distance Matrix
Parf.clust <- as.matrix(Parf)
Parf.clust.d <- dist(Parf.clust, method = "binary")

#Hierarchical Cluster Analysis
Parf.clust.cd <- hclust(Parf.clust.d, method="ward.D2")
plot(Parf.clust.cd, cex = 0.8, hang= -1)
rect.hclust(Parf.clust.cd, k=2, border="red")

#Clusters to Column
Parf.df <- as.data.frame(Parf)
Parf.groups <- cutree(Parf.clust.cd, k=2)
Parf.df$Cluster <- Parf.groups
write.csv(Parf.df, "Parf.csv", row.names=FALSE)
