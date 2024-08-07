library(readxl)

#The issue with my current dataset is that it was made to be used with a projection map like that given by GPA, unfortunately right now I do not have that (though i might be able to if i am quick enough (maybe next week??)).
#As such i must use some shotty methods to create a halfway decent presentation. These include using simple methods like MLR and using my salt and sugar modifications as factors instead of sensory attribute intensities. 
# In order to use MLR i must at the very least remove the duplicated center value.
# The next option is removing both center values because of the fact that the design will introduce multicoliniarity 

# ANOVA -------------------------------------------------------------------
library(agricolae)

#Full Dataset (1)
DataAov1 <- read_excel("Consumer Test/Liking and Appropriateness.xlsx")

str(DataAov1)
DataAov1$Subject <- as.factor(DataAov1$Subject)
DataAov1$Context <- as.factor(DataAov1$Context)
DataAov1$Order <- as.factor(DataAov1$Order)
DataAov1$Sample <- as.factor(DataAov1$Sample)
DataAov1$Salt <- as.factor(DataAov1$Salt)
DataAov1$Sucrose <- as.factor(DataAov1$Sucrose)
str(DataAov1)


Hedonic.aov1 <- aov(Liking~(Context*Sample+Subject+Order), DataAov1)
summary(Hedonic.aov1) 

Approp.aov1 <- aov(Appropriateness~(Context*Sample+Subject+Order), DataAov1)
summary(Approp.aov1) 






#Duplicate Center Removed Dataset (2)
DataAov2 <- read_excel("Consumer Test/Liking and Appropriateness.xlsx", sheet = "DupCentRem")

str(DataAov2)
DataAov2$Subject <- as.factor(DataAov2$Subject)
DataAov2$Context <- as.factor(DataAov2$Context)
DataAov2$Order <- as.factor(DataAov2$Order)
DataAov2$Sample <- as.factor(DataAov2$Sample)
DataAov2$Salt <- as.factor(DataAov2$Salt)
DataAov2$Sucrose <- as.factor(DataAov2$Sucrose)
str(DataAov2)


Hedonic.aov2 <- aov(Liking~(Context*Sample+Subject+Order), DataAov2)
summary(Hedonic.aov2) 

Approp.aov2 <- aov(Appropriateness~(Context*Sample+Subject+Order), DataAov2)
summary(Approp.aov2) 

    #checking for multicolinearity (Non found)
colcheck2 <- lm(Liking~ Salt + Sucrose, data = DataAov2)
vif(colcheck2)







#Both Centers Removed Dataset (3)
DataAov3 <- read_excel("Consumer Test/Liking and Appropriateness.xlsx", sheet = "CentRem")

str(DataAov3)
DataAov3$Subject <- as.factor(DataAov3$Subject)
DataAov3$Context <- as.factor(DataAov3$Context)
DataAov3$Order <- as.factor(DataAov3$Order)
DataAov3$Sample <- as.factor(DataAov3$Sample)
DataAov3$Salt <- as.factor(DataAov3$Salt)
DataAov3$Sucrose <- as.factor(DataAov3$Sucrose)
str(DataAov3)


Hedonic.aov3 <- aov(Liking~(Context*Salt + Sucrose + Subject), DataAov3)
summary(Hedonic.aov3) 

Approp.aov3 <- aov(Appropriateness~(Context*Salt + Sucrose + Subject), DataAov3)
summary(Approp.aov3) 






# MLR ---------------------------------------------------------------------

#Full Dataset
#Cannot be performed with full dataset due to unbalanced design with center
#DupCentRemoved (Issues with multicolinearity with center observation) 

Data3MLR <- read_excel("Consumer Test/Liking and Appropriateness.xlsx", sheet = "CentRem")













