library(agricolae)
library(readxl)
Data <- read_excel("Consumer Test/Liking and Appropriateness.xlsx")

#ANOVAs
str(Data)

Data$Subject <- as.factor(Data$Subject)
Data$Context <- as.factor(Data$Context)
Data$Sample <- as.factor(Data$Sample)
Data$Order <- as.factor(Data$Order)
Data$Salt <- as.factor(Data$Salt)
Data$Sugar <- as.factor(Data$Sugar)

str(Data)


Hedonic.aov <- aov(Liking~(Context*Salt + Context*Sugar + Subject + Order + Sample), Data)
summary(Hedonic.aov) 
LSD.test(Hedonic.aov,"Sample",console=TRUE)


Approp.aov <- aov(Appropriateness~(Context*Salt + Context*Sugar + Subject + Order+ Sample), Data)
summary(Approp.aov) 
LSD.test(Approp.aov,"Sample",console=TRUE)


#MLR
whatscore <- lm (Liking~Data$Salt+ Data$Sugar, data = Data)
summary(whatscore)

library(scatterplot3d) # to make 3d plot
s3d <- scatterplot3d(x=Salt, y=Sugar, z=Liking, pch = 16, color="steelblue",
                     xlab = "Salt",
                     ylab = "Sugar",
                     zlab = "Liking",
                     angle=100) + conto
s3d$plane3d(whatscore)

s3d$contour3d()
