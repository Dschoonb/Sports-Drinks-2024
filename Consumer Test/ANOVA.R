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
    #This was done to remove the unbalancedness the duplicate sample would give
DataAov2 <- read_excel("Consumer Test/Liking and Appropriateness.xlsx", sheet = "DupCentRem")

str(DataAov2)
DataAov2$Subject <- as.factor(DataAov2$Subject)
DataAov2$Context <- as.factor(DataAov2$Context)
DataAov2$Order <- as.factor(DataAov2$Order)
DataAov2$Sample <- as.factor(DataAov2$Sample)
DataAov2$Salt <- as.factor(DataAov2$Salt)
DataAov2$Sucrose <- as.factor(DataAov2$Sucrose)
str(DataAov2)

#checking for multicolinearity (None found)
colcheck2 <- lm(Liking~ Salt + Sucrose, data = DataAov2)
vif(colcheck2)

Hedonic.aov2 <- aov(Liking~(Context+Salt+Sucrose+Subject), DataAov2)
summary(Hedonic.aov2) 
#Salt and Subject Significant

Approp.aov2 <- aov(Appropriateness~(Context*Salt+Sucrose+Subject), DataAov2)
summary(Approp.aov2) 
#Salt, Subject, and Salt*Context Significant




# Regression --------------------------------------------------------------
DataReg1 <- read_excel("Consumer Test/Liking and Appropriateness.xlsx", sheet = "DupCentRem")

model_liking <- lm(Liking ~ Salt, data = Cardio)
summary(model_liking)
model_liking <- lm(Liking ~ Salt, data = Neutral)
summary(model_liking)

model_appropriateness <- lm(Appropriateness ~ Salt, data = Cardio)
summary(model_appropriateness)
model_appropriateness <- lm(Appropriateness ~ Salt, data = Neutral)
summary(model_appropriateness)
#Sucrose is not significant in our models so MLR is not needed and we can continue without fear of having a confounding factor (sucrose) effect out center treatment

#SLR
library(tidyverse)

Cardio <-  filter(DataReg1, "Cardio" == `Context`)
Neutral <-  filter(DataReg1, "Neutral" == `Context`)


ggplot() +
  geom_jitter(data = Cardio, aes(x = Salt, y = Liking), color = "gold") +
  geom_smooth(data = Cardio, aes(x = Salt, y = Liking), color = "gold", method = "lm", se = FALSE) +
  geom_jitter(data = Neutral, aes(x = Salt, y = Liking), color = "dodgerblue") +
  geom_smooth(data = Neutral, aes(x = Salt, y = Liking), color = "dodgerblue", method = "lm", se = FALSE)

ggplot() +
  geom_jitter(data = Cardio, aes(x = Salt, y = Appropriateness), color = "gold") +
  geom_smooth(data = Cardio, aes(x = Salt, y = Appropriateness), color = "gold", method = "lm", se = FALSE) +
  geom_jitter(data = Neutral, aes(x = Salt, y = Appropriateness), color = "dodgerblue") +
  geom_smooth(data = Neutral, aes(x = Salt, y = Appropriateness), color = "dodgerblue", method = "lm", se = FALSE)


# Box Plots ---------------------------------------------------------------
library(RColorBrewer)
library(readr)
library(dplyr)
library(ggsignif)
library(plyr)


#generating data sets w/ means and standard deviations for bar plot error bars
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)}


Liking.sd <- data_summary(DataReg1, varname="Liking",
                            groupnames=c("Context", "Salt"))
Appropriateness.sd<- data_summary(DataReg1, varname="Appropriateness",
                          groupnames=c("Context", "Salt"))


gg.Like <- ggplot(data = Liking.sd, aes(x=Salt,y=Liking)) +
  geom_col() +
  facet_grid(cols = vars(Context)) +
  geom_errorbar(aes(ymax =Liking+(sd)/sqrt(80), ymin=Liking-(sd)/sqrt(80)), width=.2,
                position=position_dodge(.9)) 
gg.Like



gg.Approp <- ggplot(data = Appropriateness.sd, aes(x=Salt,y=Appropriateness)) +
  geom_col() +
  facet_grid(cols = vars(Context)) +
  geom_errorbar(aes(ymax =Appropriateness+(sd)/sqrt(80), ymin=Appropriateness-(sd)/sqrt(80)), width=.2,
                position=position_dodge(.9)) 
gg.Approp
