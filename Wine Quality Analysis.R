# Wine Quality Analysis

install.packages('ggplot2')
install.packages('dplyr')
install.packages('randomForest')
install.packages('ggthemes')
install.packages('corrplot')
install.packages('reshape2')

#Loading the Libraries
library(ggplot2)
library(dplyr)
library(randomForest)
library(ggthemes)
library(corrplot)
library(reshape2)
# Importing the dataset

wine_df<- read.csv('C:\\R assignment\\winequalityN.csv')
View(wine_df)

#Assigning the variables for good or bad quality wine
wine_df$good.wine<-ifelse(wine_df$quality>6,1,0)
str(wine_df)

#Here we will be only working on unbalanced dataset where 19.67% of 6497 is considered as good quality

sapply(wine_df, function(x) sum(is.na(x)))

#Filling the missing values with the means of the columns
wine_df$fixed.acidity[is.na(wine_df$fixed.acidity)] <- mean(wine_df$fixed.acidity, na.rm = TRUE)
wine_df$volatile.acidity[is.na(wine_df$volatile.acidity)] <- mean(wine_df$volatile.acidity, na.rm = TRUE)
wine_df$citric.acid[is.na(wine_df$citric.acid)] <- mean(wine_df$citric.acid, na.rm = TRUE)
wine_df$residual.sugar[is.na(wine_df$residual.sugar)] <- mean(wine_df$residual.sugar, na.rm = TRUE)
wine_df$chlorides[is.na(wine_df$chlorides)] <- mean(wine_df$chlorides, na.rm = TRUE)
wine_df$pH[is.na(wine_df$pH)] <- mean(wine_df$pH, na.rm = TRUE)
wine_df$sulphates[is.na(wine_df$sulphates)] <- mean(wine_df$sulphates, na.rm = TRUE)

sapply(wine_df, function(x) sum(is.na(x)))

#Performing EDA

#Finding the correlation between the variables
plot(wine_df)

#Correlation Heatmap of Variables
corrplot(cor(select_if(wine_df,is.numeric)))

#From the graph we can infer that alocohol and density are highly correlated
# Also sugar content and density are correlated with each other

#Distribution of wine quality ratings
ggplot(wine_df,aes(x=quality))+geom_bar(stat = "count",position = "dodge")+
  scale_x_continuous(breaks = seq(3,8,1))+
  ggtitle("Distribution of Wine Quality Ratings")+
  theme_classic()
#Distribution of good/bad quality wines
ggplot(wine_df,aes(x=good.wine,fill=factor(good.wine)))+geom_bar(stat = "count",position = "dodge")+
  scale_x_continuous(breaks = seq(0,1,1))+
  ggtitle("Distribution of Good/Bad Wines")+
  theme_classic()
#From the graph we can conclude that good quality of wines are outnumbered by bad quality of wines by a large margin
# Most of the wines are of moderate quality i.e those are rated as 5 or 6
# However, there are some wines which are of poor quality and they are rated as 3 or 4
# Ratings of 7 have been recieved by most of the good quality wines

#Fixed Acidity and wine quality
ggplot(wine_df,aes(x=fixed.acidity,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(fixed.acidity[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(fixed.acidity[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(4,16,1))+
  xlab(label = "Fixed Acidity Level")+
  ggtitle("Distribution of Fixed Acidity Levels")+
  theme_classic()
#Citric Acid and Wine Quality
ggplot(wine_df,aes(x=citric.acid,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(citric.acid[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(citric.acid[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,1,0.1))+
  xlab(label = "Citric Acid Level")+
  ggtitle("Distribution of Citric Acid Levels")+
  theme_classic()

#Residual Sugar and Wine Quality
ggplot(wine_df,aes(x=residual.sugar,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(residual.sugar[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(residual.sugar[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0.5,15.5,1))+
  xlab(label = "Residual Sugar Level")+
  ggtitle("Distribution of Residual Sugar Levels")+
  theme_classic()

#Free Sulphur dioxide and wine quality
ggplot(wine_df,aes(x=free.sulfur.dioxide,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(free.sulfur.dioxide[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(free.sulfur.dioxide[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,72,8))+
  xlab(label = "Free Sulfur Dioxide Level")+
  ggtitle("Distribution of Free Sulfur Dioxide Levels")+
  theme_classic()

#Total Sulphur Dioxide and wine quality
ggplot(wine_df,aes(x=total.sulfur.dioxide,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(total.sulfur.dioxide[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(total.sulfur.dioxide[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,300,20))+
  xlab(label = "Total Sulfur Dioxide Level")+
  ggtitle("Distribution of Total Sulfur Dioxide Levels")+
  theme_classic()



#Free Sulphur dioxide and wine quality
ggplot(wine_df,aes(x=free.sulfur.dioxide,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(free.sulfur.dioxide[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(free.sulfur.dioxide[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,72,8))+
  xlab(label = "Free Sulfur Dioxide Level")+
  ggtitle("Distribution of Free Sulfur Dioxide Levels")+
  theme_classic()

#Total Sulphur Dioxide and wine quality
ggplot(wine_df,aes(x=total.sulfur.dioxide,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(total.sulfur.dioxide[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(total.sulfur.dioxide[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,300,20))+
  xlab(label = "Total Sulfur Dioxide Level")+
  ggtitle("Distribution of Total Sulfur Dioxide Levels")+
  theme_classic()



#Free Sulphur dioxide and wine quality
ggplot(wine_df,aes(x=free.sulfur.dioxide,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(free.sulfur.dioxide[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(free.sulfur.dioxide[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,72,8))+
  xlab(label = "Free Sulfur Dioxide Level")+
  ggtitle("Distribution of Free Sulfur Dioxide Levels")+
  theme_classic()

#Total Sulphur Dioxide and wine quality
ggplot(wine_df,aes(x=total.sulfur.dioxide,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(total.sulfur.dioxide[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(total.sulfur.dioxide[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,300,20))+
  xlab(label = "Total Sulfur Dioxide Level")+
  ggtitle("Distribution of Total Sulfur Dioxide Levels")+
  theme_classic()

#Free Sulphur dioxide and wine quality
ggplot(wine_df,aes(x=free.sulfur.dioxide,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(free.sulfur.dioxide[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(free.sulfur.dioxide[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,72,8))+
  xlab(label = "Free Sulfur Dioxide Level")+
  ggtitle("Distribution of Free Sulfur Dioxide Levels")+
  theme_classic()

#Total Sulphur Dioxide and wine quality
ggplot(wine_df,aes(x=total.sulfur.dioxide,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(total.sulfur.dioxide[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(total.sulfur.dioxide[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0,300,20))+
  xlab(label = "Total Sulfur Dioxide Level")+
  ggtitle("Distribution of Total Sulfur Dioxide Levels")+
  theme_classic()

#Density and Wine Quality
ggplot(wine_df,aes(x=density,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(density[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(density[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(0.9,1.1,0.05))+
  xlab(label = "Red Wine Density Level")+
  ggtitle("Distribution of Wine Density Levels")+
  theme_classic()

#PH level and Wine Quality
ggplot(wine_df,aes(x=pH,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(pH[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(pH[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(2.5,5,0.5))+
  xlab(label = "Wine PH Level")+
  ggtitle("Distribution of Wine PH Levels")+
  theme_classic()

#From this graph we are able to infer that the PH Levels of both bad and good quality wines are almost equal

#Alcohol and Wine Quality
ggplot(wine_df,aes(x=alcohol,fill=factor(good.wine)))+geom_density(alpha=0.25)+
  geom_vline(aes(xintercept=mean(alcohol[good.wine==0],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(alcohol[good.wine==1],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  scale_x_continuous(breaks = seq(8,15,1))+
  xlab(label = "Alcohol Level")+
  ggtitle("Distribution of Alcohol Levels")+
  theme_classic()
#From this graph we are able to make out that density of bad quality wines are almost 0.5

#Creating a Model
#Random Forest Model
#Removing the type column from the dataset
df <- select_if(wine_df, is.numeric)
RF<-randomForest(factor(good.wine)~.-quality,df,ntree=200)
RF

#Since we got the error of 10.3% it means the overall accuracy of our model is almosr 90%
# Hence we can say that it is good fit model

# Variable or Feature Importance
#Getting Importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

#We will have to use the ggplot for visualizing the relative importance of Variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_classic()

#We can say that the level of alcohol in a wine is the most discriminating factor.