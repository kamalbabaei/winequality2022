# winequality2022
#install required packages and libraries
install.packages("magrittr")
install.packages("dplyr")  # alternative installation of the %>%install.packages("caret")
install.packages("tidyverse")
install.packages("plsRglm")
install.packages("plsr")
install.packages("pls")
install.packages("stats19")
install.packages("plsdof")
install.packages("caret")
install.packages("plot.matrix")
install.packages("unbalanced")
install.packages("lattice")
install.packages("outliers")
install.packages("moments")
install.packages("GGally")
library(GGally)
library(moments)
library(outliers)
library(pls)
library(tidyverse)
library(plsRglm)
library(stringr)
library(caret)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(caret)
library(plsdof)
library(GGally)
library('plot.matrix')
library(unbalanced)
#Loading the data now with one HOT Encoding

fileURL1 = 'https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv'
fileURL2 = 'https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv'


white = read.csv(fileURL1, header = T, sep = ";")
red = read.csv(fileURL2, header = T, sep = ";")


#making a new colomn for color
white <- white %>% add_column(color = 'white',.after = "alcohol") 

red <- red %>% add_column(color = 'red' ,.after = "alcohol") 


#mixing the ingredients 
data <- rbind(white, red)


# shuffle the data frame by rows to homogeneously mix both types of white and red wine rows
data <- data[sample(1:nrow(data)), ]


#getting some characteristics and structure of the data
View(data)
dim(data)
head(data)
tail(data)
summary(data)
str(data)

# Looking for missing value
sum(is.na(data))

#I have never been this happy <3 #No to missing values

###Digging into single variables:

#Understand the Distribution of Alcohol
summary(data$alcohol)
ggplot(data, aes(x=alcohol, fill=color, color=color)) +
  geom_histogram(position="identity", breaks = seq(8,15,0.5), alpha=0.5)

ggplot(data) +
  aes(x = "Boxplot for alcohol", y = alcohol) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#Remving outliers
Q <- quantile(data$alcohol, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(data$alcohol)
data<- subset(data, data$alcohol > (Q[1] - 1.5*iqr) & data$alcohol < (Q[2]+1.5*iqr))


#Understand the Distribution of the density of wine
summary(data$density)
ggplot(data, aes(x=density, fill=color, color=color)) +
  geom_histogram(position="identity",  breaks = seq(0.9870,1.02, 0.001), alpha=0.5)
ggplot(data) +
  aes(x = "Boxplot for density", y = density) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#Remving outliers
Q <- quantile(data$density, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(data$density)
data<- subset(data, data$density > (Q[1] - 1.5*iqr) & data$density < (Q[2]+1.5*iqr))


#Understand the Distribution of Level of Volatile Acidity
summary(data$volatile.acidity)
qplot(volatile.acidity, data = data, fill = color, binwidth = 0.001)
scale_x_log10(breaks = seq(min(data$volatile.acidity), max(data$volatile.acidity), 0.1))
ggplot(data) +
  aes(x = "Boxplot for Volatile Acidity", y = volatile.acidity) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#Remving outliers
Q <- quantile(data$volatile.acidity, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(data$volatile.acidity)
data<- subset(data, data$volatile.acidity > (Q[1] - 1.5*iqr) & data$volatile.acidity < (Q[2]+4.5*iqr))


#Understand the Distribution of quality
summary(data$quality)
table(data$quality)
ggplot(data, aes(x=quality, fill=color, color=color)) +
geom_histogram(position="identity", breaks = seq(3, 9, 1), alpha=0.5)
ggplot(data) +
  aes(x = "Boxplot for quality", y = quality) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()


#Understand the Distribution of residual sugar
summary(data$residual.sugar)
table(data$residual.sugar)
ggplot(data, aes(x=residual.sugar, fill=color, color=color)) +
  geom_histogram(position="identity", breaks = seq(0.5, 10, 0.5), alpha=0.5)
ggplot(data) +
  aes(x = "Boxplot for residual sugar", y = residual.sugar) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#Remving outliers
Q <- quantile(data$residual.sugar, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(data$residual.sugar)
data<- subset(data, data$residual.sugar <25)


#Understand the Distribution of Level of Chlorides
summary(data$chlorides)
qplot(chlorides, data=data, fill = color, binwidth = 0.01) +
scale_x_log10(position='bottom', breaks = seq(0.009, 0.611, 0.1))

ggplot(data) +
  aes(x = "Boxplot for Chlorides", y = chlorides) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#Remving outliers
Q <- quantile(data$chlorides, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(data$chlorides)
data<- subset(data, data$chlorides > 0.4)


#Taking care of the rest of the outliers
Q <- quantile(data$sulphates, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(data$sulphates)
data<- subset(data, data$sulphates > (Q[1] - 1.5*iqr) & data$sulphates < (Q[2]+5*iqr))
data<- subset(data, data$citric.acid<1.2)
data<- subset(data, data$total.sulfur.dioxide<350)

#Now lets take a look at correlation of every pair 
ggpairs(data,columns = 1:11, aes(color = color,  alpha = 0.5)) 
data$color[data$color == 'red']<-0
data$color[data$color == 'white']<-1
data$color <- as.numeric(data$color)  #create a dummy var for color to use it in correlation matrix
cor(data, method = c("pearson", "kendall", "spearman"))
#the correlations look pretty small/average. there is only one 0.72 which is cor(free SO2 and total SF2) that is to be expected
#quality has the strongest correlation with alcohol (Wine tasters love alcohol don't they!)
#the more volatile.acidity and chlorides, the (slightly) less score. I think it's the taste and acid reflux(?!)

#Splitting the data to train and test
trainset <- createDataPartition(data$quality, p = .8, list = FALSE)
training <- data[ trainset,]
testing  <- data[-trainset,]
trainingX <- training[, 1:12]
trainingY <- training$quality

#Training the model with all features [1:12]
model_plsRglm1 <- train(trainingX,trainingY, preProcess = c("center", "scale"), method='plsRglm')
model_plsRglm1

predictions <- predict(model_plsRglm1, newdata=testing[,1:12])
postResample(pred = predictions, obs = testing[, "quality"])
##     RMSE  Rsquared       MAE 
##0.7265449 0.2870778 0.5658155 
## min(predictions)=4.366935 and max(predictions)=7.044547

#Lets round it to see what happens
predictions2<-round(predictions)
postResample(pred = predictions2, obs = testing[, "quality"])
##RMSE  Rsquared       MAE 
##0.8025383 0.2682786 0.5238829 
## YAS! Rounding did work... making sense, right?


#Training the model with all features except for "color"
trainingX <- training[, 1:11]
model_plsRglm2 <- train(trainingX,trainingY, preProcess = c("center", "scale"), method='plsRglm')
model_plsRglm2

predictions <- predict(model_plsRglm2, newdata=testing[,1:11])
postResample(pred = predictions, obs = testing[, "quality"])
##     RMSE  Rsquared       MAE 
##0.7204946 0.3027827 0.5593082 

#Training the model for only "white" dataset
trainset <- createDataPartition(white$quality, p = .8, list = FALSE)
training <- white[ trainset,]
testing  <- white[-trainset,]
trainingX <- training[, 1:11]
trainingY <- training$quality
model_plsRglm3 <- train(trainingX,trainingY, preProcess = c("center", "scale"), method='plsRglm')
model_plsRglm3

predictions <- predict(model_plsRglm3, newdata=testing[,1:11])
postResample(pred = predictions, obs = testing[, "quality"])
##     RMSE  Rsquared       MAE 
##0.7621956 0.2545499 0.6016228 

predictions2<-round(predictions)
postResample(pred = predictions2, obs = testing[, "quality"])

##WE see improvement  after roundng here as well
##     RMSE  Rsquared       MAE 
##0.8272677 0.1569920 0.5577120 


#Training the model for only "red" dataset
trainset <- createDataPartition(red$quality, p = .8, list = FALSE)
training <- red[ trainset,]
testing  <- red[-trainset,]
trainingX <- training[, 1:11]
trainingY <- training$quality
model_plsRglm4 <- train(trainingX,trainingY, preProcess = c("center", "scale"), method='plsRglm')
model_plsRglm4

predictions <- predict(model_plsRglm4, newdata=testing[,1:11])
postResample(pred = predictions, obs = testing[, "quality"])
##RMSE  Rsquared       MAE 
###0.6381099 0.3757243 0.4867897 
##min(predictions)=4.421314 max(predictions)=6.897961

predictions2<-round(predictions)
postResample(pred = predictions2, obs = testing[, "quality"])
##     RMSE  Rsquared       MAE 
##0.6981557 0.2654518 0.4433962 

#Now let us see if it makes any difference to model with high importance variables
trainset <- createDataPartition(data$quality, p = .8, list = FALSE)
training <- data[ trainset,]
testing  <- data[-trainset,]
trainingX <- training[, c(2,8,11)]
trainingY <- training$quality

model_plsRglm5 <- train(trainingX,trainingY, preProcess = c("center", "scale"), method='plsRglm')
model_plsRglm5

predictions <- predict(model_plsRglm5, newdata=testing[,c(2,8,11)])
postResample(pred = predictions, obs = testing[, "quality"])

## RMSE  Rsquared       MAE 
##0.7551364 0.2607902 0.5914055 

#I do not think this is helping

#Lets try PCA (I randomly decided to do it only on RED )
#####################  (ONLY FOR white) WITH PCA
trainset <- createDataPartition(white$quality, p = .8, list = FALSE)
training <- white[ trainset,]
testing  <- white[-trainset,]
trainingX <- training[, 1:11]
trainingY <- training$quality
model_plsRglm6 <- train(trainingX,trainingY, preProcess = c("center", "scale","pca"), method='plsRglm')
model_plsRglm6

predictions <- predict(model_plsRglm6, newdata=testing[,1:11])
postResample(pred = predictions, obs = testing[, "quality"])
#     RMSE  Rsquared       MAE 
#0.8475771 0.2184788 0.6429283 

#MAE increased but Rsquared decreased. RMSE slightly increased.


#Here we start doing the grid search on plsRglm function on all data
#Splitting the data to train and test
trainset <- createDataPartition(data$quality, p = .8, list = FALSE)
training <- data[ trainset,]
testing  <- data[-trainset,]
trainingX <- training[, 1:12]
trainingY <- training$quality
MAE_vals<- matrix(0,3,12)
alphaa<-c(0.01,0.1,1)
n_comp<-c(1:12)
for (i in 1:3){
  for (j in 1:12){
  
    model1 <- plsRglm(trainingY, trainingX, nt = n_comp[j], alpha.pvals.expli=alphaa[i], modele= "pls-glm-gaussian")
    predictions <- predict(model1, newdata=testing[,1:12])
      MAE_vals[i, j] <- mean(abs(round(predictions) - testing[, "quality"]))
      
  }
}
MAE_vals
# [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]      [,8]      [,9]     [,10]     [,11]
#[1,] 0.5640432 0.5316358 0.5354938 0.5277778 0.5185185 0.5146605 0.5154321 0.5200617 0.5231481 0.5216049 0.5216049
#[2,] 0.5640432 0.5316358 0.5354938 0.5277778 0.5185185 0.5146605 0.5154321 0.5200617 0.5231481 0.5216049 0.5216049

#in order to know which Alpha and n_comp leads to minimum of MAE:
which(MAE_vals == min(MAE_vals), arr.ind = TRUE)


#Just a comparison amongst different modeles of plsRglm                         
model_gaussian <- plsRglm(trainingY, trainingX, nt = 12, alpha.pvals.expli=3, modele= "pls-glm-gaussian")
model_Gamma <- plsRglm(trainingY, trainingX, nt = 12, alpha.pvals.expli=0.1, modele= "pls-glm-Gamma")
model_poisson <- plsRglm(trainingY, trainingX, nt = 12, alpha.pvals.expli=0.1, modele= "pls-glm-poisson")
model_inversG <- plsRglm(trainingY, trainingX, nt = 12, alpha.pvals.expli=0.1, modele= "pls-glm-inverse.gaussian")



##Running the grid for each color seprately: white:
trainset <- createDataPartition(white$quality, p = .8, list = FALSE)
training <- white[ trainset,]
testing  <- white[-trainset,]
trainingX <- training[, 1:11]
trainingY <- training$quality
MAE_vals_white<- matrix(0,3,11)
alphaa<-c(0.01,0.1,1)
n_comp<-c(1:11)
for (i in 1:3){
  for (j in 1:11){
    
    model1 <- plsRglm(trainingY, trainingX, nt = n_comp[j], alpha.pvals.expli=alphaa[i], modele=  "pls-glm-gaussian")
    predictions <- predict(model1, newdata=testing[,1:11])
    MAE_vals_white[i, j] <- mean(abs(round(predictions) - testing[, "quality"]))
    
  }
}
MAE_vals_white
##[,1]      [,2]      [,3]      [,4]     [,5]      [,6]      [,7]     [,8]      [,9]     [,10]     [,11]
##[1,] 0.6132993 0.5832482 0.5730337 0.5709908 0.566905 0.5587334 0.5617978 0.566905 0.5628192 0.5628192 0.5628192


##Running the grid for both colors seprately: red:
trainset <- createDataPartition(red$quality, p = .8, list = FALSE)
training <- red[ trainset,]
testing  <- red[-trainset,]
trainingX <- training[, 1:11]
trainingY <- training$quality
MAE_vals_red<- matrix(0,3,11)
alphaa<-c(0.01,0.1,1)
n_comp<-c(1:11)
for (i in 1:3){
  for (j in 1:11){
    
    model1 <- plsRglm(trainingY, trainingX, nt = n_comp[j], alpha.pvals.expli=alphaa[i], modele=  "pls-glm-gaussian")
    predictions <- predict(model1, newdata=testing[,1:11])
    MAE_vals_red[i, j] <- mean(abs(round(predictions) - testing[, "quality"]))
    
  }
}
MAE_vals_red
##Red shows a great progress in MAE:
#[,1]      [,2]      [,3]     [,4]      [,5]      [,6]     [,7]      [,8]      [,9]     [,10]     [,11]
#[1,] 0.4339623 0.4056604 0.4308176 0.408805 0.4119497 0.4119497 0.408805 0.4025157 0.4025157 0.4025157 0.4025157
#[2,] 0.4339623 0.4056604 0.4308176 0.408805 0.4119497 0.4119497 0.408805 0.4025157 0.4025157 0.4025157 0.4025157

#Red dataset does not have any "9" score.not a huge percentage of "8" or "3" either. In general its histogram is much more dense. That is why it has less error. 
