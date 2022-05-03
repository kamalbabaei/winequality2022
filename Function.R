AFS <- function(train, test)  {
  
  mu=mean(train$quality)
  sd=sd(train$quality)
  Redge<-mu+2*sd
  Ledge<-mu-3*sd
  train<-subset(train, train$quality>=Ledge & train$quality<=Redge)
  train_red<- subset(train, train$color=='red')
  train_white<- subset(train, train$color=='white')
  test_red<- subset(test, test$color=='red')
  test_white <- subset(test, test$color=='white')
  
  #run for red  
  MAE_vals_red<- matrix(0,3,11)
  alphaa<-c(0.01,0.1,1)
  n_comp<-c(1:11)
  for (i in 1:3){
    for (j in 1:11){
      
      model1 <- plsRglm(train_red[,13], train_red[,1:11], nt = n_comp[j], alpha.pvals.expli=alphaa[i], modele= "pls-glm-gaussian")
      predictions <- predict(model1, newdata=test_red[,1:11])
      MAE_vals_red[i, j] <- mean(abs(round(predictions) - test_red[, 13]))
      
    }
  }
  #run for white
  MAE_vals_white<- matrix(0,3,11)
  alphaa<-c(0.01,0.1,1)
  n_comp<-c(1:11)
  for (i in 1:3){
    for (j in 1:11){
      
      model2 <- plsRglm(train_white[,13], train_white[,1:11], nt = n_comp[j], alpha.pvals.expli=alphaa[i], modele= "pls-glm-gaussian")
      predictions <- predict(model2, newdata=test_white[,1:11])
      MAE_vals_white[i, j] <- mean(abs(round(predictions) - test_white[, 13]))
      
    }
  }
  #Ensemble the two paths: weighted mean
  return((nrow(red)*min(MAE_vals_red)+nrow(white)*min(MAE_vals_white))/nrow(data))
  
}


