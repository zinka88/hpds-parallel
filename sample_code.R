# Title: Sample code
# Author: Anna Zink
# Date: 11/12/2020
# Description: I have data on annual health care spending and I want to 
# predict spending using demographics and heatlh condition groups 

data<-read.csv('simulated_analysis_data.csv')

# run simple OLS
ols<-function() {
  ra1<-lm(totpay~., data=data)
}

# run OLS using 5-fold cross validation
xval_ols<-function() {
  
  nfolds<-4
  data$folds<-cut(seq(1,nrow(data)),breaks=nfolds,labels=FALSE)
  
  # create list of predicted values 
  predicted.y<-data.frame(pred=numeric(), fold=numeric()) 
  
  # run OLS for each fold 
  for (i in 1:nfolds){
    # split into train/test
    index<-which(data$folds==i)
    test_i<-data[index,]
    train_i<-data[-index,]
    
    # fit on train and predict on test
    ra_i<-lm(totpay~., data=train_i)
    fit_i<-predict(ra_i, data=test_i)
    
    # add predicted data for the fold to the final dataset
    fit_i<-data.frame(pred=fit_i, fold=i)
    predicted.y<-rbind(predicted.y,fit_i)
  }
  return(predicted.y)
  
}

# bootstrapped mean standard errors of predictions  
boot<-function(predlist) {
  mean.boot<-replicate(500, {
    xnew <- sample(predlist$pred, replace = TRUE)
    mean(xnew)
  })
  print("bootstrapped mean spending")
  mean(mean.boot)
  quantile(mean.boot, c(.025,.975))
}
