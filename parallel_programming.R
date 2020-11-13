# Title: Parallel Programming
# Author: Anna Zink
# Date: 11/12/2020
# Description: Sample code for parallel programming
# References:
#   https://bookdown.org/rdpeng/rprogdatascience/parallel-computation.html
#   http://adv-r.had.co.nz/Profiling.html

# lineprof not on cran must download directly
#devtools::install_github("hadley/lineprof")

library(lineprof)
library(microbenchmark)
library(doParallel)
library(parallel) 
library(foreach) 

##################################
##        PROFILING             ##
##################################

# PROFILING - finding bottlemarks in time/memory 
source('sample_code.R')

# run analysis
run_analysis<-function() {
  ols()
  predlist<-xval_ols()
  boot(predlist)
}

# look at 
l<-lineprof(run_analysis())
l
# output interpretation: 
# time - time to run (seconds)
# alloc - memory allocation (megabytes)
# release - memory released (megabytes)
# dups - number of vector duplications that occured 

# open with shiny
# shine(l)

##################################
##    foreach &  doParallel     ##
##################################

# foreach & doParallel- way to parallelize loops in R
# doParallel provides parallel backend for each %dopar% function 
# in the foreach statement

# update xval function in parallel using foreach and %dopar%
xval_ols_updt<-function() {
  
  # you must register the cores you will use 
  cl<-makePSOCKcluster(4)
  registerDoParallel(cl)
  
  nfolds<-5
  data$folds<-cut(seq(1,nrow(data)),breaks=nfolds,labels=FALSE)
  
  # run OLS for each fold 
  results<-foreach(i=1:nfolds, .combine=rbind) %dopar% {
    # split into train/test
    index<-which(data$folds==i)
    test_i<-data[index,]
    train_i<-data[-index,]
    
    # fit on train and predict on test
    ra_i<-lm(totpay~., data=train_i)
    fit_i<-predict(ra_i, data=test_i)
    
    # add predicted data for the fold to the final dataset
    fit_i<-data.frame(pred=fit_i, fold=i)
    
    fit_i
  }
  return(results)
  
  stopCluster(cl)
}



# benchmark update using package microbenchmark
a<-xval_ols()
b<-xval_ols_updt()
speed_test_xval<-microbenchmark(a, b)
speed_test_xval

# plot results 
#ggplot2::autoplot(speed_test_xval) 

##################################
##    the parallel package      ##
##################################

# ~~~ ONLY AVAILABLE ON MACS ~~~ 
# mclapply and mcmapply are parallel versions of 
# lapply and mapply respectively 

# bootstrap using mclapply 
boot_updt<-function(predlist) {
  mean.boot.updt<-mclapply(1:500, function(i) {
    xnew<-sample(predlist$pred, replace = TRUE)
    mean(xnew)
  }, mc.cores = 4)
  

  quantile(unlist(mean.boot.updt), c(.025,.975), na.rm=TRUE)
}

# compare update with benchmark 
output<-xval_ols_updt()

a<-boot(output)
b<-boot_updt(output)
speed_test_boot<-microbenchmark(a, b)
speed_test_boot

# let's try our whole program now with updated functions
run_analysis_updt<-function() {
  ols()
  predlist<-xval_ols_updt()
  boot_updt(predlist)
}

a<-run_analysis()
b<-run_analysis_updt()
speed_test_all<-microbenchmark(a, b)
speed_test_all

ggplot2::autoplot(speed_test_all) 

# ~~~ NO MAC? ~~~ 
### Try using parLapply and other similar functions listed here:
?clusterApply

# On Windows you need to set up your environment (e.g. register the
# cluster, load packages on new clusters, etc.). McLapply clones
# all the work processes when it's called so you don't need to set 
# up a new session removing this step for the user. 

#############################################
##    generating random #s in parallel     ##
#############################################

# We want random #s in parallel to be DIFFERENT from each other 
# but also reproducible... How do we ensure every time we run our
# parallel process we get the same different numbers for each process? 

# must set this random # generator! 
RNGkind("L'Ecuyer-CMRG")
set.seed(1248)

gen_randonums<-function() {
  nums<-mclapply(1:4, function(i) {
  rnorm(3)
}, mc.cores = 4)
  return(nums)
}

gen_randonums()
