#Arindam Ghosh;GRIPMAY21
#======import data=====#
url<-"http://bit.ly/w-data"
data<-read.csv(url)
data

#======Prepare the data=====#
y<-data[,2]
y
x<-data[,1]
x

dt = sort(sample(nrow(data)*.8))
train<-data[dt,]
test<-data[-dt,]

y_train<-train[,2]
y_train
x_train<-train[,1]
x_train

y_test<-test[,2]
y_test
x_test<-test[,1]
x_test

#======Graphical representation======#
plot(x,y)

#======Algorithm========#
linearregresion<-function(X,Y){
  n<-dim(X)
  coef.reg<-cov(X,Y)/var(X)
  inte.reg<-mean(Y)-coef.reg*mean(X)
  beta<-c(coefficient=coef.reg,intercept=inte.reg)
  return(beta)
}

#=======training the Algorithm=====#
beta_data<-linearregresion(x_train,y_train)
beta_data

#=======plotting the regression line====#
plot(x,y)
abline(beta_data[2],beta_data[1])

#=======Compare Actual vs. Predicted=======#
prediction<-function(X){
  predicted<-array(dim=length(X))
  for(i in 1:length(X)){
    predicted[i]=beta_data[2]+beta_data[1]*X[i]
  }
  return(predicted)
}
compare.df=data.frame(Predicted=prediction(x_test),Actual=y_test)
compare.df

#==========Making Prediction=========#
#What will be the predicted score if a student studies for 9.25 hrs/day?
prediction(9.25)

