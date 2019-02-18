#Univariate Regression Using Batch Gradient Descent

install.packages("plotly")
library(plotly)
require("ISLR")
data=Auto
data
auto_data_matrix<-matrix(data.matrix(data),nrow=nrow(data),ncol=ncol(data))
colnames(auto_data_matrix)=colnames(data)
auto_data_matrix

auto_data_matrix<-auto_data_matrix[,c("horsepower","mpg")]
auto_data_matrix

#---target vector
target<-auto_data_matrix[,c("mpg")]
target

train_data_matrix=auto_data_matrix[,"horsepower"]
train_data_matrix


train_data_matrix<-scale(train_data_matrix)
train_data_matrix<-cbind(c(1),train_data_matrix)
train_data_matrix

alpha=0.0001
num_iters=70000

prev_theta=matrix(,nrow = num_iters,ncol=2) #--Neeed to change
cost=matrix(,nrow=num_iters,ncol=1)
theta<-matrix(c(0),nrow=1,ncol=2)
#theta<-c(0,0)
plot(train_data_matrix[,2],target,main="Training Data Set",xlab="Horsepower",ylab="mpg")

predictvals<-function(train_data_matrix,theta)
{
  s_train_data<-t(train_data_matrix)
  #y<-(theta[,1]*s_train_data[1,])+(theta[,2]*s_train_data[2,])
  y=theta %*% s_train_data
  return(y)
}

update_theta<-function(y,train_data_matrix,theta,target,alpha)
{
  theta[,1]=theta[,1]-alpha*((1/(2*length(y)))*sum(y-target))
  theta[,2]=theta[,2]-alpha*((1/(2*length(y)))*sum((y-target)*train_data_matrix[,2]))
  return(theta)
}
calcost<-function(theta,y,train_data_matrix,target)
{
  cost=(1/(2*length(y)))*sum((y-target)^2)
  return(cost)
}


for(i in 1:num_iters)
{
  y<-predictvals(train_data_matrix,theta)
  cost[i,]<-calcost(theta,y,train_data_matrix,target)
  prev_theta[i,]<-theta
  theta<-update_theta(y,train_data_matrix,theta,target,alpha)
}
print(theta)
abline(coef=theta,col="red")


#predict for a value
hp<-220
new_train_data<-auto_data_matrix[,"horsepower"]
new_train_data
new_train_data<-c(new_train_data,220)
new_train_data<-scale(new_train_data)
new_train_data<-as.matrix(new_train_data)
new_train_data<-cbind(c(1),new_train_data)
hp1<-as.matrix(new_train_data[nrow(new_train_data),])
hp1
predicted_y<-(theta) %*% (hp1)
(predicted_y)

#normal equation
n_theta<-solve(t(train_data_matrix)%*%train_data_matrix)%*%(t(train_data_matrix)%*%target)
n_theta

#contour plot
p<-plot_ly(type="contour",z=prev_theta,contours=list(coloring="heatmap"))
#chart_link=api_create(p,filename="basic")
#chart_link
p

