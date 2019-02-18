#------------Implemented Logistic Regression using Batch Gradient Descent on Auto Dataset----------

require("ISLR")


data=Auto
logistic_data_matrix<-matrix(data.matrix(data),nrow=nrow(data),ncol=ncol(data))
colnames(logistic_data_matrix)<-colnames(data)
logistic_data_matrix
median_mpg<-median(logistic_data_matrix[,1])
logistic_data_matrix<-cbind(c(0),logistic_data_matrix)
logistic_data_matrix[,1]<-ifelse(logistic_data_matrix[,c("mpg")]>median_mpg,1,0)
colnames(logistic_data_matrix)[1]<-"mpg01"
logistic_data_matrix


new.Auto<-logistic_data_matrix[,c("mpg01","cylinders","displacement","horsepower","weight")]
new.Auto

logistic_target<-new.Auto[,1]
logistic_target

logistic_train_data<-new.Auto[,2:5]
logistic_train_data<-scale(logistic_train_data)
logistic_train_data<-cbind(c(1),logistic_train_data)
logistic_train_data

l_alpha<-0.01
l_num_iters<-700000

ltheta<-matrix(c(0),nrow=1,ncol=ncol(logistic_train_data))
prev_ltheta<-matrix(,nrow=l_num_iters,ncol=ncol(logistic_train_data))
l_cost<-matrix(,nrow=l_num_iters,ncol=1)

l_predictvals<-function(ltheta,logistic_train_data)
{
  t_logistic_train_data<-t(logistic_train_data)
  intermediate_val<-ltheta %*% t_logistic_train_data
  y<-(1/(1+exp(-intermediate_val)))
  return(y)
  
}

l_calcost<-function(l_y,logistic_target)
{
  m=(-1/length(l_y))
  cost<-(m*sum((logistic_target*log(l_y+(1-logistic_target))*log(1-l_y))))
  return(cost)
  
}

l_update_theta<-function(l_y,ltheta,logistic_train_data,logistic_target,l_alpha)
{
  
  ltheta[,1]=ltheta[,1]-l_alpha*((1/(2*nrow(logistic_train_data))))*(sum((l_y-logistic_target)))
  for(col in 2:ncol(logistic_train_data))
  {
    ltheta[,col]=ltheta[,col]-l_alpha*((1/(2*nrow(logistic_train_data))))*(sum((l_y-logistic_target)*logistic_train_data[,col]))
  }
  return(ltheta)
}


for(i in 1:l_num_iters)
{
  l_y<-l_predictvals(ltheta,logistic_train_data)
  l_cost[i,]<-l_calcost(l_y,logistic_target)
  prev_ltheta[i,]<-ltheta
  ltheta<-l_update_theta(l_y,ltheta,logistic_train_data,logistic_target,l_alpha)
}
print(ltheta)

#sigmoid function for different inputs
input<-seq(-20,20,0.05)
input
gz<-1/(1+exp(-input))
plot(input,gz,col="red")


#Testing in data
new_logistic_train_data<-new.Auto[,2:5]

new_logistic_train_data<-rbind(new_logistic_train_data,c(8,340,200,3500))
new_logistic_train_data
new_logistic_train_data<-scale(new_logistic_train_data)
logistic_test_data<-new_logistic_train_data[nrow(new_logistic_train_data),]
logistic_test_data<-as.matrix(logistic_test_data)

logistic_test_data
#logistic_test_data<-cbind(c(1),logistic_test_data)
logistic_test_data<-rbind(1,logistic_test_data)
#logistic_test_data<-t(logistic_test_data)
l_intermediate_val<-ltheta %*% logistic_test_data
l_final_y<-(1/(1+exp(-l_intermediate_val)))
l_final_y


#Test different learning rates
l_alpha1<-3
l_alpha2<-0.3
l_alpha3<-0.03
l_alpha4<-0.00003
new_num_iter<-100

# for alpha1
ltheta<-matrix(c(0),nrow=1,ncol=ncol(logistic_train_data))
prev_ltheta<-matrix(,nrow=l_num_iters,ncol=ncol(logistic_train_data))
new_l_cost<-matrix(,nrow=new_num_iter,ncol=1)
for( j in 1:new_num_iter)
{
  l_y<-l_predictvals(ltheta,logistic_train_data)
  new_l_cost[j,]<-l_calcost(l_y,logistic_target)
  prev_ltheta[j,]<-ltheta
  ltheta<-l_update_theta(l_y,ltheta,logistic_train_data,logistic_target,l_alpha1)
}
plot(new_l_cost,type="line",xlab="No of iters",ylab="Cost",col="green")


# for alpha2
ltheta<-matrix(c(0),nrow=1,ncol=ncol(logistic_train_data))
prev_ltheta<-matrix(,nrow=l_num_iters,ncol=ncol(logistic_train_data))
new_l_cost<-matrix(,nrow=new_num_iter,ncol=1)

for(j in 1:new_num_iter)
{
  l_y<-l_predictvals(ltheta,logistic_train_data)
  new_l_cost[j,]<-l_calcost(l_y,logistic_target)
  prev_ltheta[j,]<-ltheta
  ltheta<-l_update_theta(l_y,ltheta,logistic_train_data,logistic_target,l_alpha2)
}
plot(new_l_cost[,1],type="line",xlab="No of iters",ylab="Cost",col="blue")

# for alpha3
ltheta<-matrix(c(0),nrow=1,ncol=ncol(logistic_train_data))
prev_ltheta<-matrix(,nrow=l_num_iters,ncol=ncol(logistic_train_data))
new_l_cost<-matrix(,nrow=new_num_iter,ncol=1)

for(j in 1:new_num_iter)
{
  l_y<-l_predictvals(ltheta,logistic_train_data)
  new_l_cost[j,]<-l_calcost(l_y,logistic_target)
  prev_ltheta[j,]<-ltheta
  ltheta<-l_update_theta(l_y,ltheta,logistic_train_data,logistic_target,l_alpha3)
}
plot(new_l_cost,type="line",xlab="No of iters",ylab="Cost",col="red")

#for alpha4
ltheta<-matrix(c(0),nrow=1,ncol=ncol(logistic_train_data))
prev_ltheta<-matrix(,nrow=l_num_iters,ncol=ncol(logistic_train_data))
new_l_cost<-matrix(,nrow=new_num_iter,ncol=1)
for(j in 1:new_num_iter)
{
  l_y<-l_predictvals(ltheta,logistic_train_data)
  new_l_cost[j,]<-l_calcost(l_y,logistic_target)
  prev_ltheta[j,]<-ltheta
  ltheta<-l_update_theta(l_y,ltheta,logistic_train_data,logistic_target,0.00003)
}
plot(new_l_cost,type="line",xlab="No of iters",ylab="Cost",col="orange")

