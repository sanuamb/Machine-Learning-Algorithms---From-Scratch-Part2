# Multivaraite regression Using gradient Descent and Normal equations

require("ISLR")
require(stats)
data=Auto
data
mauto_data_matrix<-matrix(data.matrix(data),nrow=nrow(data),ncol=ncol(data))
colnames(mauto_data_matrix)<-colnames(data)
mauto_data_matrix<-mauto_data_matrix[,-9]
#mauto_data_matrix<-scale(mauto_data_matrix)
mauto_data_matrix


m_target<-mauto_data_matrix[,1]
m_target

mtrain_data_matrix<-matrix(mauto_data_matrix[,c(2:8)],nrow=nrow(mauto_data_matrix),ncol = ncol(mauto_data_matrix)-1)
k<-arrayInd(2:8,dim(mauto_data_matrix))
colnames(mtrain_data_matrix)<-colnames(mauto_data_matrix)[k[,1]]
mtrain_data_matrix<-scale(mtrain_data_matrix)
mtrain_data_matrix<-cbind(c(1),mtrain_data_matrix)
mtrain_data_matrix



m_alpha=0.001
m_num_iters=400000

mtheta<-matrix(c(0),nrow=1,ncol=ncol(mtrain_data_matrix))
prev_mtheta<-matrix(,nrow=m_num_iters,ncol=ncol(mtrain_data_matrix))
mcost=matrix(,nrow=m_num_iters,ncol=ncol(mtrain_data_matrix))
print("Training the model")
for(i in 1:m_num_iters)
{
  m_y=mpredictvals(mtheta,mtrain_data_matrix)
  prev_mtheta[i,]<-mtheta
  mcost[i,]<-mcalcost(m_y,mtrain_data_matrix,m_target)
  mtheta<-mupdate_theta(m_y,mtheta,mtrain_data_matrix,m_target,m_alpha)
}

print(mtheta)
#abline(coef=mtheta,col="red")


mpredictvals<-function(mtheta,mtrain_data_matrix)
{
  tm_train_data<-t(mtrain_data_matrix)
  m_y<- mtheta %*% tm_train_data
  return(m_y)
}

mcalcost<-function(m_y,mtrain_data_matrix,m_target)
{
  mcost<-(1/(2*nrow(mtrain_data_matrix)))*sum((m_y-m_target)**2)
  return(mcost)
}

mupdate_theta<-function(m_y,mtheta,mtrain_data_matrix,m_target,m_alpha)
{
  mtheta[,1]=mtheta[,1]-m_alpha*((1/(2*nrow(mtrain_data_matrix))))*(sum((m_y-m_target)))
  for(col in 2:ncol(mtrain_data_matrix))
  {
    mtheta[,col]=mtheta[,col]-m_alpha*((1/(2*nrow(mtrain_data_matrix))))*(sum((m_y-m_target)*mtrain_data_matrix[,col]))
  }
  return(mtheta)
}


#normal equation

n_mtheta<-solve(t(mtrain_data_matrix)%*%mtrain_data_matrix)%*%(t(mtrain_data_matrix)%*%m_target)
n_mtheta


#Predicting for test data
new_mtrain_data_matrix<-matrix(mauto_data_matrix[,c(2:8)],nrow=nrow(mauto_data_matrix),ncol = ncol(mauto_data_matrix)-1)
k<-arrayInd(2:8,dim(mauto_data_matrix))
colnames(new_mtrain_data_matrix)<-colnames(mauto_data_matrix)[k[,1]]
new_mtrain_data_matrix<-rbind(new_mtrain_data_matrix,c(4,300,200,3500,11,70,2))
new_mtrain_data_matrix
new_mtrain_data_matrix<-scale(new_mtrain_data_matrix)
mtest_data<-new_mtrain_data_matrix[nrow(new_mtrain_data_matrix),]


mtest_data<-c(1,mtest_data)
mtest_data<-as.matrix(mtest_data)
mtest_data

final_mpg<-mtheta %*% (mtest_data)
final_mpg


#For different learning rates
m_alpha1<-3
m_alpha2<-0.3
m_alpha3<-0.03
m_alpha4<-0.00003

m_new_iters<-100

#for alpha1
mtheta<-matrix(c(0),nrow=1,ncol=ncol(mtrain_data_matrix))
prev_mtheta<-matrix(,nrow=m_num_iters,ncol=ncol(mtrain_data_matrix))
new_mcost=matrix(,nrow=m_new_iters,ncol=1)

for(j in 1:m_new_iters)
{
  m_y=mpredictvals(mtheta,mtrain_data_matrix)
  prev_mtheta[j,]<-mtheta
  new_mcost[j,]<-mcalcost(m_y,mtrain_data_matrix,m_target)
  mtheta<-mupdate_theta(m_y,mtheta,mtrain_data_matrix,m_target,m_alpha1)
}
plot(new_mcost,type="line",xlab="No of iters",ylab="Cost",col="green")


#for alpha2
mtheta<-matrix(c(0),nrow=1,ncol=ncol(mtrain_data_matrix))
prev_mtheta<-matrix(,nrow=m_num_iters,ncol=ncol(mtrain_data_matrix))
new_mcost=matrix(,nrow=m_new_iters,ncol=1)

for(j in 1:m_new_iters)
{
  m_y=mpredictvals(mtheta,mtrain_data_matrix)
  prev_mtheta[j,]<-mtheta
  new_mcost[j,]<-mcalcost(m_y,mtrain_data_matrix,m_target)
  mtheta<-mupdate_theta(m_y,mtheta,mtrain_data_matrix,m_target,m_alpha2)
}
plot(new_mcost,type="line",xlab="No of iters",ylab="Cost",col="red")

#for alpha3
mtheta<-matrix(c(0),nrow=1,ncol=ncol(mtrain_data_matrix))
prev_mtheta<-matrix(,nrow=m_num_iters,ncol=ncol(mtrain_data_matrix))
new_mcost=matrix(,nrow=m_new_iters,ncol=1)

for(j in 1:m_new_iters)
{
  m_y=mpredictvals(mtheta,mtrain_data_matrix)
  prev_mtheta[j,]<-mtheta
  new_mcost[j,]<-mcalcost(m_y,mtrain_data_matrix,m_target)
  mtheta<-mupdate_theta(m_y,mtheta,mtrain_data_matrix,m_target,m_alpha3)
}
plot(new_mcost,type="line",xlab="No of iters",ylab="Cost",col="blue")

#for alpha4

mtheta<-matrix(c(0),nrow=1,ncol=ncol(mtrain_data_matrix))
prev_mtheta<-matrix(,nrow=m_num_iters,ncol=ncol(mtrain_data_matrix))
new_mcost=matrix(,nrow=m_new_iters,ncol=1)

for(j in 1:m_new_iters)
{
  m_y=mpredictvals(mtheta,mtrain_data_matrix)
  prev_mtheta[j,]<-mtheta
  new_mcost[j,]<-mcalcost(m_y,mtrain_data_matrix,m_target)
  mtheta<-mupdate_theta(m_y,mtheta,mtrain_data_matrix,m_target,m_alpha4)
}
plot(new_mcost,type="line",xlab="No of iters",ylab="Cost",col="brown")




