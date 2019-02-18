#------------knn on Car evaluation using euclidean distance----------------
require(dummies)
require(caret)
require(e1071)
require(Matrix)


#kfold=1
#knn_k=1
cknnerr<-c(0)
cerror_everyk<-matrix(,nrow=0,ncol=5)
coptimal_k<-matrix(c(1,3,5,7,9),nrow=5,ncol=1,byrow=TRUE)
copt<-c(0)
colnames(cerror_everyk)<-c("1","2","3","4","5")
for(knn_k in c(1,3,5,7,9))
{
  for(kfold in seq(1:5))
  {
    
    filename1=paste("careval_train",kfold,sep="_")
    filename2=paste("careval_test",kfold,sep = "_")
    ext=".csv"
    filename1=paste(filename1,ext,sep="")
    filename2=paste(filename2,ext,sep="")
    #print(filename1)
    #print(filename2)
    
    kcar_train<-read.csv(filename1,header = TRUE)
    #View(kcar_train)
    
    kcar_test<-read.csv(filename2,header=TRUE)
    #View(kcar_test)
    
    kcar_entireset<-read.table("car.data.txt",header=FALSE,sep=",")
    
    kcar_train$cgrp_data=NULL
    kcar_test$cgrp_data<-NULL
    
    
    #Ordering the levels of variables of the train datatset
    kcar_train$V1<-factor(kcar_train$V1,levels = c("low","med","high","vhigh"))
    
    kcar_train$V2<-factor(kcar_train$V2,levels=c("low","med","high","vhigh"))
    
    kcar_train$V3<-factor(kcar_train$V3,levels<-c("2","3","4","5more"))
    
    kcar_train$V4<-factor(kcar_train$V4,levels<-c("2","4","more"))
    
    kcar_train$V5<-factor(kcar_train$V5,levels<-c("small","med","big"))
    
    kcar_train$V6<-factor(kcar_train$V6,levels<-c("low","med","high"))
    
    kcar_train$V7<-factor(kcar_train$V7,levels<-levels(kcar_entireset$V7))
    
    
    #Ordering levels of the variables of the test dataset
    kcar_test$V1<-factor(kcar_test$V1,levels = c("low","med","high","vhigh"))
    
    kcar_test$V2<-factor(kcar_test$V2,levels=c("low","med","high","vhigh"))
    
    kcar_test$V3<-factor(kcar_test$V3,levels<-c("2","3","4","5more"))
    
    kcar_test$V4<-factor(kcar_test$V4,levels<-c("2","4","more"))
    
    kcar_test$V5<-factor(kcar_test$V5,levels<-c("small","med","big"))
    
    kcar_test$V6<-factor(kcar_test$V6,levels<-c("low","med","high"))
    
    kcar_test$V7<-factor(kcar_test$V7,levels<-levels(kcar_entireset$V7))
    
    
    #Getting the target of the train dataset
    
    kcar_target<-kcar_train$V7
    kcar_train$V7<-NULL
    
    #Getting the target of the test dataset
    
    kcar_act_target<-kcar_test$V7
    kcar_test$V7<-NULL
    
    
    #Performing knn using the euclidean distance
    car_knn_pred<-function(kcar_train,kcar_test_encode,kcar_target,k)
    {
      cknnpred<-c(0)
      for(j in 1:nrow(kcar_test))
      {
        dis<-apply(kcar_train,1,function(x) (sum((unname(unlist(x))-unname(unlist(kcar_test[j,])))**2)))
        #print(dis)
        
        #select the subset of points from train close to the given row of test
        t<-order(dis)[1:k]
        #print(t)
        
        #Count the frequencies of labels for those selected subset
        count<-table(kcar_target[t])
        max_count_label<-names(count)[count==max(count)]
        #print(max_count_label)
        
        #Incase both the labels have same count
        cknnpred[j]<-sample(max_count_label,1)
        
      }
      #print(knnpred)
      #View(dis)
      #View(t)
      return(cknnpred)
    }
    
    pred<-car_knn_pred(kcar_train,kcar_test,kcar_target,knn_k)
    print(pred)
    
    #pred<-factor(pred,levels=levels(kcar_act_target))
    
    
    #Confusion Matrix
    car_knn_conf<-confusionMatrix(factor(pred),(kcar_act_target))
    print(car_knn_conf)
    
    
    
    #Calculating the error rate
    car_tab1<-as.data.frame(car_knn_conf$table)
    #print(car_tab1)
    
    
    
    #car_err<-car_tab1[2,"Freq"]+iono_tab1[3,"Freq"]
    car_err<-car_tab1[2,"Freq"]+car_tab1[3,"Freq"]+car_tab1[4,"Freq"]+car_tab1[5,"Freq"]+car_tab1[7,"Freq"]+car_tab1[8,"Freq"]+
      car_tab1[9,"Freq"]+car_tab1[10,"Freq"]+car_tab1[12,"Freq"]+car_tab1[13,"Freq"]+car_tab1[14,"Freq"]+car_tab1[15,"Freq"]
    
    t_car<-sum(car_tab1[,"Freq"])
    cknnerr[kfold]<-(car_err/t_car)
    
    
  }
  
  #print(cknnerr)
  title<-paste("Car_Eval knn for k=",knn_k)
  plot(seq(1:5),cknnerr,type="o",xlab="No_of_Folds",ylab="Error_rate",main=title)
  avg_error<-mean(cknnerr)
  
  copt[knn_k]<-avg_error
  cerror_everyk<-rbind(cerror_everyk,cknnerr)
  
}

rownames(cerror_everyk)<-c("1","3","5","7","9")
print(cerror_everyk)
coptimal_k<-cbind(coptimal_k,(rowMeans(error_everyk)))
print(coptimal_k)
coptimal_k_val<-min(coptimal_k[,2])
print(coptimal_k_val)

#-----------------------------Knn on Car eval Dataset using manhattan Distance--------------
cknnerr<-c(0)
cerror_everyk<-matrix(,nrow=0,ncol=5)
coptimal_k<-matrix(c(1,3,5,7,9),nrow=5,ncol=1,byrow=TRUE)
copt<-c(0)
colnames(cerror_everyk)<-c("1","2","3","4","5")
for(knn_k in c(1,3,5,7,9))
{
  for(kfold in seq(1:5))
  {
    
    filename1=paste("careval_train",kfold,sep="_")
    filename2=paste("careval_test",kfold,sep = "_")
    ext=".csv"
    filename1=paste(filename1,ext,sep="")
    filename2=paste(filename2,ext,sep="")
    #print(filename1)
    #print(filename2)
    
    kcar_train<-read.csv(filename1,header = TRUE)
    #View(kcar_train)
    
    kcar_test<-read.csv(filename2,header=TRUE)
    #View(kcar_test)
    
    kcar_entireset<-read.table("car.data.txt",header=FALSE,sep=",")
    
    kcar_train$cgrp_data=NULL
    kcar_test$cgrp_data<-NULL
    
    
    #Ordering the levels of variables of the train datatset
    kcar_train$V1<-factor(kcar_train$V1,levels = c("low","med","high","vhigh"))
    
    kcar_train$V2<-factor(kcar_train$V2,levels=c("low","med","high","vhigh"))
    
    kcar_train$V3<-factor(kcar_train$V3,levels<-c("2","3","4","5more"))
    
    kcar_train$V4<-factor(kcar_train$V4,levels<-c("2","4","more"))
    
    kcar_train$V5<-factor(kcar_train$V5,levels<-c("small","med","big"))
    
    kcar_train$V6<-factor(kcar_train$V6,levels<-c("low","med","high"))
    
    kcar_train$V7<-factor(kcar_train$V7,levels<-levels(kcar_entireset$V7))
    
    
    #Ordering levels of the variables of the test dataset
    kcar_test$V1<-factor(kcar_test$V1,levels = c("low","med","high","vhigh"))
    
    kcar_test$V2<-factor(kcar_test$V2,levels=c("low","med","high","vhigh"))
    
    kcar_test$V3<-factor(kcar_test$V3,levels<-c("2","3","4","5more"))
    
    kcar_test$V4<-factor(kcar_test$V4,levels<-c("2","4","more"))
    
    kcar_test$V5<-factor(kcar_test$V5,levels<-c("small","med","big"))
    
    kcar_test$V6<-factor(kcar_test$V6,levels<-c("low","med","high"))
    
    kcar_test$V7<-factor(kcar_test$V7,levels<-levels(kcar_entireset$V7))
    
    
    #Getting the target of the train dataset
    
    kcar_target<-kcar_train$V7
    kcar_train$V7<-NULL
    
    #Getting the target of the test dataset
    
    kcar_act_target<-kcar_test$V7
    kcar_test$V7<-NULL
    
    
    #Performing knn using the euclidean distance
    car_knn_pred<-function(kcar_train,kcar_test_encode,kcar_target,k)
    {
      cknnpred<-c(0)
      
      for(j in 1:nrow(kcar_test))
      {
        cdist<-matrix(,nrow=nrow(kcar_train),ncol=1)
        #dis<-apply(kcar_train,1,function(x) (sum((unname(unlist(x))-unname(unlist(kcar_test[j,])))**2)))
        for(h in 1:nrow(kcar_train))
        {
          s<-unname(unlist(kcar_train[h,]))
          v<-unname(unlist(kcar_train[h,]))
          dis<-dist(rbind(s,v),method="manhattan")
          cdist[h,]<-dis
        }
        #print(dis)
        
        #select the subset of points from train close to the given row of test
        t<-order(cdist)[1:k]
        #print(t)
        
        #Count the frequencies of labels for those selected subset
        count<-table(kcar_target[t])
        max_count_label<-names(count)[count==max(count)]
        #print(max_count_label)
        
        #Incase both the labels have same count
        cknnpred[j]<-sample(max_count_label,1)
        
      }
      #print(knnpred)
      #View(dis)
      #View(t)
      return(cknnpred)
    }
    
    pred<-car_knn_pred(kcar_train,kcar_test,kcar_target,knn_k)
    print(pred)
    
    #pred<-factor(pred,levels=levels(kcar_act_target))
    
    
    #Confusion Matrix
    car_knn_conf<-confusionMatrix(factor(pred),(kcar_act_target))
    print(car_knn_conf)
    
    
    
    #Calculating the error rate
    car_tab1<-as.data.frame(car_knn_conf$table)
    #print(car_tab1)
    
    
    
    #car_err<-car_tab1[2,"Freq"]+iono_tab1[3,"Freq"]
    car_err<-car_tab1[2,"Freq"]+car_tab1[3,"Freq"]+car_tab1[4,"Freq"]+car_tab1[5,"Freq"]+car_tab1[7,"Freq"]+car_tab1[8,"Freq"]+
      car_tab1[9,"Freq"]+car_tab1[10,"Freq"]+car_tab1[12,"Freq"]+car_tab1[13,"Freq"]+car_tab1[14,"Freq"]+car_tab1[15,"Freq"]
    
    t_car<-sum(car_tab1[,"Freq"])
    cknnerr[kfold]<-(car_err/t_car)
    
    
  }
  
  #print(cknnerr)
  title<-paste("Car_Eval knn for k=",knn_k)
  plot(seq(1:5),cknnerr,type="o",xlab="No_of_Folds",ylab="Error_rate",main=title)
  avg_error<-mean(cknnerr)
  
  copt[knn_k]<-avg_error
  cerror_everyk<-rbind(cerror_everyk,cknnerr)
  
}

rownames(cerror_everyk)<-c("1","3","5","7","9")
print(cerror_everyk)
coptimal_k<-cbind(coptimal_k,(rowMeans(error_everyk)))
print(coptimal_k)
coptimal_k_val<-min(coptimal_k[,2])
print(coptimal_k_val)

