#Naive Bayes on careval dataset
require(reshape2)
require(caret)
require(reshape)
require(data.table)
#k=1
cerr<-c(0)
for(k in seq(1:5))
{
  filename1=paste("careval_train",k,sep="_")
  filename2=paste("careval_test",k,sep = "_")
  ext=".csv"
  filename1=paste(filename1,ext,sep="")
  filename2=paste(filename2,ext,sep="")
  print(filename1)
  print(filename2)
  
  #Get the train dataset
  careval_train<-read.csv(filename1,header=TRUE)
  #View(careval_train)
  
  
  #Get the test dataset
  careval_test<-read.csv(filename2,header=TRUE)
  
  #View(careval_test)
  
  #Get the target
  careval_train_org<-careval_train
  careval_train[,ncol(careval_train)]<-NULL
  target_car<-careval_train[,ncol(careval_train)]
  careval_train[,ncol(careval_train)]
  
  #Get class Labels
  cl_car<-unique(target_car)
  #print(cl_car)
  
  
  lev<-levels(cl_car)
  nr_car<-length(lev)
  #print(nr_car)
  #cp<-1/(nr_car)
  #cprior<-cp
  
  
  #Estimate each column label
  
  dat.new1 <- dcast(careval_train, careval_train$V7~careval_train$V1, fun.aggregate = length)
  print(dat.new1)
  
  dat.new2 <- dcast(careval_train, careval_train$V7~careval_train$V2, fun.aggregate = length)
  print(dat.new2)
  
  dat.new3 <- dcast(careval_train, careval_train$V7~careval_train$V3, fun.aggregate = length)
  print(dat.new3)
  
  
  dat.new4 <- dcast(careval_train, careval_train$V7~careval_train$V4, fun.aggregate = length)
  print(dat.new4)
  
  dat.new5 <- dcast(careval_train, careval_train$V7~careval_train$V5, fun.aggregate = length)
  print(dat.new5)
  
  dat.new6 <- dcast(careval_train, careval_train$V7~careval_train$V6, fun.aggregate = length)
  print(dat.new6)
  
  #Frequencies class label
  #column7
  v7f<-table(careval_train$V7)
  v7f<-as.data.frame(v7f)
  print(v7f)
  
  probs<-matrix(,nrow=0,ncol=nr_car)
  colnames(probs)<-lev
  
  careval_test$cgrp_data<-NULL
  cact_target<-careval_test$V7
  careval_test$V7<-NULL
  #View(careval_test)
  
  
  weights<-c(0)
  wks<-c(0)

  #m<-1
  #m<-nrow(careval_train)
  for(i in 1:nrow(careval_test))
  {
    xx<-careval_test[i,]
    
    for(j in 1:length(lev))
    {
      for(h in 1:length(xx))
      {
        lab<-xx[h]
        lab=unname(unlist(lab))
        lab<-as.character(lab)
        if(h==1)
        {
          #cpr1<-cal_probs(lab,dat.new1,v1f,cp,lev[j])
          nc<-dat.new1[which(dat.new1$`careval_train$V7`==lev[j]),lab]
          #print(nc)
          n<-v7f[which(v7f$Var1==lev[j]),2]
          cpr1<-(nc/n)
          if(cpr1==0)
          {
            #print(n)
            m<-(0.01*nrow(careval_train))
            cp=(1/length(unique(careval_train$V1)))
            cpr1<-(nc+(m*cp))/(n+m)
          }
          wks[h]<-cpr1
        }
        if(h==2)
        {
          #cpr2<-cal_probs(lab,dat.new2,v2f,cp,lev[j])
         
          nc<-dat.new2[which(dat.new2$`careval_train$V7`==lev[j]),lab]
          n<-v7f[which(v7f$Var1==lev[j]),2]
          cpr2<-nc/n
          if(cpr2==0)
          {
          #n<-v2f[which(v2f$Var1==lab),2]
          #print(n)
          m<-(0.01*nrow(careval_train))
          cp=(1/length(unique(careval_train$V2)))
          cpr2<-(nc+(m*cp))/(n+m)
          }
          wks[h]<-cpr2
        }
        if(h==3)
        {
          #cpr3<-cal_probs(lab,dat.new3,v3f,cp,lev[j])
          nc<-dat.new3[which(dat.new3$`careval_train$V7`==lev[j]),lab]
          n<-v7f[which(v7f$Var1==lev[j]),2]
          cpr3<-(nc/n)
          if(cpr3==0)
          {
            #n<-v3f[which(v3f$Var1==lab),2]
            m<-(0.01*nrow(careval_train))
            cp=(1/length(unique(careval_train$V3)))
            cpr3<-(nc+(m*cp))/(n+m)
          }
          wks[h]<-cpr3
        }
        if(h==4)
        {
          #cpr4<-cal_probs(lab,dat.new3,v4f,cp,lev[j])
          nc<-dat.new4[which(dat.new4$`careval_train$V7`==lev[j]),lab]
          n<-v7f[which(v7f$Var1==lev[j]),2]
          cpr4<-(nc/n)
          if(cpr4==0)
          {
          
            m<-(0.01*nrow(careval_train))
            cp<-(1/length(unique(careval_train$V4)))
            cpr4<-(nc+(m*cp))/(n+m)
          }
          wks[h]<-cpr4
          
        }
        if(h==5)
        {
          #cpr5<-cal_probs(lab,dat.new3,v5f,cp,lev[j])
          nc<-dat.new5[which(dat.new5$`careval_train$V7`==lev[j]),lab]
          n<-v7f[which(v7f$Var1==lev[j]),2]
          cpr5<-nc/n
          if(cpr5==0)
          {
          #n<-v5f[which(v5f$Var1==lab),2]
          #m<-3
          m<-(0.01*nrow(careval_train))
          cp<-(1/length(unique(careval_train$V5)))
          cpr5<-(nc+(m*cp))/(n+m)
          }
          
          wks[h]<-cpr5
          
        }
        if(h==6)
        {
          #cpr6<-cal_probs(lab,dat.new6,v6f,cp,lev[j])
          nc<-dat.new6[which(dat.new6$`careval_train$V7`==lev[j]),lab]
          n<-v7f[which(v7f$Var1==lev[j]),2]
          cpr6<-(nc/n)
          if(cpr6==0)
          {
          #n<-v6f[which(v6f$Var1==lab),2]
          m<-(0.01*nrow(careval_train))
          cp<-(1/length(unique(careval_train$V6)))
          cpr6<-(nc+(m*cp))/(n+m)
          }
          wks[h]<-cpr6
        }
        cp1<-v7f[which(v7f$Var1==lev[j]),2]
        cprior<-cp1/nrow(careval_train)
        #cprior<-(1/nr_car)
        weights[j]<-(prod(wks)*cprior)
        
      }
    }
    
    probs<-rbind(probs,weights)
    
  }
  
  #View(probs)
  
  #predict the label of every row of test
  cget_label<-function(cweight)
  {
    clabel<-c(0)
    ceach_max_val<-c(0)
    
    for(i in 1:nrow(cweight))
    {
      cvl<-max(cweight[i,])
      #print(vl)
      ceach_max_val[i]<-cvl
      clabel[i]<-colnames(cweight)[which(cweight[i,] == cvl)] 
      #print(label[i])
    }
    
    
    #label<-names(w)[apply(w, 1, function(v1) which(v1 == each_min_val))]
    
    #print(label)
    return(clabel)
  }
  
  clabels<-cget_label(probs)
  print(clabels)
  
  

  cact_target<-as.factor(cact_target)
  
  cl1<-levels(cact_target)
  cl1<-levels(target_car)
  cact_target<-factor(cact_target, levels=cl1)
  clabels<-factor(clabels,levels = cl1)
  cconf<-confusionMatrix(clabels,cact_target)
  print(cconf)
  tab1<-as.data.frame(cconf$table)
  print(tab1)
  ce<-tab1[2,"Freq"]+tab1[3,"Freq"]+tab1[4,"Freq"]+tab1[5,"Freq"]+tab1[7,"Freq"]+
    tab1[8,"Freq"]+tab1[9,"Freq"]+tab1[10,"Freq"]+tab1[12,"Freq"]+tab1[13,"Freq"]+tab1[14,"Freq"]+tab1[15,"Freq"]
  ct<-sum(tab1[,"Freq"])
  cerror<-ce/ct
  cerr[k]<-cerror
  
  
  

}
print(cerr)
plot(seq(1:5),cerr,type="o",xlab="No of Folds",ylab="Error_rate")

#Using naive baye's package
require(e1071)
#k=1
cerr1<-c(0)
for(k in seq(1:5))
{
  filename1=paste("careval_train",k,sep="_")
  filename2=paste("careval_test",k,sep = "_")
  ext=".csv"
  filename1=paste(filename1,ext,sep="")
  filename2=paste(filename2,ext,sep="")
  print(filename1)
  print(filename2)
  
  car_train_p<-read.csv(filename1,header=TRUE)
  
  car_train_p[,ncol(car_train_p)]<-NULL
  target_car<-car_train_p[,ncol(car_train_p)]
  
  car_test_p<-read.csv(filename2,header=TRUE)
  
  car_test_p$cgrp_data<-NULL
  cact_val<-car_test_p[,ncol(car_test_p)]
  car_test_p[,ncol(car_train_p)]<-NULL
  
  
  model <- naiveBayes(x=car_train_p[,-ncol(car_train_p)],y=target_car, data = car_train_p)
  class(model) 
  pred <- predict(model,car_test_p)
  table(pred)
  table(cact_val)
  cl2<-levels(cact_val)
  cl2<-levels(target_car)
  
  cact_val<-factor(cact_val, levels=cl2)
   
  
  
  cconf_p<-confusionMatrix(pred,cact_val)
  print(cconf_p)
  tab2<-(cconf_p$table)
  tab2<-as.data.frame(tab2)
  print(tab2)
  ce1<-tab2[2,"Freq"]+tab2[3,"Freq"]+tab2[4,"Freq"]+tab2[5,"Freq"]+tab2[7,"Freq"]+tab2[8,"Freq"]+
    tab2[9,"Freq"]+tab2[10,"Freq"]+tab2[12,"Freq"]+tab2[13,"Freq"]+tab2[14,"Freq"]+tab2[15,"Freq"]
  ct1<-sum(tab2[,"Freq"])
  cerror1<-(ce1/ct1)
  print(cerror1)
  cerr1[k]<-cerror1
  
}

print(cerr1)
plot(seq(1:5),cerr1,type="o",xlab="No of Folds",ylab="Error_rate")
