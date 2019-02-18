# K-fold Validation

#Ionosphere Dataset
ionosphere_dataset<-read.table("ionosphere.data.txt",header=FALSE,sep=",")
ionosphere_dataset

dim(ionosphere_dataset)

brk_point<-seq(from=0,to=nrow(ionosphere_dataset),len=6)
group_data<-cut(1:nrow(ionosphere_dataset),brk_point,labels=F)
group_data
ionosphere_dataset<-cbind(ionosphere_dataset,group_data)
ionosphere_dataset

#generating test and train files 5
for( i in seq(1:5))
{
  ionosphere_test_dataset<-ionosphere_dataset[ionosphere_dataset$group_data %in% i,]
  ionosphere_test_dataset
  x<-setdiff(seq(1:5),i)
  ionosphere_train_dataset<-ionosphere_dataset[ionosphere_dataset$group_data %in% x,]
  ionosphere_train_dataset
  
  write.csv(ionosphere_test_dataset,paste0("ionosphere_test_",i,".csv"),row.names = F)
  write.csv(ionosphere_train_dataset,paste0("ionosphere_train_",i,".csv"),row.names = F)
  
  
}

# Car Evaluation DataSet

careval<-read.table("car.data.txt",header=F,sep=",")
careval
View(careval)

car_brk_point<-seq(from=0,to=nrow(careval),len=6)
cgrp_data<-cut(1:nrow(careval),car_brk_point,labels=F)
cgrp_data

careval<-cbind(careval,cgrp_data)
careval

#generating test and train 5 files
for( i in seq(1:5))
{
  careval_test_dataset<-careval[careval$cgrp_data %in% i,]
  careval_test_dataset
  x<-setdiff(seq(1:5),i)
  careval_train_dataset<-careval[careval$cgrp_data %in% x,]
  careval_train_dataset
  
  write.csv(careval_test_dataset,paste0("careval_test_",i,".csv"),row.names = F)
  write.csv(careval_train_dataset,paste0("careval_train_",i,".csv"),row.names = F)
  
  
}


#For Credit card Dataset

#Datapreprocessing for Credit Card Dataset
creditApproval_Dataset<-read.table("crx.data.txt",header=F,sep = ",")
creditApproval_Dataset

dim(creditApproval_Dataset)

#Checking number of Columns Containing Missing Values
cols<-which(apply(creditApproval_Dataset, 2, function(z) any(grepl("?", z))))
cols


 #Cleaning first Column
  vec<-creditApproval_Dataset[,1]
  rownum<-which(vec %in% "?")
  rows<-vec[which(vec %in% c("a","b"))]
  x<-table(rows)
  sum(table(rownum))
  m<-min(sum(rows %in% "a"),sum(rows %in% "b"))
  xx<-as.data.frame(x)
  xx
  c<-xx$rows[xx$Freq==m]
  for(i in 1:length(rownum))
  {
    r<-rownum[i]
    vec[r]<-c
  }
  vec
  creditApproval_Dataset[,1]<-((vec))
  creditApproval_Dataset[,1]
  
  #Checking and Cleaning seocnd column
  vec1<-creditApproval_Dataset[,2]
  vec1
  rownum1<-which(vec1 %in% "?")
  rownum1
  rownum2<-which(vec1 !="?")
  rownum2
  vec2<-vec1[rownum2]
  vec2
  df<-as.numeric(levels(vec2)[vec2])
  avgval<-mean((df))
  avgval<-round(avgval,digits = 2)
  
  levels(vec1) <- c(levels(vec1),avgval)
  for(i in 1:length(rownum1))
  {
    r1<-rownum1[i]
    vec1[r1]<-avgval
  }
  vec1
  creditApproval_Dataset[,2]<-vec1
  
  #Checking and Cleaning Column3 No Missing Values in Column3
  vec3<-creditApproval_Dataset[,3]
  vec3
  rownum3<-which(vec3 %in% "?")
  rownum3
  
  #Checking and Cleaning Column 4
  vec4<-creditApproval_Dataset[,4]
  vec4
  rownum4<-which(vec4 %in% "?")
  rownum4
  
  rows1<-factor(creditApproval_Dataset[,4],levels=levels(creditApproval_Dataset[,4])[levels(creditApproval_Dataset[,4]) != "?"])
  
  x1<-table(rows1)
  sum(table(rownum4))
  
  #Considering Second lowest Min Value
  m1<-min(x1)
  m<-min(x1[x1>m1])
  
  xx1<-as.data.frame(x1)
  xx1
  c1<-xx1$rows[xx1$Freq==m]
  for(i in 1:length(rownum4))
  {
    r<-rownum4[i]
    vec4[r]<-c1
  }
  vec4
  creditApproval_Dataset[,4]<-((vec4))
  creditApproval_Dataset[,4]
  
  
  #Checking and Cleaning Column 5 Missing Values Present
  vec5<-creditApproval_Dataset[,5]
  vec5
  
  rownum5<-which(vec5 %in% "?")
  rownum5
  
  rows2<-factor(creditApproval_Dataset[,5],levels=levels(creditApproval_Dataset[,5])[levels(creditApproval_Dataset[,5]) != "?"])
  rows2
  
  x2<-table(rows2)
  x2
  m1<-min(x2)
  m<-min(x2[x2>m1])
  xx2<-as.data.frame(x2)
  xx2
  c2<-xx2$rows[xx2$Freq==m]
  for(i in 1:length(rownum5))
  {
    r<-rownum5[i]
    vec5[r]<-c2
  }
  vec5
  creditApproval_Dataset[,5]<-((vec5))
  creditApproval_Dataset[,5]
  
  
  
  #Checking and Cleaning Column6
  vec6<-creditApproval_Dataset[,6]
  vec6
  
  rownum6<-which(vec6 %in% "?")
  rownum6
  
  rows3<-factor(creditApproval_Dataset[,6],levels=levels(creditApproval_Dataset[,6])[levels(creditApproval_Dataset[,6]) != "?"])
  rows3
  
  x3<-table(rows3)
  
  x3
  
  m1<-max(x3)
  m<-max(x3[x3<m1])
  
  xx3<-as.data.frame(x3)
  xx3
  c3<-xx3$rows[xx3$Freq==m]
  for(i in 1:length(rownum6))
  {
    r<-rownum6[i]
    vec6[r]<-c3
  }
  vec6
  creditApproval_Dataset[,6]<-((vec6))
  creditApproval_Dataset[,6]
  
  
  
  
  #Checking and Cleaning Column7
  vec7<-creditApproval_Dataset[,7]
  vec7
  
  rownum7<-which(vec7 %in% "?")
  rownum7
  
  rows4<-factor(creditApproval_Dataset[,7],levels=levels(creditApproval_Dataset[,7])[levels(creditApproval_Dataset[,7]) != "?"])
  rows4
  
  x4<-table(rows4)
  x4
  
  m1<-max(x4)
  m<-max(x4[x4<m1])
  
  xx4<-as.data.frame(x4)
  xx4
  c4<-xx4$rows[xx4$Freq==m]
  for(i in 1:length(rownum7))
  {
    r<-rownum7[i]
    vec7[r]<-c4
  }
  vec7
  creditApproval_Dataset[,7]<-((vec7))
  creditApproval_Dataset[,7]
  
  
  
  
  
  #Checking and Cleaning Column8 No missing Values
  vec8<-creditApproval_Dataset[,8]
  vec8
  
  #Checking and Cleaning Column9 No missing Values 
  vec9<-creditApproval_Dataset[,9]
  vec9
  
  #Checking and Cleaning Column10 No missing Values
  vec10<-creditApproval_Dataset[,10]
  vec10
  
  #Checking and Cleaning Column11 No missing Values
  vec11<-creditApproval_Dataset[,11]
  vec11
  
  #Checking and Cleaning Column12 No missing Values
  vec12<-creditApproval_Dataset[,12]
  vec12
  
  #Checking and Cleaning Column13 No missing Values
  vec13<-creditApproval_Dataset[,13]
  vec13
  
  #Checking and Cleaning Column14 Missing Values present
  vec14<-creditApproval_Dataset[,14]
  vec14
  
  rownum14<-which(vec14 %in% "?")
  rownum14
  
  rownum14_1<-which(vec14 !="?")
  rownum14_1
  
  vec14_1<-vec14[rownum14_1]
  vec14_1
  
  df<-as.numeric(levels(vec14_1)[vec14_1])
  avgval<-mean((df))
  avgval<-round(avgval,digits = 0)
  avgval<-sprintf("%05d",avgval)
  
  levels(vec14) <- c(levels(vec14),avgval)
  for(i in 1:length(rownum14))
  {
    r1<-rownum14[i]
    vec14[r1]<-avgval
  }
  vec14
  creditApproval_Dataset[,14]<-vec14
  creditApproval_Dataset[,14]
  
  
  
  #Checking and Cleaning Column15 No missing Values
  vec15<-creditApproval_Dataset[,15]
  vec15
  
  #Checking and Cleaning Column16 No missing Values
  vec16<-creditApproval_Dataset[,16]
  vec16
  
  #Now implementing K fold validation
  View(creditApproval_Dataset)
  
  cr_brk_point<-seq(from=0,to=nrow(creditApproval_Dataset),len=6)
  crgrp_data<-cut(1:nrow(creditApproval_Dataset),cr_brk_point,labels=F)
  crgrp_data
  
  creditApproval_Dataset<-cbind(creditApproval_Dataset,crgrp_data)
  creditApproval_Dataset
  
  #generating test and train 5 files
  for( i in seq(1:5))
  {
    credit_test_dataset<-creditApproval_Dataset[creditApproval_Dataset$crgrp_data %in% i,]
    credit_test_dataset
    x<-setdiff(seq(1:5),i)
    credit_train_dataset<-creditApproval_Dataset[creditApproval_Dataset$crgrp_data %in% x,]
    credit_train_dataset
    
    write.csv(credit_test_dataset,paste0("credit_test_",i,".csv"),row.names = F)
    write.csv(credit_train_dataset,paste0("credit_train_",i,".csv"),row.names = F)
    
    
  }
  
  
  