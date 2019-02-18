#HC 
set.seed(100)
ionosphere_hc<-read.csv("ionosphere.data.txt",sep=',',header=FALSE)
ionosphere_hc_original_1<-ionosphere_hc
ionosphere_hc$V35<-NULL
ionosphere_hc_sample<-sample(nrow(ionosphere_hc),size=50,replace=FALSE)
ionosphere_hc_dataset<-ionosphere_hc[ionosphere_hc_sample,]
print(ionosphere_hc_dataset)
ionosphere_hc_original_1<-ionosphere_hc_original_1[ionosphere_hc_sample,]
#---HC---

#distance matrix
d<-dist(as.matrix(ionosphere_hc_dataset),method="euclidean")
hc<-hclust(d,method="complete")
plot(hc)


#----To plot cuttree------

#t<-cutree(hc,k=2,h=8)
#print(t)
plot(cut(as.dendrogram(hc), h = 8)$lower[[1]], main = "First tree of cut at h=8")
plot(cut(as.dendrogram(hc), h = 8)$lower[[2]], main = "Second tree of cut at h=8")

#---To calculate the error--------
hc_cluster<-cutree(hc,2)
dt1<-table(hc_cluster,ionosphere_hc_original_1$V35)
print(dt1)
error_rate_1<-NULL
for(i in 1:2)
{
  min_val<-min(dt1[i,])
  sum_val<-sum(dt1[i,])
  error_rate_1[i]<-(min_val/sum_val)
}

print(sum(error_rate_1))

