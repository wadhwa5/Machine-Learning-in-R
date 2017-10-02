#--------------Functions----------------------------------------
load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
show_digit <- function(arr784, col=gray(12:1/12), ...) {
  image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
}
ED<-function(a,b){dist(rbind(a, b)) }
distance_matrix<- function(p,k){
  D<-matrix(0,nrow(p),nrow(k))
  for (i in 1:nrow(p)){
    for (j in 1:nrow(k)){
        D[i,j]<-ED(p[i,],k[j,])
    }
  }
    D
}
cluster<-function(distm){
  apply(distm,1,function(x) which.min(x))
  
}
new_cluster_point<-function(p,k,clus){
  for (i in 1:nrow(k)){
    k[i,]<-colMeans(p[clus==i,])
  }
  k
}
SSD<-function(final_clus,points,centroid){
  SS=0
  for (i in 1:10){
    df<-points[final_clus==i,]
    for (j in 1:nrow(df)){
      SS=SS+ED(df[j,],centroid[i,])
    }
    
  }
  SS
}
km<-function(p,n_klus,n_inital){
  point<-p
  for(i in 1:n_inital){
    k<-point[sample(nrow(point),n_klus),]
    for(i in 1:50){
      new_k<-new_cluster_point(point,k,cluster(distance_matrix(point,k)))
      k<-new_k
      
    }
   cent<-k 
  }
  #cent
  final_clus=cluster(distance_matrix(point,cent))
  Sum_S<-SSD(final_clus,p,cent)
  return (list(final_centriod =cent, label=final_clus, SumSquare=Sum_S))
}
#------------------- Data Input --------------------------------
train <<- load_image_file("train-images-idx3-ubyte")
test <<- load_image_file('t10k-images-idx3-ubyte')
train$y <- load_label_file('train-labels-idx1-ubyte')
test$y <- load_label_file('t10k-labels-idx1-ubyte')  

train$x<-train$x[1:1000,]
train$y<-train$y[1:1000]
#---------------------- K-NN ------------------------------------

cn<-km(train$x,10,1)



