library(readxl)
profile1<-read_xlsx('Test1.xlsx')
str(profile1)
head(profile1)
plot(x=profile1$X, y=profile1$Y, 
     type='l')
linearRugosity<-function(X,Y){
  x<-X
  y<-Y
  x<-as.data.frame(x)
  y<-as.data.frame(y)
  dis<-c()
  ydis<-c()
  for(i in 2:nrow(x)){
    ydis[i]<-y[i,]-y[i-1,]
    dis[i]<-x[i,]-x[i-1,]
  }
  ThreeD.dist<-sqrt(dis^2+ydis^2)
  ThreeD.dist.sum<-sum(ThreeD.dist, na.rm=T)
  LinearRugosity<-ThreeD.dist.sum/max(x)

  
  paste('Linear Rugosity=', round(LinearRugosity, 5),
        ', Scale=', round(abs(x[2,]-x[1,]), 5))
}

linearRugosity(profile1[1], profile1[2])




linearRugosity.return<-function(X,Y){
  x<-X
  y<-Y
  x<-as.data.frame(x)
  y<-as.data.frame(y)
  dis<-c()
  ydis<-c()
  for(i in 2:nrow(x)){
    ydis[i]<-y[i,]-y[i-1,]
    dis[i]<-x[i,]-x[i-1,]
  }
  ThreeD.dist<-sqrt(dis^2+ydis^2)
  ThreeD.dist.sum<-sum(ThreeD.dist, na.rm=T)
  LinearRugosity<-ThreeD.dist.sum/max(x)
  
  return(LinearRugosity)
  
}

linearRugosity.return(profile1[13], profile1[14])

OveralRugosity<-function(data){
K<-c()
n<-ncol(data)/2
for(i in 1:n){
K[i]<- as.vector(linearRugosity.return(data[2*i-1], 
            data[2*i]))
  }
mean(K)
}
OveralRugosity(data=profile1[1:10])

plot(profile1$X4, profile1$Y4, type='l')


linearRugosity(X=profile1$X2, Y=profile1$Y2)


shanfuProfile<-read_xlsx('ShanfuTest_2.xlsx')
str(shanfuProfile)
OveralRugosity(shanfuProfile)


########################################
####fractal dimension##################
library(fractaldim)


###check function in fractaldim###
shanfu.fd<-
  fd.estim.boxcount(as.matrix(shanfuProfile[1:2]), 
                               nlags='auto', 
                                plot.loglog = TRUE,
                                plot.allpoints = TRUE)
shanfu.fd.2<-fd.estim.boxcount(as.matrix(shanfuProfile[3:4]), 
                              nlags='auto', 
                              plot.loglog = TRUE,
                              plot.allpoints = TRUE)


###Constructing FD function######
FDcalculator<-function(profile){
FD.list<-list()
n<-ncol(profile)/2
profile.fd<-c()
for(i in 1:n){
FD.list[[i]]<-
fd.estim.boxcount(as.matrix(profile[(2*i-1):(2*i)]), 
                  nlags='auto', 
                  plot.loglog = TRUE,
                  plot.allpoints = TRUE)
profile.fd[i]<-FD.list[[i]]$fd
}
paste('FD=', mean(profile.fd))
}


FDcalculator(profile1) #test the function
FDcalculator(shanfuProfile)
##########End of Function construction##########
#################################################
n<-2*c(1:22)
FD.shanfu<-fd.estim.boxcount(as.matrix(shanfuProfile[n]), 
                  nlags='auto', 
                  plot.loglog = TRUE,
                  plot.allpoints = TRUE)
FD.shanfu$fd

