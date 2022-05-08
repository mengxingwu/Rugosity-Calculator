setwd("~/Photogrammetry_Models/Houbihu_2022Kenting")
SA.data<-data.frame()
resolution<-c(0.09197660818713450197, 1,2,4,8,16,32,64,128)
SurfaceArea<-c(72.232597, 
               52.142888,46.030649,
               40.545429,   36.371377,
               33.404828,   30.013861,
               27.595310,   23.941212)
PlanAreaCalc<-c(   32.087224,
                   31.983836, 31.870331,
                   31.642130, 31.196224,
                   30.297351, 28.587016,
                   25.276941, 19.258621)
NullArea<-c( 7.743537,
             7.744112,   7.780971,
             7.741504,  7.853018,
             7.612917,  7.723510,
             6.820762, 6.419540)
PlanArea<-PlanAreaCalc-NullArea
SA.data<-data.frame(resolution=resolution, 
                    SurfaceArea=SurfaceArea, 
                    PlanArea=PlanArea)
SA.data$SurfaceComplexity<-
  SA.data$SurfaceArea/SA.data$PlanArea

SA.data$LogSA<-log(SurfaceArea)
SA.data$LogRES<-log(resolution/100)

lmSA128<-lm(LogSA~LogRES, data=SA.data)
lmSA64<-lm(LogSA[1:8]~LogRES[1:8], data=SA.data)
FD128<-2-lmSA64$coefficients[2]
FD64<-2-lmSA128$coefficients[2]
library(ggplot2)

FD128Plot<-ggplot(SA.data, aes(x=LogRES, 
                               y=LogSA))+
  geom_point()+
  geom_smooth(method='lm')+
  xlab('log(Resolution)')+
  ylab('log(Surface Area)')+
  ggtitle(paste('Houbihu_5m; ', 'D=',FD128))

FD64Plot<-ggplot(SA.data[1:8, ], aes(x=LogRES, 
                                     y=LogSA))+
  geom_point()+
  geom_smooth(method='lm')+
  xlab('log(Resolution)')+
  ylab('log(Surface Area)')+
  ggtitle(paste('Houbihu_5m; ', 'D=',FD64))


#SurfCompHighRes<-72.549800/(31.831487-7.515280)
ggsave('HBH_FD128result.png', FD128Plot, dpi=1000)
ggsave('HBH_FD64result.png', FD64Plot, dpi=1000)
write.csv(SA.data, 'HBH_SA.csv')
