SA.data<-data.frame()
resolution<-c(1,2,4,8,16,32,64,128)
SurfaceArea<-c(51.090546, 46.223506,
               41.744304,  37.656329,
               34.154483,  30.875144,
               27.922805,  22.931112)
PlanAreaCalc<-c(31.852690, 31.738863,
                31.511964, 31.051005,
                30.147869,  28.415233,
                25.428404,  19.180282)
NullArea<-c(7.895686, 7.889420,
            7.847752, 7.939805,
            7.822852,  7.509740,
            6.901995, 6.393427)
PlanArea<-PlanAreaCalc-NullArea
SA.data<-data.frame(resolution=resolution, 
           SurfaceArea=SurfaceArea, 
           PlanArea=PlanArea)
SA.data$SurfaceComplexity<-
  SA.data$SurfaceArea/SA.data$PlanArea

SA.data$LogSA<-log(SurfaceArea)
SA.data$LogRES<-log(resolution/100)

lmSA<-lm(LogSA~LogRES, data=SA.data)
FD128<-2-lmSA$coefficients[2]

library(ggplot2)

ggplot(SA.data, aes(x=LogRES, 
                    y=LogSA))+
  geom_point()+
  geom_smooth(method='lm')+
  xlab('log(Resolution)')+
  ylab('log(Surface Area)')+
  ggtitle(paste('Jialeshui_5m; ', 'D=',FD128))
