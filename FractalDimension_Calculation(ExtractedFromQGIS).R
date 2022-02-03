SA.data<-data.frame()
resolution<-c(1,2,4,8,16,32,64)
SurfaceArea<-c(59.569537, 53.947938,
               48.532532, 43.573056, 
               39.194458, 32.772937,
               25.832193)
PlanAreaCalc<-c(45.817313, 45.682153,
                45.410640, 44.871627,
                43.840381,  41.680608,
                37.977579)
NullArea<-c(21.971066, 21.971740, 
            22.070572, 21.985295,
            21.597104,  21.882319,
            22.406772)
PlanArea<-PlanAreaCalc-NullArea
SA.data<-data.frame(resolution=resolution, 
           SurfaceArea=SurfaceArea, 
           PlanArea=PlanArea)
SA.data$SurfaceComplexity<-
  SA.data$SurfaceArea/SA.data$PlanArea

SA.data$LogSA<-log(SurfaceArea)
SA.data$LogRES<-log(resolution/100)

lmSA<-lm(LogSA~LogRES, data=SA.data)
FD64<-2-lmSA$coefficients[2]

library(ggplot2)

ggplot(SA.data, aes(x=LogRES, 
                    y=LogSA))+
  geom_point()+
  geom_smooth(method='lm')+
  xlab('log(Resolution)')+
  ylab('log(Surface Area)')+
  ggtitle(paste('Turtle Island; ', 'D=',FD64))
