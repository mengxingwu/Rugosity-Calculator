library(rgdal)
library(raster)
ext <- extent(60.85, 105.0417, 15.95833, 39.31667)
ShanfuDEM<-raster('Shanfu_10m_orthomosaic.tif')
plot(ShanfuDEM)
ShanfuDEM[] <- runif(ncell(ShanfuDEM))

library(dismo)
set.seed(123)
backgr <- randomPoints(ShanfuDEM, 10)
backgrvals <- extract(ShanfuDEM, backgr)

pdf('ShanfuDEM.pdf', height=15, width=15)
plot(ShanfuDEM)
points(backgr, col='black')
dev.off()


par(mfrow=c(1,1))

ShanfuOrtho<-raster('Shanfu_10m_orthomosaic.tif')
plot(ShanfuOrtho)
ShanfuDEM[] <- runif(ncell(ShanfuDEM))

library(dismo)
set.seed(0)
backgr <- randomPoints(ShanfuDEM, 10)
backgrvals <- extract(ShanfuDEM, backgr)

pdf('ShanfuDEM.pdf', height=15, width=15)
plot(ShanfuDEM)
points(backgr, col='black')
dev.off()
