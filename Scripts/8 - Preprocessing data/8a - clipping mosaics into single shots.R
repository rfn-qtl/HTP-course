###################################
# HTP in PB
# Clipping mosaic into single shots
# Author: Roberto Fritsche Neto
# E-mail: rfneto@agcenter.lsu.edu
# Last update: Mar 22, 2024 
###################################

# Libraries
library(raster)
library(EBImage)
library(rgdal)
library(foreach)
library(doParallel)

# setting the number of cores that will be used
registerDoParallel(cores = 9) # type the number of cores you want to use
getDoParWorkers()

#importing DH mosaics
setwd("../../datasets/DHmosaic")
(mosaics <- dir())

mosaic_1 <- stack(mosaics[1])
print(mosaic_1)
plot(mosaic_1) #plot all bands
plotRGB(mosaic_1)

# Run, the for( below, then click on four points at the corners of the image of interest
corners <- NULL
for(i in 1:4){
  aux <- locator(type = "p", n = 1, col = "red", pch = 19)
  corners <- rbind(corners, c(aux$x, aux$y))
}
corners
corners <- rbind(corners, corners[1,])
colnames(corners) <- c("x", "y")
lines(corners, col= "red", type = "l", lty = 2, lwd = 3)

# create objects of class SpatialPolygons
plg <- Polygons(list(Polygon(corners)), "x")
# create objects of class SpatialPolygonsDataFrame
shpfl <- SpatialPolygonsDataFrame(SpatialPolygons(list(plg)), 
                                  data.frame(z=1, 
                                             row.names=c("x") ) )
# Now, both on the same projections
raster::projection(shpfl) <- raster::projection(mosaic_1)

# define how many rows and cols
grid <- raster(shpfl, 
               nrows=3, # number of image rows
               ncols=7, # number of image cols
               crs = proj4string(mosaic_1))

# then, save as a polygon
shpfl <- rasterToPolygons(grid)
length(shpfl)

#let's check i it works
dev.off()
plotRGB(mosaic_1)
plot(shpfl, add = T, col = "white")

# clipping raster by plots - a function in parallel 
plot_clip <- function(ortho, shape) {
  results <- foreach(i = 1:length(shape), .packages = "raster") %dopar% {
    temp <- mask(crop(x = ortho, y = extent(shape[i,])), shape[i,])
    return(temp)
  }
  return(results)} 

# clipping all seeds in single shots
rasterbyplots <- plot_clip(mosaic_1, shpfl)
class(rasterbyplots)
length(rasterbyplots)
plotRGB(rasterbyplots[[21]])

# save the single shots in a folder
# First, let's create a new directory
dir.create("../ssDH")

# Save the singles images .png format into the newest directory
foreach(i = 1:length(rasterbyplots), .packages = "rgdal", .export = "writeGDAL") %dopar% {
  writeGDAL(as(rasterbyplots[[i]], "SpatialGridDataFrame"), 
            paste("../ssDH/", "D", "_", i, ".png", sep = ""), 
            drivername = "PNG", 
            type = "CFloat64")
}

##### The End #######
