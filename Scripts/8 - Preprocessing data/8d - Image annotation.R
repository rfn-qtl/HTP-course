###################################
# HTP in PB
# Imagery for annotation
# Author: Roberto Fritsche-Neto
# E-mail: rfneto@agcenter.lsu.edu
# Last update: March 22, 2024 
###################################

library("EBImage")
library("raster")
library("rgdal")
library("doParallel")
library(foreach)

# setting the number of cores that will be used
registerDoParallel(cores = 9) # type the number of cores you want to use
getDoParWorkers()

# let's create a new directory
dir.create("../../datasets/annotation/images")
out.dir <- "../../datasets/annotation/images"

#importing a mosaic
mosaic_V3 <- stack("../../datasets/mosaics/18DAS_09_02_drone.tif")
plotRGB(mosaic_V3) #plot composite image

# importing shapefiles (plots)
plots_rep1 <- shapefile("../../datasets/plots/PI_R1_DAS.shp")
plot(plots_rep1, add = T, col = "White")

# clipping raster by plots - a function in parallel 
plot_clip <- function(ortho, shape) {
  results <- foreach(i = 1:length(shape), .packages = "raster") %dopar% {
    temp <- mask(crop(x = ortho, y = extent(shape[i,])), shape[i,])
    return(temp)
    }
  return(results)} 


# clipping all plots - it takes 2 minutes
system.time(
V3plots <- plot_clip(mosaic_V3, plots_rep1)
)

# let's see an example
class(V3plots)
length(V3plots)
plotRGB(V3plots[[356]])
print(V3plots[[356]])
class(V3plots[[356]])

# Save some plots as .png format into the newest directory
for (i in 356:length(V3plots)) {
writeGDAL(as(V3plots[[i]], "SpatialGridDataFrame"), 
          paste(out.dir, "/", "parcelaRGB", "_", i, ".png", sep = ""), 
          drivername = "PNG", 
          type = "CFloat64")
  }

# then, annotate them at http://www.robots.ox.ac.uk/~vgg/software/via/via.html

#### The End ############