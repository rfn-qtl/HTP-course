###################################
# HTP in PB
# Extracting data from hyperspectral imagery
# Author: Roberto Fritsche-Neto
# E-mail: rfneto@agcenter.lsu.edu
# Last update: Mar 22, 2024 
###################################

# Libraries
library(raster)
library(data.table)
library(foreach)
library(doParallel)
library(EBImage)
library(ggplot2)

# setting the number of cores that will be used
registerDoParallel(cores = 8) # type the number of cores you want to use
getDoParWorkers()

# load and visualize hyper data
setwd("../../datasets/hyperspectral")
dir()
  
img.rgb <-  stack("B1-RGB.tif") 
plotRGB(img.rgb)

img.hyper <-  brick("B1.bil", continuousdata = TRUE)

print(img.hyper)
class(img.hyper)

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
raster::projection(shpfl) <- raster::projection(img.rgb)

# define how many rows and cols
grid <- raster(shpfl, 
               nrows=1, # number of image rows
               ncols=1, # number of image cols
               crs = proj4string(img.rgb))

# then, save as a polygon
shpfl <- rasterToPolygons(grid)
length(shpfl)

#let's check i it works
dev.off()
plotRGB(img.rgb)
plot(shpfl, add = T, col = "white")

# clipping raster by plots - a function in parallel 
plot_clip <- function(ortho, shape) {
  results <- foreach(i = 1:length(shape), .packages = "raster") %dopar% {
    temp <- mask(crop(x = ortho, y = extent(shape[i,])), shape[i,])
    return(temp)
  }
  return(results)} 

# clipping all seeds in single shots
rasterbyplots <- plot_clip(img.rgb, shpfl)
class(rasterbyplots)
length(rasterbyplots)
plotRGB(rasterbyplots[[1]])

# Cropping the hyper images
hyper.crop <-  plot_clip(img.hyper, shpfl)
print(hyper.crop)
class(hyper.crop)
length(hyper.crop)

############### Choosing the best index to build a mask for RGB ##################

# Removing background using Otsu method to determine the threshold based on histogram of pixel values
EX1 <- rasterbyplots[[1]]

# First, we let's calculate EXG index (Excess Green Index)
ExGI <- overlay(x = EX1,
                fun = function(B, G, R, NIR, RE) {
                  
                  # normalizing
                  bn = B / 255
                  gn = G / 255
                  rn = R / 255
                  
                  b = bn / (bn + gn + rn)
                  g = gn / (bn + gn + rn)
                  r = rn / (bn + gn + rn)
                  
                  return((2 * g) - r - b)
                  
                }
)


NGRDI <- overlay(x = EX1,
                 fun = function(Red, Green, Blue){
                   return(((Green - Red)/(Green + Red)))}) 


GCC <- overlay(x = EX1,
               fun = function(Red, Green, Blue){
                 return(((Green - Blue)/(Green + Blue)))})

# lets' check it
plot(ExGI)
plot(NGRDI)
plot(GCC)
thr.manual <- range(click(GCC))
thr.manual

# obtain the Otsu threshold for this plot
# for the ExGI
thr <- EBImage::otsu(x = raster::as.matrix(ExGI),
            range = c(min(raster::as.matrix(ExGI), na.rm = TRUE),
                      max(raster::as.matrix(ExGI), na.rm = TRUE)
            ),
            levels = 256)
thr

# for the NGRDI
thr2 <- EBImage::otsu(x = raster::as.matrix(NGRDI),
            range = c(min(raster::as.matrix(NGRDI), na.rm = TRUE),
                      max(raster::as.matrix(NGRDI), na.rm = TRUE)
            ),
            levels = 256)
thr2

# for the GCC
thr3 <- EBImage::otsu(x = raster::as.matrix(GCC),
                      range = c(min(raster::as.matrix(GCC), na.rm = TRUE),
                                max(raster::as.matrix(GCC), na.rm = TRUE)
                      ),
                      levels = 256)
thr3

# creating a mask using the Otsu values
soilMask  <- ExGI > thr
soilMask2  <- NGRDI > thr2
soilMask3  <- GCC > thr3
soilMask4 <- GCC < -0.16 | GCC > 0.16

plot(soilMask)
plot(soilMask2)
plot(soilMask3)
plot(soilMask4)

# removing the background
# ExGI
plotXMasked <- mask(x = EX1, mask = soilMask, maskvalue = FALSE)
plotRGB(plotXMasked)
# NGRDI
plotXMasked2 <- mask(x = EX1, mask = soilMask2, maskvalue = FALSE)
plotRGB(plotXMasked2)
# GCC
plotXMasked3 <- mask(x = EX1, mask = soilMask3, maskvalue = FALSE)
plotRGB(plotXMasked3)
# Manual filter based on GCC
plotXMasked4 <- mask(x = EX1, mask = soilMask4, maskvalue = FALSE)
plotRGB(plotXMasked4)

# finally, apply the mask on a hyper
hyper_Masked <- raster::mask(x = hyper.crop[[1]], mask = soilMask4)

# Picking the number of layers in a hyper image
wavebands <- round(as.numeric(
              gsub(".nm", "", 
              gsub("X", "", names(hyper_Masked), ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE)
              , ignore.case = FALSE, perl = FALSE,fixed = TRUE, useBytes = FALSE)))

length(wavebands)
wavebands

aux2 <- as.array(hyper_Masked)
length(aux2)
class(aux2)

# obtain the median for layers 
med <- unlist(apply(as.array(hyper_Masked), MARGIN = 3, median, na.rm = T))
med

# signature for sample 1
plot(wavebands, med, type = "l", col = "red")

############################################################
# apply this pipeline for all imagery and obtain a data.frame
############################################################
# loading image hyper data files .bill
hyper.names <- dir()[dir() %like% ".bil$"]
hyper.names

rgb.names <- dir()[dir() %like% ".tif$"]
rgb.names

# pick the image id
img.id <- gsub(".bil", "", hyper.names, ignore.case = FALSE, perl = FALSE, fixed = TRUE, useBytes = FALSE)
img.id
length(img.id)

# So, it time to analyse all the images in parallel
system.time(
  results <- foreach(i = 1:length(img.id), 
                     .packages = c("raster"), 
                     .combine = "rbind") %dopar% {
    
    i.h <-  raster::brick(hyper.names[i], continuousdata = TRUE)
    i.rgb <-  stack(rgb.names[i]) 
    h.c <-  crop(i.h, shpfl)
    v.c <-  crop(i.rgb, shpfl)
    
    GCC <- overlay(x = v.c,
                   fun = function(Red, Green, Blue){
                     return(((Green - Blue)/(Green + Blue)))})
    
    Mask  <- GCC < -0.16 | GCC > 0.16
    Masked <- mask(x = v.c, mask = Mask, maskvalue = FALSE)
    h.m <- raster::mask(x = h.c, mask = Mask)
    
    md <- unlist(apply(as.array(h.m), MARGIN = 3, median, na.rm = T))
    
    output <- data.frame(id = img.id[i],
                         wavebands = wavebands,
                         reflectance = md)
    
  }
)

# reorganizing the results 
head(results)
tail(results)

# saving,
write.table(t(results), "hyperdata5.txt")
write.csv(t(results), "hyperdata5.csv")

# and visualizing signatures for plots
ggplot(results, aes(x = wavebands, y = reflectance, group = id, col = id)) +
  geom_line(aes(linetype = id)) 

###### the end ###########