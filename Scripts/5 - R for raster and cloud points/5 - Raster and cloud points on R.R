########################################
# HTP in PB
# Raster and cloud point on R practice
# Author: Roberto Fritsche Neto
# E-mail: rfneto@agcenter.lsu.edu
# Last update: Feb 8, 2024 
########################################

# Libraries
library(raster)        # working with raster objects
library(EBImage)       # otsu function
library(foreach)
library(doParallel)
library(sp)
library(tidyr)
library(pliman)
library(data.table)
library(rlas)
library(lidR)

# setting the number of cores that will be used
registerDoParallel(cores = 8) # type the number of cores you want to use
getDoParWorkers()
###########################

#importing mosaics
mosaic_parrot <- raster::stack("../../datasets/mosaics/60DAS_22_03_sequoia.tif")
print(mosaic_parrot)
plot(mosaic_parrot) #plot all
plot(mosaic_parrot, 1) #plot only band 1
# camera parrot sequoia (G, R, RE, NIR) 
plotRGB(mosaic_parrot, r=2, g=1, b=1, stretch="hist")
click(mosaic_parrot) #plot composite image - pixel composition

mosaic_VT <- stack("../../datasets/mosaics/60DAS_22_03_drone.tif")
print(mosaic_VT)
plot(mosaic_VT) #plot all
plot(mosaic_VT, 1) #plot only band 1
plotRGB(mosaic_VT) #plot composite image
click(mosaic_VT) #plot composite image - pixel composition

# importing shapefiles (plots)
plots_rep1 <- shapefile("../../datasets/plots/PI_R1_DAS.shp")
print(plots_rep1)
plot(plots_rep1, add = T, col = "White")

#Getting specific info
extent(plots_rep1) #limits
length(plots_rep1) #number of features (plots)
plot(plots_rep1[360,]) #plot feature number 1
head(plots_rep1@data, 6)
# value from name$... are the variables of the shapefile (same as qgis attribute table) e.g.:
plots_rep1$cat #plot names

# clipping raster by plots - a function in parallel 
plot_clip <- function(ortho, shape) {
  results <- foreach(i = 1:length(shape), .packages = "raster") %dopar% {
    temp <- mask(crop(x = ortho, y = extent(shape[i,])), shape[i,])
    return(temp)
  }
  return(results)} 

# clipping all plots
rasterbyplots <- plot_clip(mosaic_VT, plots_rep1)

# lets' check
class(rasterbyplots)
length(rasterbyplots)
plotRGB(rasterbyplots[[360]])
plot(plots_rep1[360,], add = T, col = "grey")
dev.off()

# Removing soil/background using Otsu method to determine the threshold based on histogram of pixel values
EX1 <- rasterbyplots[[360]]
plotRGB(EX1)

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

# lets' check it
plot(ExGI)

# obtain the threshold for this plot
thr <- EBImage::otsu(x = raster::as.matrix(ExGI),
             range = c(min(raster::as.matrix(ExGI), na.rm = TRUE),
                       max(raster::as.matrix(ExGI), na.rm = TRUE)
                       ),
                       levels = 256)

thr
 
# creating a mask using the Otsu value
soilMask  <- ExGI > thr
 
# removing the soil
plotXMasked <- mask(x = EX1, mask = soilMask, maskvalue = FALSE)
plotRGB(plotXMasked)

# applying for all plots
plots_without_soil <- list()

for (i in 1:length(rasterbyplots)) {
  cat("plot", i, "\n")
  
  ExGI <- overlay(x = rasterbyplots[[i]],
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
  
  thr <- EBImage::otsu(x = raster::as.matrix(ExGI),
              range = c(min(raster::as.matrix(ExGI), na.rm = TRUE),
                        max(raster::as.matrix(ExGI), na.rm = TRUE)
              ),
              levels = 256)
  
  soilMask  <- ExGI > thr
  plots_without_soil[[i]] <- mask(x = rasterbyplots[[i]], mask = soilMask, maskvalue = FALSE)

}

length(plots_without_soil)
print(plots_without_soil[[359]])
plotRGB(plots_without_soil[[359]])

########################################
# Indices and canopy cover
########################################
# camera phantom 4 (R, G, B) 
# camera mica sense (B, G, R, NIR, RE) 
# camera parrot sequoia (G, R, RE, NIR) 

#index	formula
# CIG	(NIR / G) - 1
# CIRE	(NIR / RE) - 1
# GNDVI	(NIR - G) / (NIR + G)
# NDVI	(NIR - R) / (NIR + R)
# NDRE	(NIR - RE) / (NIR + RE)
# PSRI	(R - G) / RE
# RVI	NIR / R

# ExG (2 * G) - R - B
# VARI	(G - R) / (G + R - B)
# NGRDI	(G - R) / (G + R)


# Estimating indices
NGRDI <- overlay(x = plots_without_soil[[360]],
                 fun = function(Red, Green, Blue){
                   return(((Green - Red)/(Green + Red)))}) 
class(NGRDI)
plot(as.raster(NGRDI))
median(raster::as.matrix(NGRDI), na.rm = T) # median for the index. Much better than mean
plots_rep1@data[360, "C_R"] #ID in the experimental design

# Estimate the canopy area
NGRDI.df <- as.data.frame(NGRDI)
head(NGRDI.df)
sum(!is.na(NGRDI.df)) / dim(NGRDI.df)[1] *100

# applying for all plots
output <- data.frame()

for (i in 1:length(plots_without_soil)) {
cat("plot", i, "\n")
   
    idx.NGRDI = overlay(x = plots_without_soil[[i]],
                  fun = function(Red, Green, Blue){
                    return(((Green - Red)/(Green + Red)))})
    
    idx.ExGI = overlay(x = plots_without_soil[[i]],
                       fun = function(Red, Green, Blue){
                         return(((2*Green/256) - Green/256 - Red/256))})
    
    idx.NGRDI.df <- as.data.frame(idx.NGRDI)
    idx.ExGI.df <- as.data.frame(idx.ExGI)
      
    output <- rbind(output, data.frame(
      C_R = plots_rep1@data[i, "C_R"],        
      NGRDI = median(raster::as.matrix(idx.NGRDI), na.rm = T),
      ExGI = median(raster::as.matrix(idx.ExGI), na.rm = T),
      area_perc_NGRDI = sum(!is.na(idx.NGRDI.df)) / dim(idx.NGRDI.df)[1] *100,
      area_perc_ExGI = sum(!is.na(idx.ExGI.df)) / dim(idx.ExGI.df)[1] *100
            ))  
}

head(output)
tail(output)
cor(output$area_perc_NGRDI, output$area_perc_ExGI)


########################################
# Counting the number of objects - stand
########################################

#importing mosaic
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
V3plots <- plot_clip(mosaic_V3, plots_rep1)

# Removing soil/background using Otsu method to determine the threshold based on histogram of pixel values
EX2 <- V3plots[[360]]
plotRGB(EX2)

# First, we let's calculate EXG index (Excess Green Index)
ExGI <- overlay(x = EX2,
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

# lets' check it
plot(ExGI)

# obtain the threshold for this plot
thr <- EBImage::otsu(x = raster::as.matrix(ExGI),
            range = c(min(raster::as.matrix(ExGI), na.rm = TRUE),
                      max(raster::as.matrix(ExGI), na.rm = TRUE)
            ),
            levels = 256)

thr

# creating a mask using the Otsu value
soilMask  <- ExGI > thr

# removing the soil
plotXMasked <- mask(x = EX2, mask = soilMask, maskvalue = FALSE)
plotRGB(plotXMasked)

# saving rasters
writeRaster(EX2, filename=file.path("plotX.grd"), overwrite = TRUE, format="raster", options="INTERLEAVE=BAND")
writeRaster(soilMask, filename=file.path("plotXMasked.grd"), overwrite = TRUE, format="raster")

# saving the masked plot as image in a temp dir
temp <- tempdir()
png(file.path(temp, "plotX.png"), height=nrow(plotXMasked), width=ncol(plotXMasked)) 
plotRGB(plotXMasked, maxpixels=ncell(plotXMasked))
dev.off()

# loading again
img <- pliman::image_import(file.path(temp, "plotX.png"))
plot(img)
out <- pliman::analyze_objects(img, marker = "id")$results
head(out)

# defining a thr for object size
sort(out$area, decreasing = T)[1:20]
thr.count <- sort(out$area, decreasing = T)[20]
out <- out[out$area > thr.count,]
nrow(out) # number of objects in the plot

################################### applying for all plots ##############################
system.time(  
results.st <- foreach(i = 1:length(V3plots), 
                          .packages = c("raster", "pliman"), 
                          .combine = "rbind",
#                          .export = c("mask","overlay"),
                          .multicombine = TRUE, 
                          .errorhandling = "remove",
                          .verbose = TRUE    
    ) %dopar% {
  
  EX2 <- V3plots[[i]]
  ExGI <- overlay(x = EX2,
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
  thr <- EBImage::otsu(x = raster::as.matrix(ExGI),
              range = c(min(raster::as.matrix(ExGI), na.rm = TRUE),
                        max(raster::as.matrix(ExGI), na.rm = TRUE)
              ),
              levels = 256)
  soilMask  <- ExGI > thr
  plotXMasked <- mask(x = EX2, mask = soilMask, maskvalue = FALSE)
  temp <- tempdir()
  png(file.path(temp, "plotX.png"), height=nrow(plotXMasked), width=ncol(plotXMasked)) 
  plotRGB(plotXMasked, maxpixels=ncell(plotXMasked))
  dev.off()
  img <- pliman::image_import(file.path(temp, "plotX.png"))
  out <- pliman::analyze_objects(img, marker = "id", verbose = F, plot = F)$results
  out <- out[out$area > thr.count,]
  
  output <- data.frame(
    C_R = plots_rep1@data[i, "C_R"],        
    Count = nrow(out),
    plot_area_px = dim(img)[1] * dim(img)[2],
    canopy_area = sum(out$area),
    canopy_area_perc = sum(out$area) / (dim(img)[1] * dim(img)[2]) *100
    
  )  
 }
)

dim(results.st)
head(results.st)
tail(results.st)

###############################
# plant Height via cloud points
###############################

#importing cloud points
c60DAS <- fread("../../datasets/cloudpoints/60_DAS_CP.txt") 
colnames(c60DAS) <- c("X","Y", "Z")
head(c60DAS)

c0DAS <- fread("../../datasets/cloudpoints/0_DAS_CP.txt") 
colnames(c0DAS) <- c("X","Y", "Z")
head(c0DAS)

# creating LAS files
write.las(file = (file.path(getwd(), "60.laz")), header = header_create(c60DAS), data = c60DAS)
write.las(file = (file.path(getwd(), "0.laz")), header = header_create(c0DAS), data = c0DAS)

# loading the LAS files
las60 <- readLAS(files = "60.laz", select = "xyz")
las0 <- readLAS(files = "0.laz", select = "xyz")
plot(las60)
plot(las0)
dev.off()

# importing shapefiles (plots)
plots_rep1 <- shapefile("../../datasets/plots/PI_R1_DAS.shp")
print(plots_rep1)
plot(plots_rep1, 
     #add = T, 
     col = "Red")

## Auxiliary function for clipping point cloud by shapefile (features)
clipper.cloud <- function(cloud, shape){
  pc <- list()
  for(i in 1:nrow(shape)){
    p <- shape[i,]
    c <- clip_rectangle(cloud, xleft = p@bbox['x','min'], ytop = p@bbox['y','max'], xright = p@bbox['x','max'], ybottom = p@bbox['y','min'])
    if(!is.null(c)){
      pc[[i]] <- c
      names(pc)[i] <- paste0(shape$row[i],"_",shape$col[i])}}
  pc <- pc[!unlist(lapply(pc, is.null))]
  return(pc)
}

system.time( # it takes 1 minutes
plot.cloud60 <- clipper.cloud(las60, plots_rep1) #clip cloud for plot canopy height
)

plot.cloud0 <- clipper.cloud(las0, plots_rep1) #clip cloud for plot soil level


#applying a percentile to canopy height
p90.60 <- lapply(plot.cloud60, function(x) { quantile(x@data$Z, .90) }) #90th percentile
p90.60$`40_1`

# visualizing shapes
plot(plot.cloud60$`40_1`@data$Y, plot.cloud60$`40_1`@data$X) # nadir seeing 
plot(plot.cloud60$`40_1`@data$Y, plot.cloud60$`40_1`@data$Z) #vista frontal
plot(plot.cloud60$`40_1`@data$X, plot.cloud60$`40_1`@data$Z) #vista lateral
abline(h = p90.60$`40_1`, col = "red")

#applying percentile to soil elevation
p50.0 <- lapply(plot.cloud0, function(x) { quantile(x@data$Z, .5) }) #50th percentile
p50.0$`40_1`
plot(plot.cloud0$`40_1`@data$Y, plot.cloud0$`40_1`@data$Z) #vista lateral
abline(h = p50.0$`40_1`, col = "red")

#estimating plant height for all plots
PH <- data.frame()

for (i in 1:length(p90.60)) {
  cat("plot", i, "\n")
  PH <- rbind(PH, data.frame(
    C_R = names(p90.60)[[i]],        
    Height = round(as.numeric(p90.60[[i]]) - as.numeric(p50.0[[i]]), 2)
  ))  
}

head(PH)
tail(PH)

# It is possible to estimate indices for each point in the cloud when you have the band values. 
# Then, we can trim it using the mask threshold to eliminate points regarding tassels or soil, for instance.
# Finally, this cleaning can improve the accuracy and correlation between PH_real and PH_imagery 

##### The End #######