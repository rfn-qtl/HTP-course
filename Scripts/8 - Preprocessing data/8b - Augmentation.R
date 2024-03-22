###################################
# HTP in PB
# Imagery augmentation
# Author: Roberto Fritsche-Neto
# E-mail: rfneto@agcenter.lsu.edu
# Last update: March 22, 2024 
###################################

# Libraries
library(EBImage)
library(raster)
library(data.table)
library(foreach)
library(doParallel)

# setting the number of cores that will be used
registerDoParallel(cores = 9) # type the number of cores you want to use
getDoParWorkers()

# set the working directory
#importing DH single-shots
setwd("../../datasets/ssDH")
dir()
# read only the .png images
(imagery <- dir()[!dir() %like% ".xml"])

# loading and plotting an image as example
img <- readImage("D_1.png")
display(img, method = "raster")

# rotating images
colorMode(img) = Color
img_rotate <- EBImage::rotate(img, 180)
display(img_rotate,  method = "raster", all = TRUE)

# creating distortions
m =  matrix(c(1, -.25, 120, 0, 1, 0), nrow = 3, ncol = 2)
m
img_affine <- affine(img, m)
display(img_affine, method = "raster")


# So, let's increase or imagery dataset
# creating a new folder
dir.create("../../datasets/augmented")

# rotating and saving all images
foreach(i = 1:length(imagery), 
        .errorhandling = "remove",
        .packages = "EBImage") %dopar% {

  img.temp <- readImage(imagery[i])

    img_0 = EBImage::rotate(img.temp, 0)
  writeImage(img_0, paste("../../datasets/augmented", "/", strsplit(dir()[i], "_")[[1]][1], "_", "0", "_", strsplit(dir()[i], "_")[[1]][2], sep = ""), quality = 100)
  
    img_90 = EBImage::rotate(img.temp, 90)
  writeImage(img_90, paste("../../datasets/augmented", "/", strsplit(dir()[i], "_")[[1]][1], "_", "90", "_", strsplit(dir()[i], "_")[[1]][2], sep = ""), quality = 100)
  
  img_180 = EBImage::rotate(img.temp, 180)
  writeImage(img_180, paste("../../datasets/augmented", "/", strsplit(dir()[i], "_")[[1]][1], "_", "180", "_", strsplit(dir()[i], "_")[[1]][2], sep = ""), quality = 100)
  
  img_270 = EBImage::rotate(img.temp, 270)
  writeImage(img_270, paste("../../datasets/augmented", "/", strsplit(dir()[i], "_")[[1]][1], "_", "270", "_", strsplit(dir()[i], "_")[[1]][2], sep = ""), quality = 100)
  
}

#### The End ############