########################
# HTP in PB
# Image matrix practice
# Author: Roberto Fritsche Neto
# E-mail: rfneto@agcenter.lsu.edu
# Last update: Jun 24, 2023 
########################

# Libraries
require(imager)
require(raster)
require(magick)

# loading and plotting and image
lena <- load.image("lena.jpg")
plot(lena)
diploid <- load.image("D_330.jpg")
plot(diploid)

# How images are represented
dim(lena)

# The four dimensions are labeled x,y,z,c. The first two are the usual spatial dimensions, the third one usually correspond to depth or time, and the fourth one is color
dim(diploid)

# split and see the channels
redPal <- colorRampPalette(c("black", "red"))
greenPal <- colorRampPalette(c("black", "green"))
bluePal <- colorRampPalette(c("black", "blue"))

diploid[,,1][1:5, 1:5]
diploid[,,2][1:5, 1:5]
diploid[,,3][1:5, 1:5]

par(mfrow = c(2,2))
plot(diploid)
image(diploid[,,1], col= redPal(256)) # red
image(diploid[,,2], col= greenPal(256)) # green
image(diploid[,,3], col= bluePal(256)) # blue
dev.off()

# reducing the number of channels to one
gray.seed <- grayscale(diploid)
dim(gray.seed)
plot(gray.seed)

# lets see the channels as matrices
plot(lena)
lena[,,,1][1:5, 1:5] # just one channel
diploid[,,,3][1:5, 1:5] # third channel
gray.seed[,,,1][1:5, 1:5]
gray.seed[,,,1][1:5, 1:5] * 255 # adjusting the range
head(as.data.frame(lena)) # as data frame

# dimensions
dim(as.matrix(gray.seed))
as.matrix(gray.seed)[1:5, 1:5]

# differentiate images by histograms
par(mfrow = c(2,2))
plot(as.raster(lena))
plot(as.raster(gray.seed))
imcol(R(lena),10) %>% plot(main = "Lena grayscale", type="l")
imcol(R(gray.seed),10) %>% plot(main = "Seed grayscale", type="l")

# or see an image by its channels
par(mfrow = c(2,2))
plot(diploid)
imcol(R(diploid), 10) %>% plot(main = "Red channel along 10th row", type = "l", col = "red")
imcol(G(diploid), 10) %>% plot(main= "Green channel along 10th line", type= "l", col = "green")
imcol(B(diploid), 10) %>% plot(main= "Blue channel along 10th line", type= "l", col = "blue")

# Transformations
lena.log <- lena
lena.log[,,,1] <- log(lena[,,,1])
lena.sqrt <- lena
lena.sqrt[,,,1] <- sqrt(lena[,,,1])
lena.exp <- lena
lena.exp[,,,1] <- exp(lena[,,,1])

# add or subtract
adj.raster <- function(x, taxa){
  xadj <- x
  temp1 <- x[,,,1] + taxa
  temp1[temp1 > 1] <- 1
  temp1[temp1 < 0] <- 0
  xadj[,,,1] <- temp1
  return(xadj)
}

darker.lena <- adj.raster(lena, -0.4)
lighter.lena <- adj.raster(lena, 0.4)
range(lena[,,,1])
range(darker.lena[,,,1])
range(lighter.lena[,,,1])

par(mfrow = c(2,3))
plot(lena)
plot(lena.log, main = "log")
plot(lena.sqrt, main = "sqrt")
plot(lena.exp, main = "exp")
plot(darker.lena, main = "darker")
plot(lighter.lena, main = "lighter")
dev.off()

# Filters - corrections or reductions without loss of information
einsten <- load.image("einstein.jpg")
plot(einsten)
dim(einsten)

filter.median <- function(x) {
  xadj <- x
  temp1 <- x[,,,1]
  
  for (i in 1:nrow(temp1)) {
    cat(i)
    for (j in 1:ncol(temp1)) {

      if (i > 1 & j > 1 & i < nrow(temp1) & j < ncol(temp1)) {              
       neighbor <- c(temp1[i-1, j-1],
                     temp1[i-1, j],
                     temp1[i-1, j+1],
                     temp1[i, j-1],
                     temp1[i, j],
                     temp1[i, j+1],
                     temp1[i+1, j-1],
                     temp1[i+1, j],
                     temp1[i+1, j+1])
}
      else {neighbor <- temp1[i, j]}
      
      newij <- median(neighbor, na.rm = TRUE) 
      temp1[i,j] <- newij
    }
  }
  xadj[,,,1] <- temp1
  return(xadj)
}

einsten.median <- filter.median(einsten)

par(mfrow = c(1,2))
plot(einsten)
plot(grayscale(einsten.median))

# Blurry filter based on median
blur.ein <- medianblur(einsten, 5)
plot(einsten)
plot(blur.ein)
dev.off()

# saving our newest image
imager::save.image(blur.ein, "blur.ein.jpg")

# rotate
imrotate(blur.ein, 30) %>% plot(main = "Rotating")

# re-size resolution
res.eins <- resize(blur.ein, round(width(blur.ein) / 10), round(height(blur.ein) / 10))
dim(res.eins)
plot(res.eins, main = "Thumbnail") #Pixellated 

#Edge detector along x-axis
im.xedges <- deriche(gray.seed, 2, order = 1, axis = "x") 
plot(im.xedges)

#Edge detector along y-axis
im.yedges <- deriche(gray.seed, 2, order = 1, axis = "y") 
plot(im.yedges)
dev.off()

# Individual pixels can be accessed using at and color.at:
plot(diploid)
at(diploid, x = 200, y = 120, cc = 1:3)
at(diploid, x = 200, y = 300, cc = 1:3)
color.at(diploid, x = 20, y = 20)

# another way to manage images
diploid2 <- image_read("D_330.jpg")
print(diploid2)

# converting format
seed_png <- image_convert(diploid2, "png")
image_info(seed_png)
print(seed_png)

# another kind of filter
image_charcoal(diploid2, sigma = .9)
?image_charcoal
# Kernel convolution
# The image_convolve() function applies a kernel over the image. Kernel convolution means that each pixel value is recalculated using the weighted neighborhood sum defined in the kernel matrix. For example lets look at this simple kernel:

kern <- matrix(0, ncol = 3, nrow = 3)
kern[1, 2] <- 0.25
kern[2, c(1, 3)] <- 0.25
kern[3, 2] <- 0.25
kern

# This kernel changes each pixel to the mean of its horizontal and vertical neighboring pixels, which results in a slight blurring effect in the right-hand image below:
img <- image_resize(diploid2, "300x300")
?image_convolve
img_blurred <- image_convolve(img, kern)
image_append(c(img, img_blurred))

# Or use any of the standard kernels
img_sobel <- image_convolve(img, 'Sobel')
image_append(c(img, img_sobel))
dev.off()

# pixel composition - help to build masks
# load images as mosaics
diploid3 <- stack("D_330.jpg")
print(diploid3)
plot(diploid3) #plot all
plot(diploid3, 1) #plot only band 1
plotRGB(diploid3) #plot composite image
click(diploid3) #plot composite image - pixel composition

###### The End #########