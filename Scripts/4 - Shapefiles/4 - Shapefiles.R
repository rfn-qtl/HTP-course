###################################
# HTP in PB
# Building shapefiles
# Author: Roberto Fritsche Neto
# E-mail: rfneto@agcenter.lsu.edu
# Last update: Jun 24, 2023 
###################################

# Libraries
library(UAStools) # doi: 10.3389/fpls.2020.511768
library(raster)

# The infile for plotshpcreate.R requires four columns (Additional columns are permitted but wont be utilize):
# Plot The number of each plot (numeric)
# Barcode A unique identifier for each plot (character)
# Range The range [also called row] number of each plot in the experimental grid (numeric)
# Row The row [also called column] number of each plot in the experimental grid (numeric)
# It is recommended to have repeated Barcodes and Plot numbers if there are multi-row plots (mrowplot= >1) as the plotshpcreatre function accounts for this redundancy within the function.

# First, we need to load or reate a fieldbook
fieldbook <- data.frame(
  Loc = rep("PIR", 180*2),
  Test = rep("HN", 180*2), # High N
  Plot = rep(1:180, each = 2), # two-rows plots
  Rep = rep(1, 180*2), # Rep 1
  Stock = paste0("L", rep(1:180, each = 2)),
  Barcode = paste0(rep("PIR", 180*2), "-", rep("HN", 180*2), "-", rep(1, 180*2), "-", paste0("L", rep(1:180, each = 2))),
  Range = as.numeric(rep(1:20, each = 360/20)), # 20 blocks within rep
  Row = as.numeric(rep(1:18, 20))
)

str(fieldbook)
head(fieldbook)
tail(fieldbook)
write.csv(fieldbook, "fieldbook.csv")

#importing mosaic
mosaic_VT <- stack("../../datasets/mosaics/60DAS_22_03_drone.tif")
plotRGB(mosaic_VT) #plot composite image
# The function was developed for UTM coordinates. Please convert to UTM before attempting to use it
print(mosaic_VT)

# Let's use the click function again, zoom-in and pick the coordinate of points A and B
AB <- click(mosaic_VT, n = 2, xy = TRUE)[,1:2]
# The location of "A" is specific and must lie at the bottom left corner of the first plot. 
# More specifically, within the middle of the preceding alley and in the middle of the inter-row space to the left of the first plot
AB

### Create and set a working directory to where you want the files to be saved
getwd()
dir.create("shapes", showWarnings = F)
setwd(paste0(getwd(), "/shapes"))

# Combining Multi-row Plots into a Single Polygon
shape <- plotshpcreate(A = as.numeric(AB[1,]), 
              B = as.numeric(AB[2,]),
              UTMzone = 23,
              Hemisphere = "S",
              infile = fieldbook,
              outfile="PIR_HN_R1",
              nrowplot = 2, # number of adjacent rows that constitute a plot.
              multirowind = F, # if adjacent plot rows should be combined. 
              rowspc = 0.5, # spacing of a single row
              rowbuf = 0.1, # distance removed from both sides to create a buffer.
              rangespc = 4.5, # refers to the total plot length including half alley
              rangebuf = 0.5, # distance removed from both sides of rangespc to create a buffer zone
              plotsubset = 0, # how many adjacent rows should be excluded from the shapefile.
              field = "PIR-HN-REP1",
              unit = "meter")

# importing the shapefile just created (plots)
plots_rep1 <- shapefile("../shapes/PIR-HN-REP1_PIR_HN_R1_buff.shp")
print(plots_rep1)
plot(plots_rep1, add = T, col = "White")

# all.plots <- rbind(plots_rep1, plots_rep2, ...)

#### the end ######