###################################
# HTP in PB
# Naming and locating in images
# Author: Roberto Fritsche Neto
# E-mail: rfneto@agcenter.lsu.edu
# Last update: April 8, 2024 
###################################

# load the libraries
library(stringr)
library(tidyr)
library(dplyr)
library(magick)
library(keras)
library(tensorflow)

##################### pre-processing #############################
img_dir <- "../../datasets/annotation/images"
annotation <- read.csv("../../datasets/annotation/via_export_csv.csv")
head(annotation)

# functions to extract information from the annotation file
shape.attributes <- function(x){
  temp1 <- strsplit(x$region_shape_attributes, split = "\"", fixed = T)
  temp2 <- matrix(unlist(temp1), ncol = 13, byrow = T)[,c(7, 9, 11, 13)]
  temp3 <- gsub("}", "", temp2, fixed = T)
  temp4 <- gsub(":", "", temp3, fixed = T)
  temp5 <- as.data.frame(apply(gsub(",", "", temp4, fixed = T), 2, as.numeric))
  colnames(temp5) <- c("x", "y", "width", "height")
  return(temp5)
}

boxinfo <- shape.attributes(annotation)
colnames(boxinfo) <- c("x_left", "y_top", "box_width", "box_height")
head(boxinfo)

region.attributes <- function(x){
  temp1 <- strsplit(x$region_attributes, split = "\"", fixed = T)
  temp2 <- matrix(unlist(temp1), ncol = length(temp1[[1]]), byrow = T)
  temp3 <- gsub("}", "", temp2, fixed = T)
  temp4 <- gsub(",", "", temp3, fixed = T)
  temp5 <- gsub(":", "", temp4, fixed = T)
  temp6 <- gsub("{", "", temp5, fixed = T)
  temp7 <- gsub("true", T, temp6, fixed = T)
  temp8 <- apply(temp7, 1, function(x){which(x == T)})
  classes <- c()
  for(i in 1:length(temp8)){
    classes <- c(classes, temp7[i, (temp8[i]-1)])  
  }
  return(data.frame(class = as.factor(classes), class_id = as.numeric(as.factor(classes))-1))
}

regions <- region.attributes(annotation)
head(regions)
tail(regions)

# loading image file names
setwd(img_dir)
image.names <- data.frame(dir())
head(image.names)

# selecting only files that ends with .png  
image.names <- image.names %>% filter(str_detect(dir.., ".png$"))
colnames(image.names) <- "filename"
head(image.names)

# getting the imagery info
image.info <- data.frame()
for (i in 1:dim(image.names)[1]) {
  img <- image_read(image.names[i,])
  image.info <- rbind(image.info, 
                      data.frame(
                        filename = image.names[i,],
                        width =  image_info(img)[2],
                        height = image_info(img)[3]))
}

# add a column for image id, as numeric
image.info$img_id <- as.numeric(as.factor(image.info$filename))-1
colnames(image.info)[2:3] <- c("img_width", "img_height")
head(image.info)

# merge data sets
image.info <- merge(annotation[, c(1,4,5)], image.info)
head(image.info)
dim(image.info)

# then, combine everything
imagery.data <- cbind(image.info, boxinfo, regions)
head(imagery.data)
dim(imagery.data)

# For the bounding boxes, the annotation file provides x_left and y_top coordinates, as well as width and height. We will mostly be working with corner coordinates, so we create the missing x_right and y_bottom.
# As usual in image processing, the y axis starts from the top.
imagery.data$y_bottom <- imagery.data$y_top + imagery.data$box_height - 1
imagery.data$x_right <- imagery.data$x_left + imagery.data$box_width - 1
head(imagery.data)
tail(imagery.data)

# lets add a col for the images sequence
imagery.data$seq <- seq(from = 1001, to = (1000+dim(imagery.data)[1]))
head(imagery.data)

# Let’s take a glance at our data. Picking one of the early entries and displaying the original image together with the object annotation yields
img_data <- imagery.data[c(1, 26),]
img <- image_read(img_data[1,]$filename)
img <- image_draw(img)
rect(
  img_data$x_left,
  img_data$y_bottom,
  img_data$x_right,
  img_data$y_top,
  border = "white",
  lwd = 2
)
text(
  img_data$x_left,
  img_data$y_top,
  img_data$class,
  offset = 1,
  pos = 3,
  cex = 1.5,
  col = "white"
)
dev.off()

# Now, we need to crop image samples and save them in a new folder
# First, let's create a new folder
# dir.create("/Users/RFN-ESALQ-USP/Google Drive/HD Roberto/Prof Efetivo USP/Disciplinas/LGN5838 Fenotipagem de alto rendimento/datasets/annotation/images_crop")
img_dir_crop <- "../../annotation/images_crop"

# then, crop all sample images
for (i in 1:dim(imagery.data)[1]) {
  img <- image_read(imagery.data[i,]$filename)
  ext <- paste(imagery.data$box_width[i], "x", imagery.data$box_height[i], "+", imagery.data$x_left[i], "+", imagery.data$y_top[i], sep = "")
  img_crop = image_crop(img, ext)
  image_write(img_crop, path = paste(img_dir_crop, "/", imagery.data$seq[i], "_", imagery.data$class[i], ".png", sep = ""), format = "png")
}

head(dir(img_dir_crop))
length(dir(img_dir_crop)) # adjust how to sort as numeric

# replacing this filenames in our imagery data 
imagery.data$img_sample <- dir(img_dir_crop)
head(imagery.data)

# The dimensions to which all images found will be resized
max(imagery.data$box_width)
max(imagery.data$box_height)
min(imagery.data$box_width)
min(imagery.data$box_height)
target_width <- 71 # minimal number
target_height <- 71


# There’s one step that will bitterly hurt our localization performance if we later forget it, so let’s do it now already: We need to scale all bounding box coordinates according to the actual image size we’ll use when we pass it to our network
imagery.data <- imagery.data %>% mutate(
  x_left_scaled = (x_left / img_width * target_width) %>% round(),
  x_right_scaled = (x_right / img_width * target_width) %>% round(),
  y_top_scaled = (y_top / img_height * target_height) %>% round(),
  y_bottom_scaled = (y_bottom / img_height * target_height) %>% round(),
  bbox_width_scaled =  (box_width / img_width * target_width) %>% round(),
  bbox_height_scaled = (box_height / img_height * target_height) %>% round()
)


# Finally train-test split, equalizing the samples per class
dim(imagery.data)
set.seed(29121983)

equalize <- c(sample(which(imagery.data$class == "corn"), sum(imagery.data$class == "soil")), which(imagery.data$class == "soil"))
length(equalize)

imagery.data.eq <- imagery.data[equalize, ]
dim(imagery.data.eq)

set.seed(29121983)
train_indices <- sample(1:dim(imagery.data.eq)[1], 0.75*dim(imagery.data.eq)[1])
       
train_data <- imagery.data.eq[train_indices,]
dim(train_data)
sum(train_data$class == "soil")
sum(train_data$class == "corn")

validation_data <- imagery.data.eq[-train_indices,]
dim(validation_data)
sum(validation_data$class == "soil")
sum(validation_data$class == "corn")

batch_size <- 17 # the ideal is higher than 32
# The batch size is a number of samples processed before the model is updated. 
# The size of a batch must be more than or equal to one and less than or equal to the number of samples in the training data set
# It is related to many factor and a good relations is with the number of steps per epoch. 
# For the latter, 10 is good number.


##################### Single-object classification ####################

# We will use XCeption as a basic feature extractor. 
feature_extractor <-
  application_xception(
    include_top = FALSE,
    input_shape = c(target_width, target_height, 3),
    pooling = "avg"
  )

# Also, we will leave XCeption’s weights untouched.
feature_extractor %>% freeze_weights()

model.c <- keras_model_sequential() 

model.c %>%
  feature_extractor %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.20) %>%
  layer_dense(units = 512, activation = "relu") %>% #512
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.20) %>%
  layer_dense(units = 2, activation = "softmax") # number of output classes

model.c %>% 
  compile(
  optimizer = "adam",
  loss = "sparse_categorical_crossentropy",
  metrics = list("accuracy")
)

# Train and test models
train_generator <- image_data_generator(rescale = NULL)

# Note that the validation data shouldn't be augmented!
test_generator <- image_data_generator(rescale = NULL)  

# obtaining the images from the directory using the generator function
train.class <- flow_images_from_dataframe(dataframe = train_data, 
                                    directory = img_dir_crop,
                                    x_col = "img_sample", 
                                    y_col = "class",
                                    generator = train_generator, 
                                    target_size = c(target_width, target_height),
                                    color_mode = "rgb", 
                                    classes = NULL, 
                                    class_mode = "sparse",
                                    batch_size = batch_size, 
                                    shuffle = FALSE, 
                                    seed = 29121983, 
                                    save_to_dir = NULL,
                                    save_prefix = "", 
                                    save_format = "png", 
                                    interpolation = "nearest", 
                                    drop_duplicates = TRUE)

# the same for validations set
validation.class <- flow_images_from_dataframe(dataframe = validation_data, 
                                    directory = img_dir_crop,
                                    x_col = "img_sample", 
                                    y_col = "class",
                                    generator = test_generator, 
                                    target_size = c(target_width, target_height),
                                    color_mode = "rgb", 
                                    classes = NULL, 
                                    class_mode = "sparse",
                                    batch_size = batch_size, 
                                    shuffle = FALSE, 
                                    seed = 29121983, 
                                    save_to_dir = NULL,
                                    save_prefix = "", 
                                    save_format = "png", 
                                    interpolation = "nearest", 
                                    drop_duplicates = TRUE)

# Fit 
history.c <- model.c %>% fit_generator(
  train.class,
  steps_per_epoch = round(nrow(train_data) / batch_size),
  epochs = 10,
  validation_data = validation.class,
  validation_steps = round(nrow(validation_data) / batch_size)
)

# Plot 
plot(history.c)
dev.off()

# evaluate predictions
model.c %>% evaluate_generator(validation.class, steps = batch_size) 

predictions.c <- model.c %>% predict_generator(validation.class, steps = batch_size)
validation_data$class
(preds.c <- ifelse(predictions.c[,1] > 0.5, "corn", "soil"))

# confusion matrix
table(validation_data$class, ifelse(predictions.c[,1] > 0.5, "corn", "soil"))

sum(diag(table(validation_data$class, ifelse(predictions.c[,1] > 0.5, "corn", "soil")))) / sum(table(validation_data$class, ifelse(predictions.c[,1] > 0.5, "corn", "soil")))

########## Single-object localization ###################

# The question now is, how do we learn bounding boxes? It is a regression problem and aim to predict the actual coordinates. 

# Concretely, it means we’ll have a dense output layer with 4 units, each corresponding to a corner coordinate.

# For Xception, the output resolution will be 7x7. So a priori, we shouldn’t expect high precision on objects much smaller than about 10x10 pixels (assuming the standard input size of 71x71).

feature_extractor <- application_xception(
  include_top = FALSE,
  input_shape = c(target_width, target_height, 3)
)

feature_extractor %>% freeze_weights()
# Now we append our custom regression module.

model.l <- keras_model_sequential() %>%
  feature_extractor %>%
  layer_flatten() %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.20) %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.20) %>%
  layer_dense(units = 4) # four corners

# We will train with one of the loss functions common in regression tasks, mean absolute error. But in tasks like object detection or segmentation, we’re also interested in a more tangible quantity: How much do estimate and ground truth overlap?
  
#  Overlap is usually measured as Intersection over Union, or Jaccard distance. Intersection over Union is exactly what it says, a ratio between space shared by the objects and space occupied when we take them together.

# To assess the model’s progress, we can easily code this as a custom metric:

metric_iou <- function(y_true, y_pred) {
  
  # order is [x_left, y_top, x_right, y_bottom]
  intersection_xmin <- k_maximum(y_true[ ,1], y_pred[ ,1])
  intersection_ymin <- k_maximum(y_true[ ,2], y_pred[ ,2])
  intersection_xmax <- k_minimum(y_true[ ,3], y_pred[ ,3])
  intersection_ymax <- k_minimum(y_true[ ,4], y_pred[ ,4])
  
  area_intersection <- (intersection_xmax - intersection_xmin) * 
    (intersection_ymax - intersection_ymin)
  area_y <- (y_true[ ,3] - y_true[ ,1]) * (y_true[ ,4] - y_true[ ,2])
  area_yhat <- (y_pred[ ,3] - y_pred[ ,1]) * (y_pred[ ,4] - y_pred[ ,2])
  area_union <- area_y + area_yhat - area_intersection
  
  iou <- area_intersection/area_union
  k_mean(iou)
  
}


# Model compilation then goes like
model.l %>% compile(
  optimizer = "adam",
  loss = "mae",
  metrics = list("accuracy", custom_metric("iou", metric_iou))
)


# obtaining the images from the directory using the generator function
# Train model
train_generator <- image_data_generator(rescale = NULL)

# Note that the validation data shouldn't be augmented!
test_generator <- image_data_generator(rescale = NULL)  

# Now modify the generator to return bounding box coordinates as targets…
train.loc <- flow_images_from_dataframe(dataframe = train_data, 
                                    directory = img_dir_crop,
                                    x_col = "img_sample", 
                                    y_col = c("x_left_scaled", 
                                              "y_top_scaled", 
                                              "x_right_scaled", 
                                              "y_bottom_scaled"),
                                    generator = train_generator, 
                                    target_size = c(target_width, target_height),
                                    color_mode = "rgb", 
                                    classes = NULL, 
                                    class_mode = "other",
                                    batch_size = batch_size, 
                                    shuffle = FALSE, 
                                    seed = 29121983, 
                                    save_to_dir = NULL,
                                    save_prefix = "", 
                                    save_format = "png", 
                                    interpolation = "nearest", 
                                    drop_duplicates = TRUE)


# the same for validations set
validation.loc <- flow_images_from_dataframe(dataframe = validation_data, 
                                               directory = img_dir_crop,
                                               x_col = "img_sample", 
                                                   y_col = c("x_left_scaled", 
                                                             "y_top_scaled", 
                                                             "x_right_scaled", 
                                                             "y_bottom_scaled"),
                                               generator = test_generator, 
                                               target_size = c(target_width, target_height),
                                               color_mode = "rgb", 
                                               classes = NULL, 
                                               class_mode = "other",
                                               batch_size = batch_size, 
                                               shuffle = FALSE, 
                                               seed = 29121983, 
                                               save_to_dir = NULL,
                                               save_prefix = "", 
                                               save_format = "png", 
                                               interpolation = "nearest", 
                                               drop_duplicates = TRUE)


#… and we’re ready to go!
history.l <- model.l %>% fit_generator( 
    train.loc,
    epochs = 10,
    steps_per_epoch = nrow(train_data) / batch_size,
    validation_data = validation.loc,
    validation_steps = nrow(validation_data) / batch_size
    )

# Plot 
plot(history.l)
dev.off()
  
# To learn more about how training went, we need to see some predictions
# Thus, let’s see predictions on the base images from the validation set
load_and_preprocess_image <- function(img_sample) {
    img_array <- image_load(file.path(img_dir_crop, img_sample),
                            target_size = c(target_width, target_height)) %>% 
                            image_to_array() %>%     
                            xception_preprocess_input() 
    dim(img_array) <- c(1, dim(img_array))
    img_array
  }
  
# selecting the first sample image as example  
double.check <- validation_data[1, ]

#define the dir where the samples are
setwd(img_dir_crop)

# and predict the relative position
preds <- model.l %>% predict(
                      load_and_preprocess_image(double.check[, "img_sample"]),
                      batch_size = 1
                      )

preds

# then, rescale "preds" to the actual location in the base image
rescale.preds <- preds
rescale.preds[1] <- rescale.preds[1] + double.check$img_width * double.check$x_left_scaled / target_width
rescale.preds[2] <- rescale.preds[2] + double.check$img_height * double.check$y_top_scaled / target_height
rescale.preds[3] <- rescale.preds[3] + double.check$img_width * double.check$x_right_scaled / target_width
rescale.preds[4] <- rescale.preds[4] + double.check$img_height * double.check$y_bottom_scaled / target_height

# Load the base image
img <- image_read(file.path(img_dir, double.check$filename))
img <- image_draw(img)

# Draw the real annotation
rect(
  double.check$x_left,
  double.check$y_bottom,
  double.check$x_right,
  double.check$y_top,
  border = "white",
  lwd = 2
)
text(
  double.check$x_left,
  double.check$y_top,
  double.check$class,
  offset = 1,
  pos = 3,
  cex = 1.5,
  col = "white"
)

# Then, the predictions
rect(
  rescale.preds[1],
  rescale.preds[2],
  rescale.preds[3],
  rescale.preds[4],
  border = "blue",
  lwd = 2
)
text(
  rescale.preds[1],
  rescale.preds[2],
  preds.c[1],
  offset = 1,
  pos = 4,
  cex = 1.5,
  col = "blue"
)

################# the end #############################