###################################
# HTP in PB
# Deep learning on raw imagery
# Author: Roberto Fritsche Neto
# E-mail: rfneto@agcenter.lsu.edu
# Last update: Jun 24, 2023 
###################################

#https://tensorflow.rstudio.com/install/

# Libraries
library(reticulate) 
use_condaenv('conda_env', required=T) # only for Mac
py_config() # only for Mac
library(Cairo)
library(keras)
library(tensorflow)
require(imager)
library(abind)
library(ggplot2)
library(data.table)

######## loading images as arrays
setwd("../../datasets/haploidseed")
# all images must be in the same folder, without sub directories
# To smooth the job I suggest using files names as follows "D_103.jpg", which is is composed by CLASS_NUMBER.FORMAT
head(dir())
tail(dir())
length(dir())
img.in.dir <- dir()[dir() %like% "jpg"] # picking only files that are images

# a loop to create the X array - combine the RGB channels (matrix) from all images
X <- list()
for (i in 1:length(img.in.dir)) {
  X[[i]]  <- as.array(load.image(img.in.dir[i])[,,1,])
}

X <- abind(X, along = 0)
dim(X)
class(X)
X[1, 1:5, 1:5, 1]
X[1, 1:5, 1:5, 2]
X[1, 1:5, 1:5, 3]

# another loop for Y as classes
Y <- c()
for (i in 1:length(img.in.dir)) {
  Y  <- c(Y, strsplit(img.in.dir[i], "_", fixed = T)[[1]][1])
}

Y <- as.matrix(Y)
head(Y)
tail(Y)
dim(Y)
table(Y)
(classes <- unique(Y))
Y <- as.matrix(as.numeric(as.factor(Y))) - 1 # python starts the counting on zero
head(Y)
tail(Y)
dim(Y)
table(Y)
(classes <- unique(Y)) # as digits or numeric

# plot a sample of the imagery
set.seed(29121983)
s <- sample(1:length(dir()), 9)
par(mfrow = c(3,3))
for (i in 1:length(s)) {
  img <- load.image(dir()[s[i]])
  plot(implot(img, text(250, 30, Y[s[i]], cex = 3, col = "white")))  
}
dev.off()

# split X and Y into train and test sets 
set.seed(29121983)
index <- rep(sample(c(TRUE, FALSE), 150, replace = TRUE, prob = c(0.75, 0.25)), 3)
sum(index)/length(index)

Y_train <- Y[index]
Y_test <- Y[!index]
X_train <- X[index,,,]
dim(X_train)
X_test <- X[!index,,,]
dim(X_test)
test = list(x = X_test, y = Y_test)
rm(X)
ls()

###################### Build a CNN model ######################

model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 16, kernel_size = c(3,3), activation = "relu", 
                input_shape = c(500, 500, 3)) %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2,2))
  
# The lines of code above define the convolutional base using a common pattern: a stack of Conv2D and MaxPooling2D layers.
# As input, a CNN takes tensors of shape (image_height, image_width, color_channels), ignoring the batch size. If you are new to these dimensions, color_channels refers to (R,G,B). In this example, you will configure our CNN to process inputs of shape (500, 500, 3). You can do this by passing the argument input_shape to our first layer.
# filters =  dimensionality of the output space 
# kernel_size	= An integer or list of 2 integers, specifying the width and height of the 2D convolution window.
# pool_size = factors by which to downscale (vertical, horizontal) will halve the input in both spatial dimension
# Above, you can see that the output of every Conv2D and MaxPooling2D layer is a 3D tensor of shape (height, width, channels). The width and height dimensions tend to shrink as you go deeper in the network. The number of output channels for each Conv2D layer is controlled by the first argument (e.g., 64). Typically, as the width and height shrink, you can afford (computationally) to add more output channels in each Conv2D layer.

summary(model)

########################## Add Dense layers on top ####################################

model %>% 
  layer_flatten() %>% 
  layer_dense(units = 512, activation = "relu") %>%
                      #number of output channels
  layer_dropout(0.3) %>% #avoid overfitting
  layer_dense(units = 512, activation = "relu") %>% 
  layer_dropout(0.3) %>%
  layer_dense(units = 3, activation = "sigmoid")

# To complete our model, you will feed the last output tensor from the convolutional base (of shape (3, 3, 64)) into one or more Dense layers to perform classification. Dense layers take vectors as input (which are 1D), while the current output is a 3D tensor. First, you will flatten (or unroll) the 3D output to 1D, then add one or more Dense layers on top. Our dataset has 3 output classes, so you use a final Dense layer with 3 outputs and a softmax activation.
# layer_flatten, transforms the format of the images from a 2d-array (of 500 by 500 pixels), to a 1d-array of = 250000 pixels 
#As you can see, our (3, 3, 64) filters outputs were flattened into vectors of shape (576) before going through two Dense layers.
# layer_dense = dimensionality of the output space, in the last case the number of classes
# dropout = the most effective and most commonly used regularization techniques for neural networks
# these layers are densely-connected, or fully-connected, neural layers. The first dense layer has 500 nodes (or neurons). The second (and last) layer is a 3-node softmax layer —this returns an array of 3 probability scores that sum to 1. Each node contains a score that indicates the probability that the current image belongs to one of the 3 digit classes.

summary(model)


########### Compile and train the model #########################

model %>% compile(
  optimizer = "adam",
  loss = "sparse_categorical_crossentropy",
  metrics = "accuracy"
)

# Loss function — This measures how accurate the model is during training. We want to minimize this function to “steer” the model in the right direction.
# Optimizer — This is how the model is updated based on the data it sees and its loss function.
# Metrics —Used to monitor the training and testing steps. The following example uses accuracy, the fraction of the images that are correctly classified.

history <- model %>% 
  fit(
    x = X_train, y = Y_train,
    epochs = 5,
    validation_data = unname(test),
    verbose = 2
  )
# verbose	= (0 = silent, 1 = progress bar, 2 = one line per epoch).
# Number of epochs to train the model
# batch size that you specify in the code above defines the number of samples that going to be propagated through the network. Also, by doing this, you optimize efficiency because you make sure that you don’t load too many input patterns into memory at the same time.

plot(history)
# the accuracy on the test dataset is a little less than the accuracy on the training dataset. This gap between training accuracy and test accuracy is an example of overfilling. Overfilling is when a machine learning model performs worse on new data than on their training data

# Plot the model loss of the training data
plot(history$metrics$loss, main="Model Loss", xlab = "epoch", ylab="loss", col="blue", type="l")

# Plot the model loss of the test data
lines(history$metrics$val_loss, col="green")

# Add legend
legend("topright", c("train","test"), col=c("blue", "green"), lty=c(1,1))

# Plot the accuracy of the training data 
plot(history$metrics$acc, main="Model Accuracy", xlab = "epoch", ylab="accuracy", col="blue", type="l")

# Plot the accuracy of the validation data
lines(history$metrics$val_acc, col="green")

# Add Legend
legend("bottomright", c("train","test"), col=c("blue", "green"), lty=c(1,1))

# evaluating the model
scores <- evaluate(model, X_test, Y_test, verbose = 0)
print(scores)

# Predict the classes for the test data
pred.classes <- model %>% predict(X_test) %>% k_argmax(axis = -1)
pred.classes.str <- as.vector(c(pred.classes)[[1]])
pred.classes.str

# Confusion matrix
table(Y_test, pred.classes.str)

# Accuracy
sum(diag(table(Y_test, pred.classes.str))) / sum(table(Y_test, pred.classes.str))

# saving the model in a different place
save_model_tf(object = model, filepath = "../../Mining and modeling on R/model")

# export your model configuration to JSON 
model.json <- model_to_json(model)

# reload the model
reloaded_model <- load_model_tf("../../Mining and modeling on R/model")
all.equal(predict(model, X_test), predict(reloaded_model, X_test))
  
# Get model configuration
get_config(reloaded_model)

# Get layer configuration
get_layer(reloaded_model, index = 1)
get_layer(reloaded_model, index = 2)
get_layer(reloaded_model, index = 14)

# List the model's layers
reloaded_model$layers

# List the input tensors
reloaded_model$inputs

# List the output tensors
reloaded_model$outputs

################# VERY IMPORTANT !!! ############### 
# In this example, we build a model. 
# However we can use use model already published. 
# Not only use, but we can fine tune it.
# For instance, we could upload the model VGG17

#model.vgg17 <- keras::applications$vgg17(weights = "imagenet")

# from that you can add more layers, change the optmizers, etc.

############ The end #############