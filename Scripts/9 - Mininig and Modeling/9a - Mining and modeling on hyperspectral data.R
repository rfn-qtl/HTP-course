###################################
# HTP in PB
# Data mining on hyperespectral data
# Author: Roberto Fritsche Neto
# E-mail: rfneto@agcenter.lsu.edu
# Last update: Jun 24, 2023 
###################################

# libraries
library(superheat)
library(caret)
library(caretEnsemble)
library(h2o)

# setting the dir 
setwd("../../datasets/hyperspectral")

# load pheno
pheno <- read.csv("pheno.csv")
dim(pheno)
head(pheno)
tail(pheno)

# load imagery data file
hyper <- read.table("hyper.txt")
dim(hyper)
head(hyper)

# matching the information
pheno <- pheno[pheno$id %in% rownames(hyper), ]
hyper <- hyper[rownames(hyper) %in% pheno$id, ]
dim(hyper);dim(pheno)

# Double check
all(rownames(hyper) %in% pheno$id)
all(pheno$id %in% rownames(hyper))
all(pheno$id == rownames(hyper))

## Visualizing the relationship between wavebands
superheat(cor(hyper), pretty.order.rows = T, pretty.order.cols = T, col.dendrogram = T, clustering.method = "kmeans", dist.method = "euclidean",  bottom.label.text.size = 1, left.label.text.size = 1, legend.text.size = 5, title = "Wavebands")

# apply a PCA - center and scale = TRUE is highly recommended
hyper.pca <- prcomp(hyper, center = TRUE, scale. = TRUE) 
var.pca <- summary(hyper.pca)$importance[, 1:8] 
print(var.pca) # the first 8 == 99%

# proportion explained
plot(var.pca[3,]*100, type = "l", xlab = "Number of PCA", ylab = "% explained", col = "red")

# PCA graph
plot(hyper.pca$x[,1], hyper.pca$x[,2], col = rainbow(150), xlab = paste("PCA 1 = ", round(summary(hyper.pca)$importance[2,1]*100), "%"), ylab = paste("PCA 2 = ", round(summary(hyper.pca)$importance[2,2]*100), "%"))

##################################
# selecting attributes - features
##################################

######## Correlation - eliminating those bands almost perfected correlated
highlyCorrelated <- findCorrelation(cor(hyper), cutoff = 0.98) # more than 98%
print(highlyCorrelated)
hyper.cor <- hyper[,-highlyCorrelated]
dim(hyper.cor)
head(hyper.cor)

######### Recursive Feature Selection (RFE) - Backwards Feature Selection
# data for classification
data.IA.cls <- cbind(as.factor(pheno[, 2]), data.frame(apply(scale(hyper.cor), 2, as.numeric)))
colnames(data.IA.cls)[1] <- "INOC"
head(data.IA.cls)
str(data.IA.cls)
dim(data.IA.cls)

# data for regression
data.IA.reg <- cbind(pheno[, 3], data.frame(apply(scale(hyper.cor), 2, as.numeric)))
colnames(data.IA.reg)[1] <- "DMLL"
head(data.IA.reg)
str(data.IA.reg)
dim(data.IA.reg)

### The best wavebands for INOC - 
# "Accuracy" and "Kappa" metrics for classification
set.seed(29121983)

# Design the cross-validation system 
control <- rfeControl(functions = rfFuncs, method = "repeatedcv", number = 5, repeats = 5)

# running the model
opt.cls <- rfe(x = data.IA.cls[, 2:ncol(data.IA.cls)], 
               y = data.IA.cls[, 1], 
               sizes = 1:dim(hyper.cor)[2], 
               metric = "Accuracy", 
               maximize = TRUE, 
               rfeControl = control)

opt.cls
opt.cls$bestSubset
(bands.cls <- opt.cls$optVariables)


### The best wavebands for "dry mass of last leaf" (DMLL)
# "Rsquared" metric for regressions
set.seed(29121983)
opt.reg <- rfe(x = data.IA.reg[, 2:ncol(data.IA.reg)], 
               y = data.IA.reg[, 1], 
               sizes = 1:dim(hyper.cor)[2], 
               metric = "Rsquared", 
               maximize = TRUE, 
               rfeControl = control)

opt.reg
predictors(opt.reg)
opt.reg$bestSubset
(bands.reg <- opt.reg$optVariables)

# Cleaning the datasets for further analyses
data.IA.cls <- data.IA.cls[, c("INOC", bands.cls)]
head(data.IA.cls)
dim(data.IA.cls)

data.IA.reg <- data.IA.reg[,c("DMLL", bands.reg)]
head(data.IA.reg)
dim(data.IA.reg)

#######################################
# Modeling (model selection) - Classify
#######################################
# Build the training/validate/test sets
trainIndex <- createDataPartition(data.IA.cls[, 1], p = 0.75, list = FALSE)
train <- data.IA.cls[ trainIndex,]
test <- data.IA.cls[-trainIndex,]

#### many models at the same time
# k-fold cross validation
trainControl <- trainControl(method = "repeatedcv", 
                             number = 5, 
                             repeats = 5,
                             classProbs = TRUE, 
                             savePredictions = TRUE, 
                             index = createFolds(train[, 1]))

prop.table(table(train[, 1]))

# Overview of algorithms supported by caret
names(getModelInfo())
# List of algorithms that will be tested
algorithmList <- c('rpart', 'rf', 'knn', 'svmRadial')

# Create models in a list
models <- caretList(INOC ~ ., data = train, trControl = trainControl, methodList = algorithmList)

results <- resamples(models)
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales = scales)

###### tuning the best model ########
# Create index to split based on labels  
index <- createDataPartition(data.IA.cls$INOC, p = 0.75, list = FALSE)
# Subset training set with index
training <- data.IA.cls[index,]
# Subset test set with index
test <- data.IA.cls[-index,]

# Train a model
set.seed(29121983)

trainControl <- trainControl(method = "repeatedcv", 
                             number = 5, 
                             repeats = 5,
                             classProbs = TRUE, 
                             savePredictions = TRUE, 
                             index = createFolds(train[, 1]))

model_best <- train(x = training[, 2:ncol(training)], 
                   metric = "Accuracy",
                   y = training[, 1],
                   maximize = TRUE,
                   method = 'rf',
                   tuneLength = dim(training)[2]-1, 
                   trControl = trainControl)

plot(model_best)
print(model_best$bestTune)

# Predict the labels of the test set
predictions <- predict(object = model_best, test[, 2:ncol(test)])

plot(test[,1], predict(model_best, test[, 2:ncol(test)]))

# Evaluate the predictions
table(predictions)

# Confusion matrix 
conf.matrix <- confusionMatrix(predictions, test[, 1])
conf.matrix$table
conf.matrix$overall

#######################################
# Modeling (model selection) - Regression
#######################################

# Build the training/validate/test sets
trainIndex <- createDataPartition(data.IA.reg[, 1], p = 0.75, list = FALSE)
train <- data.IA.reg[ trainIndex,]
test <- data.IA.reg[-trainIndex,]

#### many models at the same time
# k-fold cross validation
trainControl <- trainControl(method = "repeatedcv", 
                             number = 5, 
                             repeats = 5,
                             classProbs = F, 
                             savePredictions = TRUE, 
                             index = createFolds(train[, 1]))

# List of algorithms that will be tested
algorithmList <- c('rpart', 'rf', 'knn', 'svmRadial')

# Create models in a list
models <- caretList(DMLL ~ ., data = train, trControl = trainControl, methodList = algorithmList)

results <- resamples(models)
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales = scales)


############ tuning the random forest model ##############

# Create index to split based on labels  
index <- createDataPartition(data.IA.reg$DMLL, p = 0.75, list = FALSE)
# Subset training set with index
training <- data.IA.reg[index,]
# Subset test set with index
test <- data.IA.reg[-index,]

trainControl <- trainControl(method = "repeatedcv", 
                             number = 5, 
                             repeats = 5, 
                             search = "grid")

tunegrid <- expand.grid(.mtry=c(1:(dim(training)-1)[2]))

# Train a model
set.seed(29121983)

model_rf <- train(x = training[, 2:ncol(training)], 
                   metric = "Rsquared",
                   y = training[, 1],
                   maximize = TRUE,
                   method = 'rf',
                  tuneGrid = tunegrid,
                  trControl = trainControl)

print(model_rf)
print(model_rf$bestTune)
plot(model_rf)
model_rf$resample
model_rf$finalModel

# Predict the labels of the test set
predictions <- predict(object = model_rf, test[, 2:ncol(test)])

# Accuracy
plot(test[,1], predict(model_rf, test[, 2:ncol(test)]), ylab = "predicted", xlab = "observed", col = "blue")
abline(lm(predict(model_rf, test[, 2:ncol(test)])~test[,1]), col = "red")
cor(test[,1], predict(model_rf, test[, 2:ncol(test)]))

################################# Ensemble via stacking models #####################################
# combine models to improve predictions
# Apparently, there is no high correlated models (> 70%)
modelCor(results)
splom(results)

# to combine (stack) we will use RF
stackControl <- trainControl(method = "repeatedcv", 
                             number = 4, 
                             repeats = 4, 
                             savePredictions = TRUE, 
                             classProbs = F)

stack.rf <- caretStack(models, 
                       method = "rf" , 
                       metric = "Rsquared", 
                       trControl = stackControl) # it takes 6-7 minutes

print(stack.rf)
(w.models <- caretEnsemble(models))
summary(w.models)

# Accuracy of the stack model
predictions2 <- predict(stack.rf, test[, 2:ncol(test)])
cor(test[,1], predict(stack.rf, test[, 2:ncol(test)]))

# saving the best model
saveRDS(stack.rf, "./rf_model.rds")

# load the model
model_saved <- readRDS("./rf_model.rds")

# predict using the saved model
predictions3 <- predict(model_saved, test[, 2:ncol(test)])

#check equality of predictions
all.equal(predictions2, predictions3)

######### the end ############