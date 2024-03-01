########################################
# HTP in PB
# Envirotyping
# Author: Roberto Fritsche Neto
# E-mail: rfneto@agcenter.lsu.edu
# Last update: Feb 27, 2024 
########################################

#loading packages
library(heatmaply)
require(foreach)
require(doParallel)
library(ggplot2)
require(EnvRtype)
source('https://raw.githubusercontent.com/allogamous/EnvRtype/master/Supplementary%20Source%20and%20Data/get_weather_v2')
library(SoilType)
# setting the number of cores that will be used
detectCores()
registerDoParallel(cores = detectCores()-1) # type the number of cores you want to use
getDoParWorkers()

# matching positions
met <- read.csv("LSU Rice_locations.csv")

met$Name
dim(met)
head(met)
tail(met)
period <- 2019:2023
trial <- met$Name  
grid2 <- expand.grid(trial, period)
colnames(grid2) <- c("Name", "period")
grid2 <- merge(grid2, met)
grid2$planting <- gsub("2023", "", grid2$planting, fixed = T)
grid2$harvest <- gsub("2023", "", grid2$harvest, fixed = T)
dim(grid2)
head(grid2)

system.time(
   env.data <- foreach(i = 1:nrow(grid2), 
                        .packages = c("EnvRtype"), 
                        .combine = "rbind",
                        .export = c("get_weather"),
                        .multicombine = TRUE, 
                        .errorhandling = "remove",
                        .verbose = TRUE    
  ) %dopar% {
  
    
  sample <- grid2[i,]  
    
  output <- get_weather(env.id = sample$Name,
                               country = NULL,
                               lat = sample$Lat,
                               lon = sample$Long,
                               start.day = paste0(sample$period, sample$planting),
                               end.day = paste0(sample$period, sample$harvest),
                               parallel = F)
  }  
)  
  
dim(env.data)
head(env.data)
tail(env.data)
saveRDS(env.data, "env.data")
unique(env.data$env)

# tuning for cardinal of temperature, and obtaining other traits
aux <- met[,c(1,5)] 
colnames(aux) <- c("env", "Alt")
aux2 <- merge(env.data, aux)
dim(aux2)
df.clim <- processWTH(env.data = aux2,Tbase1 = 12,Tbase2 = 24,Topt1 = 33,Topt2 = 37, Alt = aux2$Alt)
head(df.clim)
length(unique(df.clim$env))
dim(df.clim)
summary(df.clim)
sum(is.na(df.clim))

# defining stage interval (day of start), stage names to define de EC for T matrix 
(var.i = names(df.clim)[c(9:16,22:29)])
stages = c('EM_MAX.TIL',"MAX.TIL_PAN.INIT", "PAN.INIT_PRE.FLW", "PRE.FLW_FLW", "FLW_POST.FLW", "POST.FLW_MAT")
interval <- c(0, 45, 60, 75, 90, 105) # do not include the last date

EC <- W_matrix(env.data = df.clim, 
               env.id = 'env', 
               var.id = var.i, 
               statistic = 'mean',
               scale = F, 
               center = F,
               by.interval = T, 
               sd.tol = 5,
               time.window = interval, 
               names.window = stages)


dim(EC)
W <- EC
sum(W == "NaN")
W[W == "NaN"] <- 0
W <- scale(W)
dim(W)
W[, 1:4]

#########################################################################
# Soil information
#########################################################################

# running
system.time(
  SC <- foreach(i = 1:nrow(met), 
                   .packages = c("caret", "stringr"), 
                   .combine = "rbind",
                   .export = c("predict", "train", "trainControl", "str_replace_all"),
                   .multicombine = TRUE, 
                   .errorhandling = "remove",
                   .verbose = TRUE    
  ) %dopar% {
    # subset the data  
    trial.MET <- droplevels.data.frame(met[i,])
    # retrieve the data
    output <- get_soil(env.id = trial.MET$Name, 
                       lat = trial.MET$Lat, 
                       long = trial.MET$Long, 
                       max.depth = 20, 
                       isric.data = soil.data)
  }   
)

head(SC)
tail(SC)
dim(SC)

# Soil Covariates visualization
SCov <- reshape2::dcast(SC, env ~ Trait, value.var = "Predicted.Value", mean)
head(SCov)
dim(SCov)
rownames(SCov) <- SCov[,1]
SCov <- SCov[,2:ncol(SCov)]

# eliminate soil traits without any information
SCov <- SCov[, apply(SCov, 2, function(x){!all(is.na(x))})]
SCov[is.na(SCov)] <- 0
SCov <- scale(SCov)
dim(SCov)

W <- W[match(rownames(SCov), rownames(W)),]
all(rownames(W) == rownames(SCov))

EC <- cbind(W, SCov)
dim(EC)

################# Quality Control and feature selection ###############
##### Correlation - eliminating those almost perfected correlated #####
library(caret)
highlyCorrelated <- findCorrelation(cor(EC), cutoff = 0.98)
print(highlyCorrelated)
EC.clean <- EC[,-highlyCorrelated]
dim(EC.clean)
EC.clean[EC.clean == "NaN"] <- 0

# plot the EC - like environmental markers
heatmaply(EC.clean, 
          fontsize_row = 6,
          fontsize_col = 6,
          file = c("EC_heatmap.html", "EC_heatmap.png"))

# Obtain the environmental relationship matrix - ERM
kinship <- env_kernel(env.data = as.matrix(EC.clean), is.scaled = T, gaussian = T, sd.tol = 5)[[2]]
dim(kinship)
kinship
saveRDS(kinship, "W_kinship.")
heatmaply(kinship, 
          fontsize_row = 6,
          fontsize_col = 6,
          file = c("EC_kinship_heatmap.html", "EC_kinship_heatmap.png"))

# clustering
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

# or we can use this function to determine the optimal number of clusters
set.seed(123)
dev.off()
fviz_nbclust(EC.clean, 
             FUNcluster = kmeans, 
             k.max = nrow(EC.clean)-1, 
             nboot = nrow(EC.clean)/2, 
             method = "wss")

# K-Means Clustering
k <- kmeans(scale(EC.clean), centers = 3, nstart = 25)
p <- fviz_cluster(k, data = EC.clean)
p

ggsave(filename = paste0("./", 'cluster.tiff'),
       plot = p,
       device = 'tiff',
       width = 200,
       height = 200,
       units = 'mm',
       dpi = 300)

k$size
sum(k$size)

(clusters <- data.frame(Name = names(k$cluster), 
                        cluster = k$cluster))

dim(clusters)
write.table(clusters, "clusters.txt")

######## the end #################