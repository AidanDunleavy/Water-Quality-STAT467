## Importing Data
dataset <- read.csv("lib/WaterQualityData.csv")
head(dataset,40)
str(dataset)

## Converting "<0.0070" to Nitrate + Nitrite column min = 0
dataset$Nitrate...Nitrite = ifelse(dataset$Nitrate...Nitrite == "<0.0070",
                                0,dataset$Nitrate...Nitrite)

## Converting Nitrate + Nitrite from type char to num
dataset$Nitrate...Nitrite <- as.numeric(dataset$Nitrate...Nitrite)

## Converting Conductivity from type int to num
dataset$Conductivity <- as.numeric(dataset$Conductivity)

## Subetting Data to omit columns where all values are missing
dataset=subset(dataset, dataset$pH != "NA")

## Replacing NAs with column average for each column
dataset$Temp = ifelse(is.na(dataset$Temp),
                   ave(dataset$Temp, FUN = function(x) mean(x, na.rm = TRUE)),
                   dataset$Temp)

dataset$DO = ifelse(is.na(dataset$DO),
                   ave(dataset$DO, FUN = function(x) mean(x, na.rm = TRUE)),
                   dataset$DO)

dataset$X..Sat = ifelse(is.na(dataset$X..Sat),
                   ave(dataset$X..Sat, FUN = function(x) mean(x, na.rm = TRUE)),
                   dataset$X..Sat)

dataset$pH = ifelse(is.na(dataset$pH),
                   ave(dataset$pH, FUN = function(x) mean(x, na.rm = TRUE)),
                   dataset$pH)

dataset$Conductivity = ifelse(is.na(dataset$Conductivity),
                   ave(dataset$Conductivity, FUN = function(x) mean(x, na.rm = TRUE)),
                   dataset$Conductivity)

dataset$Flow2 = ifelse(is.na(dataset$Flow2),
                   ave(dataset$Flow2, FUN = function(x) mean(x, na.rm = TRUE)),
                   dataset$Flow2)

dataset$Nitrate...Nitrite = ifelse(is.na(dataset$Nitrate...Nitrite),
                   ave(dataset$Nitrate...Nitrite, FUN = function(x) mean(x, na.rm = TRUE)),
                   dataset$Nitrate...Nitrite)

## Eliminate Noisy Data
#install.packages("NoiseFiltersR")

## Normalize Data
normalize <- function(x){
  (x - min(x)) / (max(x) - min(x))
}
dataset.norm <- as.data.frame(lapply(dataset[1:7], normalize))

## Splitting data into the training and test sets
#install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(dataset, SplitRatio = 0.70)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

### Messing around with clustering code DO NOT USE

## Test Clustering
# Using the elbow method to find the optimal number of clusters
set.seed(123)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(dataset, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the dataset
set.seed(123)
kmeans = kmeans(x = dataset, centers = 5)
y_kmeans = kmeans$cluster

# Visualizing the clusters
library(cluster)
clusplot(dataset,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Cluster'),
         xlab = '',
         ylab = '')

Rcpp::sourceCpp(file = "main/k-means-cluster.cpp")