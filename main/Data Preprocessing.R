## Importing Data
dataset <- read.csv("lib/WaterQualityData.csv")
dataset <- read.csv("WaterQualityData.csv")
head(dataset,40)
str(dataset)

## Converting "<0.0070" to Nitrate + Nitrite column min = 0
dataset$Nitrate...Nitrite = ifelse(dataset$Nitrate...Nitrite == "<0.0070",
                                   0,dataset$Nitrate...Nitrite)

## Converting Nitrate + Nitrite from type char to num
dataset$Nitrate...Nitrite <- as.numeric(dataset$Nitrate...Nitrite)

## Converting Conductivity from type int to num
dataset$Conductivity <- as.numeric(dataset$Conductivity)

## Subsetting Data to omit columns where all values are missing
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

## TEMP: Finding IQR and quartile values
d=dataset$Temp
tempIQR <- IQR(dataset$Temp)
q1Temp <- as.numeric(quantile(dataset$Temp, prob=0.25))
q3Temp <- as.numeric(quantile(dataset$Temp, prob=0.75))
# For outliers on the high end
dataset$Temp[dataset$Temp > q3Temp+1.5*tempIQR] <- q3Temp+1.5*tempIQR
# For outliers on the low end
dataset$Temp[dataset$Temp < q1Temp-1.5*tempIQR] <- q1Temp-1.5*tempIQR
# Show any changes
d-dataset$Temp
# Note there are no outliers for the temp variable

## DO: Finding IQR and quartile values
d=dataset$DO
doIQR <- IQR(dataset$DO)
q1DO <- as.numeric(quantile(dataset$DO, prob=0.25))
q3DO <- as.numeric(quantile(dataset$DO, prob=0.75))
# For outliers on the high end
dataset$DO[dataset$DO > q3DO+1.5*doIQR] <- q3DO+1.5*doIQR
# For outliers on the low end
dataset$DO[dataset$DO < q1DO-1.5*doIQR] <- q1DO-1.5*doIQR
# Show any changes
d-dataset$DO
# Note there were outliers adjusted for the DO variable

## X..SAT: Finding IQR and quartile values
d=dataset$X..Sat
satIQR <- IQR(dataset$X..Sat)
q1SAT <- as.numeric(quantile(dataset$X..Sat, prob=0.25))
q3SAT <- as.numeric(quantile(dataset$X..Sat, prob=0.75))
# For outliers on the high end
dataset$X..Sat[dataset$X..Sat > q3SAT+1.5*satIQR] <- q3SAT+1.5*satIQR
# For outliers on the low end
dataset$X..Sat[dataset$X..Sat < q1SAT-1.5*satIQR] <- q1SAT-1.5*satIQR
# Show any changes
d-dataset$X..Sat
# Note there were outliers adjusted for the Sat variable

## pH: Finding IQR and quartile values
d=dataset$pH
pHIQR <- IQR(dataset$pH)
q1pH <- as.numeric(quantile(dataset$pH, prob=0.25))
q3pH <- as.numeric(quantile(dataset$pH, prob=0.75))
# For outliers on the high end
dataset$pH[dataset$pH > q3pH+1.5*pHIQR] <- q3pH+1.5*pHIQR
# For outliers on the low end
dataset$pH[dataset$pH < q1pH-1.5*pHIQR] <- q1pH-1.5*pHIQR
# Show any changes
d-dataset$pH
# Note there are no outliers for the pH variable

## Conductivity: Finding IQR and quartile values
d=dataset$Conductivity
conIQR <- IQR(dataset$Conductivity)
q1Con <- as.numeric(quantile(dataset$Conductivity, prob=0.25))
q3Con <- as.numeric(quantile(dataset$Conductivity, prob=0.75))
# For outliers on the high end
dataset$Conductivity[dataset$Conductivity >= q3Con+1.5*conIQR] <- q3Con+1.5*conIQR
# For outliers on the low end
dataset$Conductivity[dataset$Conductivity <= q1Con-1.5*conIQR] <- q1Con-1.5*conIQR
# Show any changes
d-dataset$Conductivity
# Note there were outliers adjusted for the Conductivity variable

## Flow2: Finding IQR and quartile values
d=dataset$Flow2
flowIQR <- IQR(dataset$Flow2)
q1Flow <- as.numeric(quantile(dataset$Flow2, prob=0.25))
q3Flow <- as.numeric(quantile(dataset$Flow2, prob=0.75))
# For outliers on the high end
dataset$Flow2[dataset$Flow2 >= q3Flow+1.5*flowIQR] <- q3Flow+1.5*flowIQR
# For outliers on the low end
dataset$FLow2[dataset$FLow2 <= q1Flow-1.5*flowIQR] <- q1Flow-1.5*flowIQR
# Show any changes
d-dataset$Flow2
# Note there were outliers adjusted for the Flow2 variable

## Nitrate + Nitrite: Finding IQR and quartile values
d=dataset$Nitrate...Nitrite
nnIQR <- IQR(dataset$Nitrate...Nitrite)
q1NN <- as.numeric(quantile(dataset$Nitrate...Nitrite, prob=0.25))
q3NN <- as.numeric(quantile(dataset$Nitrate...Nitrite, prob=0.75))
# For outliers on the high end
dataset$Nitrate...Nitrite[dataset$Nitrate...Nitrite >= q3NN+1.5*nnIQR] <- q3NN+1.5*nnIQR
# For outliers on the low end
dataset$Nitrate...Nitrite[dataset$Nitrate...Nitrite <= q1NN-1.5*nnIQR] <- q1NN-1.5*nnIQR
# Show any changes
d-dataset$Nitrate...Nitrite;
# Note there were outliers adjusted for the Nitrate + Nitrite variable

## Normalize Data
normalize <- function(x){
  (x - min(x)) / (max(x) - min(x))
}
#write.csv(dataset,"C:\\Users\\Nrs11\\OneDrive\\Documents\\WQD.csv",row.names=FALSE)
dataset.norm <- as.data.frame(lapply(dataset[1:7], normalize))

#write.csv(dataset.norm,"C:\\Users\\Nrs11\\OneDrive\\Documents\\NormWQD.csv",row.names=FALSE)
## Splitting data into the training and test sets
#install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(dataset, SplitRatio = 0.70)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


## Test Clustering
# Using the elbow method to find the optimal number of clusters
set.seed(123)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(dataset.norm, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the dataset
set.seed(123)
kmeans = kmeans(x = dataset.norm, centers = 5, iter.max = 18, nstart = 5)
y_kmeans = kmeans$cluster

# Center Count
count.c1 = sum(kmeans$cluster==1)
count.c2 = sum(kmeans$cluster==2)
count.c3 = sum(kmeans$cluster==3)
count.c4 = sum(kmeans$cluster==4)
count.c5 = sum(kmeans$cluster==5)
center.count = cbind=c(count.c1, count.c2, count.c3, count.c4, count.c5)

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

## C5?
#install.packages("C50")
library(C50)
label <- read.csv("Cluster Labels.csv")
labeltest <- factor(label$Cluster,
                    levels = c(1, 2, 3, 4, 5),
                    labels = c("Very Good",
                               "Good",
                               "Neutral",
                               "Poor",
                               "Very Poor"))

dataset.norm$Cluster <- labeltest
names(dataset.norm$Cluster) <- "Cluster"
dataset.norm$Cluster = factor(dataset.norm$Cluster,
                              levels = c(1, 2, 3, 4, 5),
                              labels = c("Very Good",
                                         "Good",
                                         "Neutral",
                                         "Poor",
                                         "Very Poor"))

library(caTools)
set.seed(123)
split = sample.split(dataset.norm, SplitRatio = 0.70)
training_set = subset(dataset.norm, split == TRUE)
test_set = subset(dataset.norm, split == FALSE)

tree <- C5.0(x = training_set[,-8], y = training_set$Cluster)
summary(tree)
plot(tree)

# Prediction?
p1=predict(tree, test_set)
table(test_set[,8],p1)

library(caret)
cm = confusionMatrix(test_set[,8],p1); cm