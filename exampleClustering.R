
# KMeans clusters with all variables in the dataset, excludes rows with NA values
# gives vector with 'clusterNr' preserving original input dataset rows number,
# rows which were not clustered because they contained NA values will have NA as 'clusterNr'

library(scales)
library(dplyr)
library(tidyr)
library(dummy)

df <- data.table::fread(input = 'irisWithMissingValues.csv')
df <- as.data.frame(df)
dfToClust <- df
dfToClust <- na.omit(dfToClust)

# normalize continuous variables

dfToClust[sapply(dfToClust, function(x) is.numeric(x) | is.integer(x))] <- 
  lapply(dfToClust[sapply(dfToClust, function(x) is.numeric(x) | is.integer(x))], rescale)

#  one-hot encode categorical variables

if (sum(sapply(dfToClust, function(x){is.ordered(x) | is.factor(x) | is.character(x)})) > 0){
  
  dfDummy <- dummy(dfToClust, int = T)
  dfToClust[, sapply(dfToClust, function(x){is.ordered(x) | is.factor(x) | is.character(x)})] <- NULL
  dfToClust <- cbind(dfToClust, dfDummy)
  
} 

# create KMeans model and get clusters

set.seed(888)
km <- kmeans(x = dfToClust, centers = 3)

df$cluster[rowSums(is.na(df)) == 0] <- paste('cluster', km$cluster, sep = '')
df$cluster

