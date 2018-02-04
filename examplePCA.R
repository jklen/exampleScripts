
library(dummy) # for creating dummy variables

df <- data.table::fread(input = 'diamondsProblem.csv')
df <- as.data.frame(df)
dfToPCA <- df
dfToPCA <- na.omit(dfToPCA)

#  one-hot encode categorical variables (PCA works only with numeric variables, so categorical variables
#   need to be encoded)

if (sum(sapply(dfToPCA, function(x){is.ordered(x) | is.factor(x) | is.character(x)})) > 0){
  
  dfDummy <- dummy(dfToPCA, int = T)
  dfToPCA[, sapply(dfToPCA, function(x){is.ordered(x) | is.factor(x) | is.character(x)})] <- NULL
  dfToPCA <- cbind(dfToPCA, dfDummy)
  
} 

# perform PCA, prcomp() function scales variables with scale = T

pca <- prcomp(x = dfToPCA, scale = T)

# merging principal components with input dataset, so input and output rows number are the same,
# rows which contained NA values in ipnut dataset will have NAs in principal components

outputDF <- merge(df, pca$x, by = 'row.names', all = T)
outputDF$Row.names <- as.integer(outputDF$Row.names)
outputDF <- outputDF[order(outputDF$Row.names),]
row.names(outputDF) <- as.character(outputDF$Row.names)
outputDF$Row.names <- NULL

# adding info about explained variance of principal components into their column names

colnames(outputDF)[(ncol(df) + 1):ncol(outputDF)] <- 
  paste(colnames(outputDF)[(ncol(df) + 1):ncol(outputDF)],
        'expVar',
        round(summary(pca)[['importance']][2,], 2) * 100, sep = '')

# in case you need output as dataframe without input data, so only with principal components, uncomment next line
outputDF <- outputDF[, (ncol(df) + 1):ncol(outputDF)]

outputDF
