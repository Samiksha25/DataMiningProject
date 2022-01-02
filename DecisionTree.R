# Install tidyverse package and neuralnet package
# install.packages ("tidyverse")
# install.packages ("neuralnet")

# Load the tidyverse, corrplot packages 
library (tidyverse)
library (rpart)
library (rpart.plot)

# setting folder to Lab4
setwd ("/Users/samiksha/Desktop/Sem1/Data Mining/Project") 

# read the contents of csv in a tibble called playerData
playerData <- read_csv(file = "NDataset.csv",
                       col_types = "iinnnnnnl",
                       col_names = TRUE)

# print the tibble playerData
print (playerData)

# Display the structure of playerData in the console
str (playerData)

# Display the summary of playerData in the console and remove data with 
# null values.
summary (playerData)

# remove any rows having null data
playerData %>% drop_na()

# display only relavant information
playerData <- playerData %>% select(-MinutesPlayed)
print(playerData)


# set random seed 
set.seed(591)

# Randomly split the dataset into playerDataTraining (75% of records) and 
# playerDataTesting (25% of records) using 591 as the random seed
sample_set <- sample (nrow (playerData),
                      round (nrow (playerData) * 0.75),
                      replace = FALSE) 

playerDataTraining <- playerData[sample_set, ]
playerDataTesting <- playerData[-sample_set, ]

# Generate the decision tree model to predict SalaryCategory based on the other 
# variables in the dataset. Use 0.01 as the complexity parameter.
playerDataDecisionTreeModel <- rpart(formula = SalaryCategory ~ Age + 
                                       TotalRebounds +
                                       Assists + 
                                       Steals +
                                       Blocks +
                                       Points,
                                   method = "class",
                                   cp= 0.001,
                                   data = playerDataTraining)

# Display the decision tree visualization in R
rpart.plot(playerDataDecisionTreeModel)

# Predict classes for each record in the testing dataset and store them in 
# playerDataPrediction
playerDataPrediction <- predict (playerDataDecisionTreeModel, 
                                 playerDataTesting,
                               type = "class")

# Display playerDataPrediction on the console
print (playerDataPrediction)

# Evaluate the model by forming a confusion matrix
playerDataConfusionMatrix <- table(playerDataTesting$SalaryCategory,
                                   playerDataPrediction)

# Display the confusion matrix on the console
print (playerDataConfusionMatrix)

# Calculate the model predictive accuracy and store it into a variable called 
# predictiveAccuracy
predictiveAccuracy <- sum(diag(playerDataConfusionMatrix)) / 
  nrow(playerDataTesting)

# Display the predictive accuracy on the console
print (predictiveAccuracy)

