# Install tidyverse package and neuralnet package
# install.packages ("tidyverse")
# install.packages ("neuralnet")
# install.packages("dummies")

# Load the tidyverse, corrplot packages 
library (tidyverse)
library(neuralnet)
library (corrplot)
library (olsrr)

# setting folder to Lab4
setwd ("/Users/samiksha/Desktop/Sem1/Data Mining/Project") 

# read the contents of csv in a tibble called playerData
playerData <- read_csv(file = "NDataset.csv",
                           col_types = "iinnnnnnl",
                           col_names = TRUE)

# print the tibble playerData
print (playerData)

# Display the structure of fishingCharter in the console
str (playerData)

# Display the summary of fishingCharter in the console
summary (playerData)

# craete a correlation plot  
round (cor (playerData), 2 )

# removed MinutesPlayed  
playerData <- playerData %>% select(-MinutesPlayed)

#scale the data to normalize it
playerData <- playerData %>%
  mutate(AgeScaled = (Age - min(Age)) /
           (max(Age) - min(Age)),
         TotalReboundsScaled = (TotalRebounds - min(TotalRebounds)) /
           (max(TotalRebounds) - min(TotalRebounds)),
         AssistsScaled = (Assists - min(Assists)) /
           (max(Assists) - min(Assists)),
         StealsScaled = (Steals - min(Steals)) /
           (max(Steals) - min(Steals)),
         BlocksScaled = (Blocks - min(Blocks)) /
           (max(Blocks) - min(Blocks)),
         PointsScaled = (Points - min(Points)) /
           (max(Points) - min(Points)))

print (playerData)
# set random seed
set.seed(591)

# Randomly split the dataset into playerDataTraining (75% of records) and 
# playerDataTesting (25% of records) using 591 as the random seed
sample_set <- sample (nrow (playerData),
                      round (nrow (playerData) * 0.75),
                      replace = FALSE) 

# riceFarmsTesting (25% of records)
playerDataTraining <- playerData[sample_set, ]
playerDataTesting <- playerData[-sample_set, ]

# generating the neural network
playerDataNeuralNet <- neuralnet(
  formula = SalaryCategory ~ Age + TotalRebounds + Steals 
  + Assists + Blocks + Points ,
  data = playerDataTraining,
  hidden = 3,
  act.fct = "logistic",
  linear.output = FALSE,
  stepmax = 1e10)

# display the numeric result
print (playerDataNeuralNet$result.matrix)

# visualize the neural network
plot(playerDataNeuralNet)

# Use playerDataNeuralNet to generate probabilities on the 
#playerDataTesting data set and store this in playerDataProbability
playerDataProbability <- compute(playerDataNeuralNet,
                                     playerDataTesting)

# display the predictions from the testing dataset on consolde
print(playerDataProbability)

# Convert probability predictions into 0/1 predictions and store this into 
# playerDataPrediction
playerDataPrediction <- 
  ifelse(playerDataProbability$net.result > 0.5, 1, 0)

# print the predictions
print(playerDataPrediction)

# Evaluate the model by forming a confusion matrix
playerDataConfusionMatrix <- table(playerDataTesting$SalaryCategory,
                                       playerDataPrediction)

# print it on console
print(playerDataConfusionMatrix)

# calculate the model predictive accuracy
predictiveAccuracy <- sum(diag(playerDataConfusionMatrix)) /
nrow(playerDataTesting)

# display the predictive accuracy
print(predictiveAccuracy)
  
