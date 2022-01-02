# Write R code to generate a naive bayes model and testing the fit of the model
#by calculating the accuracy based on the results of the confusion matrix

#install the required packages
#install.packages("tidyverse")
#install.packages("e1071")

#load the required libraries
library(tidyverse)
library(e1071)

#Read the csv file and store it into a tibble by specifying the column types
playerStatistics <- read_csv(file = "NDataset.csv",
                        col_types = "iinnnnnnl",
                        col_names = TRUE)

#Print the newly generated tibble onto the console
print(playerStatistics)

#Display the summary and structure of the newly created tibble
summary(playerStatistics)
structure(playerStatistics)

#Drop the NA values from the tibble if any
playerStatistics %>% drop_na()

#Remove playerId from the tibble 
playerStatistics <- playerStatistics %>% select(-PlayerID, -MinutesPlayed)

#Set a random seed to get consistent results on every run
set.seed(456)

#Create a sample set with 75% of the records
sampleSet <- sample(nrow(playerStatistics),
                    round(nrow(playerStatistics) * 0.75),
                    replace = FALSE)

#Create the training and testing datasets 
playerStatTraining <- playerStatistics[sampleSet,]
playerStatTesting <- playerStatistics[-sampleSet,]

#Generate the naive bayes model to predict the salary category based on 
#the other independent variables
playerModel <- naiveBayes(formula = SalaryCategory ~.,
                            data = playerStatTraining,
                            laplace = 1)

print(playerModel)

#Calculate the probability of the the salary category being either 0 or 1
playerStatProbability <- predict(playerModel,playerStatTesting, type = "raw")
print(playerStatProbability)

#Predict the value of 0 or 1 for salary category
playerStatPrediction <- predict(playerModel, playerStatTesting, type = "class")
print(playerStatPrediction)

#Generate a confusion matrix using the testing dataset
playerConfusionMatrix <- table(playerStatTesting$SalaryCategory , 
                               playerStatPrediction)
print(playerConfusionMatrix)

#Calculate and display the predictive accuracy of the generated model
predictiveAccuracy <- sum(diag(playerConfusionMatrix)) / nrow(playerStatTesting)
print(predictiveAccuracy)
