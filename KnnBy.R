# install.packages("tidyverse")

library(tidyverse)
library(class)

setwd("/Users/xieyucheng/Desktop/2021FALL/MIS545/FInalproject")

# read the csv file
playerStats <- read_csv(file = "NDataset.csv",
                        col_types = "iinnnnnnl",
                        col_names = TRUE)

# display the structure as well as the summary
str(playerStats)
summary(playerStats)

# remove the playerID and MinutesPlayed column
playerStats <- playerStats %>% select(-PlayerID) 
playerStats <- playerStats %>% select(-MinutesPlayed)


# what stats do those high-salary players have
playerStatsHighSalary <- playerStats %>%
  filter(SalaryCategory == 1)

# the age range of all the NBA player during 2013-14 season
playerCountAge <- playerStats %>%
  count(Age)

# the average stats that players have, grouping by salary level 0/1
playerGroupBy <- playerStats %>%
  group_by(SalaryCategory) %>%
  summarize(round(mean(Age), 2), round(mean(TotalRebounds), 2), round(mean(Assists), 2), 
            round(mean(Steals), 2), round(mean(Blocks), 2), round(mean(Points), 2))

# set the salary category as the label which is used to be predicted
playerSalaryLabels <- playerStats %>% select(SalaryCategory)

# remove the label column
playerStats <- playerStats %>% select(-SalaryCategory)

displayAllHistograms <- function(tibbleDataset) {
  tibbleDataset %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x=value, fill=key),
                              color = "black") +
    facet_wrap(~ key, scales = "free") +
    theme_minimal()
}

displayAllHistograms(playerStats)

set.seed(545)

sampleSet <- sample(nrow(playerStats),
                    round(nrow(playerStats) * 0.75),
                    replace = FALSE)
# use the 75% sampleset to create the training and testing table
playerStatsTraining <- playerStats[sampleSet, ]
playerStatsTesting <- playerStats[-sampleSet, ]

# same as last step that create the training and testing table for label                    
playerStatsTrainingLabels <- playerSalaryLabels[sampleSet, ]
playerStatsTestingLabels <- playerSalaryLabels[-sampleSet, ]

# create the knn model
playerStatsPrediction <- knn(train = playerStatsTraining,
                             test = playerStatsTesting,
                             cl = playerStatsTrainingLabels$SalaryCategory,
                             k = 71)

print(playerStatsPrediction)

print(summary(playerStatsPrediction))

# createa a confusion matrix, making up of the original testing and prediciion result
playerStatsMatrix <- table(playerStatsTestingLabels$SalaryCategory,
                           playerStatsPrediction)

print(playerStatsMatrix)

# compute the prediction accuracy
predictionAccuracy <- sum(diag(playerStatsMatrix)) / 
  nrow(playerStatsTesting)

print(predictionAccuracy)

# create a matrix to store k value and corresponding prediction accuracy
kMatrix <- matrix(data = NA,
                  nrow = 0,
                  ncol = 2)

colnames(kMatrix) <- c("k value", "Predictive accuracy")

# create a for loop to compute the accuracy for each k value
for(kValue in 1:nrow(playerStatsTraining)) {
  if(kValue %% 2 != 0){
    playerStatsPrediction <- knn(train = playerStatsTraining,
                                 test = playerStatsTesting,
                                 cl = playerStatsTrainingLabels$SalaryCategory,
                                 k = kValue)
    
    playerStatsMatrix <- table(playerStatsTestingLabels$SalaryCategory,
                               playerStatsPrediction)
    
    predictionAccuracy <- sum(diag(playerStatsMatrix)) / 
      nrow(playerStatsTesting)
    
    kMatrix <- rbind(kMatrix, c(kValue, predictionAccuracy))
  }
}

print(kMatrix)

# by creating the k value matrix, we could observe that the best k value 
# for this prediction model is k=71



