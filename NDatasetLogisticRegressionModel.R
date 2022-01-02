# Load the tidyverse, corrplot, and olsrr libraries
library(tidyverse)
library(corrplot)
library(olsrr)
library(smotefamily)


# Set the working directory to the Group Project folder
setwd("~/U of A/Year 2 - Semester 1/MIS 545/Group Project")


# Read NDataSet.csv into a tibble called nDataSet 
nDataSet <- read_csv(file = "NDataSet.csv",
                        col_types = "iinnnnnnl",
                        col_names = TRUE)


# Display nDataSet in the console
print(nDataSet)


# Display the structure of nDataSet in the console
print(str(nDataSet))


# Display the summary of nDataSet in the console
print(summary(nDataSet))


# Remove PlayerID and MinutesPlayed (We remove MinutesPlayed because, at the end
# of our code, we check for multicollinearity, which we find with MinutesPlayed
# and Points, so we decide to eliminate MinutesPlayed)
nDataSet <- select(nDataSet, -c(PlayerID, MinutesPlayed))


# Recreate the displayAllHistograms() function as shown in a prior video
# demonstration
displayAllHistograms <- function(tibbleDataset) {
  tibbleDataset %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping= aes(x=value, fill=key),
                              color= "black") +
    facet_wrap (~ key, scales = "free") +
    theme_minimal ()
}

# Call the displayAllHistograms() function, passing in nDataSet as an 
# argument
displayAllHistograms(nDataSet)


# Display a correlation matrix of nDataSet rounded to two decimal places
round(cor(nDataSet), 2)


# Display a correlation plot using the "number" method and limit output to the
# bottom left
corrplot(cor(nDataSet),
         method = "number",
         type = "lower")


# Set random seed to 198
set.seed(198)


# Create a vector of 75% randomly sampled rows from the dataset
sampleSet <- sample(nrow(nDataSet),
                    round(nrow(nDataSet) * 0.75),
                    replace = FALSE)


# Randomly split the dataset into nDataSetTraining (75% of records)
nDataSetTraining <- nDataSet[sampleSet, ]


# Randomly split the dataset into nDataSetTesting (25% of records)
nDataSetTesting <- nDataSet[-sampleSet, ]


# Check if we have a class imbalance issue in SalaryCategory
summary(nDataSetTraining$SalaryCategory)


# Generate the logistic regression model (using SalaryCategory as the binary
# dependent variable) and save it in an object called nDataSetModel
nDataSetModel <- glm(data = nDataSetTraining,
                        family = binomial,
                        formula = SalaryCategory ~ .)


# Display the logistic regression model results using the summary() function
summary(nDataSetModel)


# Calculate the odds ratios for each of the 6 independent variable coefficients
exp(coef(nDataSetModel)["Age"])
exp(coef(nDataSetModel)["TotalRebounds"])
exp(coef(nDataSetModel)["Points"])


# Use the model to predict outcomes in the testing dataset as described in the
# video.
nDataSetPrediction <- predict(nDataSetModel,
                                 nDataSetTesting,
                                 type = "response")


# Treat anything below or equal to 0.5 as a 0, anything above 0.5 as a 1.
nDataSetPrediction <- 
  ifelse(nDataSetPrediction >= 0.5, 1, 0)


# Generate a confusion matrix of predictions
print(nDataSetConfusionMatrix <- table(nDataSetTesting$SalaryCategory,
                                       nDataSetPrediction))


# Calculate the false positive rate
nDataSetConfusionMatrix[1, 2] /
  (nDataSetConfusionMatrix[1, 2] +
     nDataSetConfusionMatrix[1, 1])


# Calculate the false negative rate
nDataSetConfusionMatrix[2, 1] /
  (nDataSetConfusionMatrix[2, 1] +
     nDataSetConfusionMatrix[2, 2])


# Calculate the model prediction accuracy
sum(diag(nDataSetConfusionMatrix)) / nrow(nDataSetTesting)


# Check for multicollinearity in our dataset
car::vif(nDataSetModel)
