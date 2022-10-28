#Installing Packages
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(janitor)
library(gmodels)
library(class)
library(corrplot)

#Loading the dataset
diabetes_df <- read.csv("diabetes.csv")

#Exploring the dataset
str(diabetes_df)
head(diabetes_df)
summary(diabetes_df)
colnames(diabetes_df)

#Checking for duplicates
get_dupes(diabetes_df)

#Transforming the outcome column
diabetes_df$Outcome <- as.character(diabetes_df$Outcome)
diabetes_df <- diabetes_df %>%
  mutate(Outcome = fct_recode(Outcome, "Diabetic" = "1", "Non Diabetic" = "0"))
diabetes_df$Outcome <- factor(diabetes_df$Outcome,
                              levels = c("Diabetic", "Non Diabetic"),
                              labels = c("Diabetic", "Non Diabetic"))

#Checking for Correlation
diabetes_correlation_df <- diabetes_df[-9]
diabetes_correlation_df <- cor(diabetes_correlation_df)
corrplot(diabetes_correlation_df, method = "color", type = "lower", addCoef.col = "black", col = COL2("RdYlBu"), number.cex = 0.8, tl.cex = 0.8)

diabetic_correlation_df <- diabetes_df %>%
  filter(Outcome == "Diabetic")
diabetic_correlation_df <- diabetic_correlation_df[-9]
diabetic_correlation_df <- cor(diabetic_correlation_df)
corrplot(diabetic_correlation_df, method = "color", type = "lower", addCoef.col = "black", col = COL2("RdYlBu"), number.cex = 0.8, tl.cex = 0.8)

#Training the model
#Creating the normalize function
normalize <- function(x){
  (x-min(x))/(max(x)-min(x))
}

#applying the normalize function to the dataset
diabetes_df_n <- as.data.frame(lapply(diabetes_df[1:8],normalize))

#seperating the dataset into Train and Test data-frames
diabetes_df_train <- diabetes_df_n[1:668, ]
diabetes_df_test <- diabetes_df_n[669:768, ]

#creating the labels for the train and test data-frames
diabetes_train_labels <- diabetes_df[1:668, 9]
diabetes_test_labels <- diabetes_df[669:768, 9]

#training the model
diabetes_prediction <- knn(train = diabetes_df_train, test = diabetes_df_test, cl = diabetes_train_labels, k = 29)

CrossTable(y = diabetes_prediction, x = diabetes_test_labels, prop.chisq = FALSE)

#applying z-standardization to the dataset
diabetes_df_z <- as.data.frame(scale(diabetes_df[1:8]))

diabetes_df_train <- diabetes_df_z[1:668, ]
diabetes_df_test <- diabetes_df_z[669:768, ]