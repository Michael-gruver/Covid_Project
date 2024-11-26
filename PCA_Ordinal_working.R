library(readxl)
library(nnet)
library(ordinal)  #ordinal regression package
library(rcompanion) #pseudo R square 
library(MASS) #plyr method (for getting data that allows the test of proportional odds)
library(brant)# test of proportional odds

# Read in the data
Training_datadf <- read_excel("Training_data_relabe.xlsx")
Training_datadf <- Training_datadf[-1, , drop = FALSE]
clean_data <- Training_datadf[, 1:152]
clean_data <- na.omit(clean_data)

# Split the data into training and test sets
set.seed(69)
train_index <- sample(1:nrow(clean_data), 0.8 * nrow(clean_data))
train_data <- clean_data[train_index, ]
test_data <- clean_data[-train_index, ]

# Label the response variable
train_data$Y <- factor(train_data$Y, levels = c(0, 1, 2), labels = c("Rarely", "Most of the Time", "Always"))
test_data$Y <- factor(test_data$Y, levels = c(0, 1, 2), labels = c("Rarely", "Most of the Time", "Always"))

# Label the predictor variable Q69
train_data$Q69 <- factor(train_data$Q69, levels = c(1, 2), labels = c("male", "female"))
test_data$Q69 <- factor(test_data$Q69, levels = c(1, 2), labels = c("male", "female"))

# Convert non-numeric columns to numeric
train_data_numeric <- data.frame(lapply(train_data[, -which(names(train_data) %in% c("Y", "Q69"))], as.numeric))
test_data_numeric <- data.frame(lapply(test_data[, -which(names(test_data) %in% c("Y", "Q69"))], as.numeric))

# Perform PCA on the training data
pca <- prcomp(train_data_numeric, scale. = TRUE)

# Determine the number of principal components to retain
explained_variance <- pca$sdev^2 / sum(pca$sdev^2)
num_components <- min(which(cumsum(explained_variance) >= 0.9))

# Create a new data frame with the principal components
train_data_pca <- data.frame(pca$x[, 1:num_components])
names(train_data_pca) <- paste0("PC", 1:num_components)

# Project the test data onto the principal components
test_data_pca <- data.frame(predict(pca, newdata = test_data_numeric)[, 1:num_components])
names(test_data_pca) <- paste0("PC", 1:num_components)

# Fit ordinal regression model with principal components and Q69
OrdModel <- clm(Y ~ Q69 + ., data = cbind(train_data[, c("Y", "Q69")], train_data_pca), link = "logit")
summary(OrdModel)

# Predict on test data
test_predictions_class <- predict(OrdModel, newdata = cbind(test_data[, c("Q69")], test_data_pca), type = "class")

# Calculate test error
test_predictions_numeric <- as.numeric(test_predictions_class$fit) - 1
test_data_Y_numeric <- as.numeric(test_data$Y) - 1
test_error <- sum(abs(test_predictions_numeric - test_data_Y_numeric))
print(paste("Test error:", test_error))

# Read in the new test data
new_test_data <- read_excel("Test_data-1.xlsx")
new_test_data <- new_test_data[, 1:152]
new_test_data <- na.omit(new_test_data)

# Label the response variable and predictor variable Q69
new_test_data$Y <- factor(new_test_data$Y, levels = c(0, 1, 2), labels = c("Rarely", "Most of the Time", "Always"))
new_test_data$Q69 <- factor(new_test_data$Q69, levels = c(1, 2), labels = c("male", "female"))

# Convert non-numeric columns to numeric
new_test_data_numeric <- data.frame(lapply(new_test_data[, -which(names(new_test_data) %in% c("Y", "Q69"))], as.numeric))

# Project the new test data onto the principal components
new_test_data_pca <- data.frame(predict(pca, newdata = new_test_data_numeric)[, 1:num_components])
names(new_test_data_pca) <- paste0("PC", 1:num_components)

# Predict on the new test data
new_test_predictions_class <- predict(OrdModel, newdata = cbind(new_test_data[, c("Q69")], new_test_data_pca), type = "class")

# Calculate test error on the new test data
new_test_predictions_numeric <- as.numeric(new_test_predictions_class$fit) - 1
new_test_data_Y_numeric <- as.numeric(new_test_data$Y) - 1
new_test_error <- sum(abs(new_test_predictions_numeric - new_test_data_Y_numeric))
total_wrong <- sum(test_predictions_numeric != test_data_Y_numeric)
print(paste("Test error on new test data:", new_test_error))

print(paste("The number of predictors used in the model",length(coef(OrdModel)) - 1))

library(modelsummary)
library(flextable)

# Example: Create a linear regression model


# Generate the summary table
tab <- modelsummary(OrdModel, output = 'flextable')

# # Optionally, add a header row
# tab <- tab %>% add_header_row(values = c("", "Coefficients", "Std. Error", "t value", "Pr(>|t|)"))

# Save the table as an image (e.g., PNG)
save_as_image(tab, path = "OrdModel_summary.png")