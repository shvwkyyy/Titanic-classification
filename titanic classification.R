library(rpart)
library(rpart.plot)
library(pROC)
library(ggplot2)

# Read the Titanic dataset
Titanic <- read.csv("C:/Users/youse/OneDrive/Desktop/archive/Titanic-Dataset.csv")
sum(duplicated(Titanic))
distinct(Titanic)

# Remove unnecessary columns and convert Survived to a factor
Titanic <- Titanic[, !(names(Titanic) %in% c("PassengerId", "Name", "Ticket", "Cabin"))]
Titanic$Survived <- as.factor(Titanic$Survived)

# Remove rows with missing values
Titanic <- na.omit(Titanic)

# Remove outliers from the Age column
outlier <- boxplot(Titanic$Age)
Titanic <- Titanic[-which(Titanic$Age %in% outlier),]

# Visualize Survival Rate by Sex
ggplot(Titanic, aes(x = Sex, fill = Survived)) +
  geom_bar(position = "fill") +
  labs(title = "Survival Rate by Sex")

# Split the data into training and test sets
set.seed(123)
train_indices <- sample(1:nrow(Titanic), size = 0.7 * nrow(Titanic))
train_data <- Titanic[train_indices, ]
test_data <- Titanic[-train_indices, ]

# Train the decision tree model
tree_model <- rpart(Survived ~ ., data = train_data, method = "class")

# Plot the decision tree
rpart.plot(tree_model)

# Make predictions on the test data
predictions <- predict(tree_model, newdata = test_data, type = "class")

# Create the confusion matrix
conf_matrix <- table(predictions, test_data$Survived)
print("Confusion Matrix:")
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("\nAccuracy:", accuracy, "\n")

# Plot the ROC curve
roc_curve <- roc(as.numeric(test_data$Survived), as.numeric(predictions))
plot(roc_curve, main = "ROC Curve")

# Calculate AUC
auc <- auc(roc_curve)
cat("AUC:", auc, "\n")

# Calculate precision, recall, and F1 score
TP <- conf_matrix[2, 2]  # True Positives
TN <- conf_matrix[1, 1]  # True Negatives
FP <- conf_matrix[1, 2]  # False Positives
FN <- conf_matrix[2, 1]  # False Negatives

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * precision * recall / (precision + recall)

cat("\nPrecision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

# Number of people predicted to have survived and died
predicted_survived <- sum(predictions == 1)
predicted_died <- sum(predictions == 0)

cat("Number of people predicted to have survived:", predicted_survived, "\n")
cat("Number of people predicted to have died:", predicted_died, "\n")

# Number of people correctly predicted to have died
correctly_predicted_died <- TN

cat("Number of people correctly predicted to have died:", correctly_predicted_died, "\n")

# Number of people who survived and died based on the actual data
actual_survived <- sum(test_data$Survived == 1)
actual_died <- sum(test_data$Survived == 0)

cat("Number of people who actually survived:", actual_survived, "\n")
cat("Number of people who actually died:", actual_died, "\n")











