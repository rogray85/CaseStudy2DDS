#Import the data file
# #office
# data = read.csv("/Users/database/Desktop/SMU/Doing_Data_Science/Unit 14 and 15 Case Study 2/CaseStudy2-data.csv")
# #laptop
# #data = read.csv("/Users/oneal/Desktop/SMU/Doing_Data_Science/Unit 14 and 15 Case Study 2/CaseStudy2-data.csv")
# #find missing values in the data
# missing_values_per_column = colSums(is.na(data))
# total_missing_values = sum(missing_values_per_column)
# print("Number of missing values per column:")
# print(missing_values_per_column)
# cat("\nTotal number of missing values in the dataset:", total_missing_values)
# 
# 
# #going to do a KNN and NB Model, not going to do a linear model since its best fitted for continues variables, not catagorical ones
# 
# #KNN
# library(class)
# library(caret)
# library(randomForest)
# #convert catagorical variables into numeric factors 
# data$Attrition = as.numeric(factor(data$Attrition)) - 1
# data$BusinessTravel = as.numeric(factor(data$BusinessTravel))
# data$Department = as.numeric(factor(data$Department))
# data$EducationField = as.numeric(factor(data$EducationField))
# data$Gender = as.numeric(factor(data$Gender))
# data$JobRole = as.numeric(factor(data$JobRole))
# data$MaritalStatus = as.numeric(factor(data$MaritalStatus))
# data$Over18 = as.numeric(factor(data$Over18))
# data$OverTime = as.numeric(factor(data$OverTime))
# 
# #Split our dataset into training and testing sets
# set.seed(123)
# indexes = sample(1:nrow(data), size = 0.7 * nrow(data))
# train_set = data[indexes,]
# test_set = data[-indexes,]
# 
# #scale the numeric features
# numeric_features = c('Age', 'DailyRate', 'DistanceFromHome', 'Education', 'EmployeeNumber', 'EnvironmentSatisfaction', 'HourlyRate', 'JobInvolvement', 'JobLevel', 'JobSatisfaction', 'MonthlyIncome', 'MonthlyRate', 'NumCompaniesWorked', 'PercentSalaryHike', 'PerformanceRating', 'RelationshipSatisfaction', 'StandardHours', 'StockOptionLevel', 'TotalWorkingYears', 'TrainingTimesLastYear', 'WorkLifeBalance', 'YearsAtCompany', 'YearsInCurrentRole', 'YearsSinceLastPromotion', 'YearsWithCurrManager')
# train_set[, numeric_features] = scale(train_set[, numeric_features])
# test_set[, numeric_features] = scale(test_set[, numeric_features])
# #removed 
# train_set = train_set[, !colnames(train_set) %in% c("EmployeeCount", "StandardHours")]
# test_set = test_set[, !colnames(test_set) %in% c("EmployeeCount", "StandardHours")]
# 
# # #find the best K-value
# # # Add this line to your code to store the accuracies of each K value
# # accuracies = c()
# # 
# # # Set the range of K values to be evaluated
# # k_range = 1:100
# # 
# # # Iterate over the K values in the range
# # for (k in k_range) {
# #   # Perform the KNN classification with the current K value
# #   predicted_labels = knn(train_set[, -2], test_set[, -2], cl = train_set$Attrition, k = k)
# #   
# #   # Calculate the accuracy of the model for the current K value
# #   actual_lables = test_set$Attrition
# #   accuracy = sum(predicted_labels == actual_lables) / length(actual_lables)
# #   
# #   # Store the accuracy of the model for the current K value
# #   accuracies = c(accuracies, accuracy)
# # }
# # 
# # # Find the optimal K value and its corresponding accuracy
# # optimal_k = k_range[which.max(accuracies)]
# # max_accuracy = max(accuracies)
# # 
# # cat("Optimal K value:", optimal_k, "\nMaximum accuracy:", max_accuracy)
# 
# #perform the KNN classification
# k = 3 # Number of nearest neighbors
# predicted_labels = knn(train_set[, -2], test_set[, -2], cl = train_set$Attrition, k = k)
# 
# #Evaluate our models performance
# actual_lables = test_set$Attrition
# accuracy = sum(predicted_labels == actual_lables) / length(actual_lables)
# print(accuracy)
# 
# # Create a confusion matrix
# conf_matrix = table(Predicted = predicted_labels, Actual = actual_lables)
# 
# # Print the confusion matrix
# print(conf_matrix)
# 
# # Calculate sensitivity (recall or true positive rate)
# sensitivity = conf_matrix[2, 2] / (conf_matrix[2, 2] + conf_matrix[1, 2])
# cat("Sensitivity:", sensitivity)
# 
# # Calculate specificity (true negative rate)
# specificity = conf_matrix[1, 1] / (conf_matrix[1, 1] + conf_matrix[2, 1])
# cat("\nSpecificity:", specificity)

                                                                    
#do the model with Naïve Bayes
convert_to_factors = function(df, threshold) {
  for (col_name in colnames(df)) {
    if (length(unique(df[[col_name]])) <= threshold) {
      df[[col_name]] = as.factor(df[[col_name]])
    }
  }
  return(df)
}
data2 = read.csv("/Users/home/Desktop/SMU/Doing_Data_Science/Unit 14 and 15 Case Study 2/CaseStudy2-data.csv")
no_attrition = read_csv("/Users/home/Desktop/SMU/Doing_Data_Science/Unit 14 and 15 Case Study 2/CaseStudy2CompSet No Attrition.csv")
data2$Age = NULL
library(caret)
library(e1071)
library(olsrr)
library(dplyr)
library(klaR)
# Split the data into train and test sets
set.seed(123)
data_split = createDataPartition(data2$Attrition, p = 0.7, list = FALSE)
train_set = data2[data_split, ]
test_set = data2[-data_split, ]

threshold = 10
train_set = convert_to_factors(train_set, threshold)
test_set = convert_to_factors(test_set, threshold)

nb_model = naiveBayes(Attrition ~ ., data = train_set)
predicted_labels = predict(nb_model, test_set[, -2])

actual_labels = test_set$Attrition
accuracy = sum(predicted_labels == actual_labels) / length(actual_labels)
print(accuracy)

# Create a confusion matrix
conf_matrix_nb = table(Predicted = predicted_labels, Actual = actual_labels)
print(conf_matrix_nb)

# Calculate sensitivity (true positive rate)
sensitivity_nb = conf_matrix_nb[2, 2] / (conf_matrix_nb[2, 2] + conf_matrix_nb[1, 2])
cat("Sensitivity (Naive Bayes):", sensitivity_nb)

# Calculate specificity (true negative rate)
specificity_nb = conf_matrix_nb[1, 1] / (conf_matrix_nb[1, 1] + conf_matrix_nb[2, 1])
cat("\nSpecificity (Naive Bayes):", specificity_nb)

# Build a new Naïve Bayes model with adjusted Laplace value
nb_model_laplace = naiveBayes(Attrition ~ ., data = train_set, laplace = 2)

# Make predictions with the new model
predicted_labels_laplace = predict(nb_model_laplace, test_set[, -2])

# Calculate new accuracy
accuracy_laplace = sum(predicted_labels_laplace == actual_labels) / length(actual_labels)
print(accuracy_laplace)

# Create a new confusion matrix
conf_matrix_nb_laplace = table(Predicted = predicted_labels_laplace, Actual = actual_labels)
print(conf_matrix_nb_laplace)

# Calculate new sensitivity (true positive rate)
sensitivity_nb_laplace = conf_matrix_nb_laplace[2, 2] / (conf_matrix_nb_laplace[2, 2] + conf_matrix_nb_laplace[1, 2])
cat("Sensitivity (Naive Bayes with Laplace adjustment):", sensitivity_nb_laplace)

# Calculate new specificity (true negative rate)
specificity_nb_laplace = conf_matrix_nb_laplace[1, 1] / (conf_matrix_nb_laplace[1, 1] + conf_matrix_nb_laplace[2, 1])
cat("\nSpecificity (Naive Bayes with Laplace adjustment):", specificity_nb_laplace)

no_attrition = convert_to_factors(no_attrition, threshold)
predicted_labels = predict(nb_model_laplace, no_attrition[, -2])
no_attrition$Attrition = predicted_labels
subset = no_attrition[, c("ID", "Attrition")]
write.csv(subset, "/Users/home/Desktop/SMU/Doing_Data_Science/Unit 14 and 15 Case Study 2/predicted_labels.csv", row.names = FALSE)

library(pROC)

# Use the Laplace-adjusted Naive Bayes model to predict probabilities on the test set
predicted_probs = predict(nb_model_laplace, test_set[, -2], type = "raw")[, 2]

# Compute the ROC curve based on the predicted probabilities and actual "Attrition" class labels
roc_obj = roc(actual_labels, predicted_probs)

# Plot the ROC curve
plot(roc_obj, main = "ROC Curve for Naive Bayes Model (with Laplace adjustment)",
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)",
     print.auc = TRUE)

#############################################################################################################################

#Model to get the monthly incomes - Regression model 
library(tidyverse)
library(caret)
library(glmnet)
library(readxl)
train_data = read_csv("/Users/database/Desktop/SMU/Doing_Data_Science/Unit 14 and 15 Case Study 2/CaseStudy2-data.csv")
test_data = read_excel("/Users/database/Desktop/SMU/Doing_Data_Science/Unit 14 and 15 Case Study 2/CaseStudy2CompSet No Salary.xlsx")

preprocess_data = function(data, target_variable = TRUE) {
  data = data %>%
    dplyr::select(-EmployeeCount, -EmployeeNumber, -Over18) %>%
    mutate(across(where(is.character), as.factor))
  
  if (target_variable) {
    data_dummy = dummyVars(MonthlyIncome ~ ., data = data)
    data_clean = data.frame(predict(data_dummy, newdata = data))
    data_clean$MonthlyIncome = data$MonthlyIncome
  } else {
    data_dummy = dummyVars(~., data = data)
    data_clean = data.frame(predict(data_dummy, newdata = data))
  }
  
  return(data_clean)
}



train_clean = preprocess_data(train_data)
test_clean = preprocess_data(test_data, target_variable = FALSE)

model = lm(MonthlyIncome ~ ., data = train_clean)
summary(model)

forward_model = lm(MonthlyIncome ~ ID + Age + BusinessTravel + DailyRate + Department.Human.Resources + Department.Research...Development + Department.Sales + DistanceFromHome , data = train_clean)
train_control = trainControl(method = "LOOCV")
test_data$MonthlyIncome = predict(forward_model, newdata = test_data)
write.csv(test_data, "forward_model.csv")
#train the forward model
##################################
# Compute average conditional probabilities for each feature
avg_cond_probs = sapply(nb_model$tables, function(x) mean(x[, 2]) - mean(x[, 1]))

# Sort the average conditional probabilities in descending order
sorted_importance = sort(avg_cond_probs, decreasing = TRUE)

# Extract the top three features
top_three_features = names(sorted_importance)[1:3]

# Print the top three features
cat("Top 3 factors contributing to turnover:", top_three_features, "\n")
#Top 3 factors contributing to turnover: MonthlyIncome Age TotalWorkingYears


##Graphs##
library(ggplot2)
#Age Vs Monthly Income by Gender
ggplot(data, aes(x = Age, y = MonthlyIncome, color = factor(Gender))) +
  geom_point() +
  theme_minimal() +
  labs(title = "", x = "Age", y = "Monthly Income") +
  scale_color_manual(values=c("blue", "pink"), labels=c("Male", "Female")) +
  geom_smooth(method = "lm", se = FALSE, data = subset(data, Gender == 1)) +
  geom_smooth(method = "lm", se = FALSE, data = subset(data, Gender == 2))

#Job Role Vs. Monthly Income by Department
ggplot(data2, aes(x = JobRole, y = MonthlyIncome, fill = Department)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Job Role vs. Monthly Income by Department", x = "Job Role", y = "Monthly Income")

ggplot(data2, aes(x = DistanceFromHome, fill = Attrition)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  facet_wrap(~ JobRole, scales = "free_y") +
  theme_minimal() +
  labs(title = "", x = "Distance from Home", y = "Frequency")

ggplot(data2, aes(x = YearsAtCompany, y = YearsInCurrentRole, color = Attrition)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Years at Company vs. Years in Current Role by Attrition", x = "Years at Company", y = "Years in Current Role")


# Convert JobRole to a factor
data2$JobRole = as.factor(data2$JobRole)

ggplot(data2, aes(x = Attrition, y = MonthlyIncome, fill = Attrition)) +
  geom_boxplot(color = "black", alpha = 0.7) +
  scale_fill_manual(values = c("#0072B2", "#E69F00")) + # use colorbrewer2.org scheme
  theme_minimal() +
  labs(title = "Attrition and Monthly Income", subtitle = "Distribution of Monthly Income by Attrition Status", x = "Attrition Status", y = "Monthly Income (USD)", fill = "Attrition Status") +
  theme(plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 14))


ggplot(data2, aes(x = MonthlyIncome, fill = Attrition, color = Attrition)) +
  geom_density(alpha = 0.7) +
  labs(title = "Monthly Income by Attrition Status",
       x = "Monthly Income (USD)",
       y = "Density",
       fill = "Attrition Status",
       color = "Attrition Status") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))

library(ggplot2)

# Create boxplot
# Convert Attrition to a factor variable
data$Attrition = as.factor(data$Attrition)

# Create the boxplot
# Convert Attrition to a factor variable
data$Attrition = factor(data$Attrition, levels = c(0, 1), labels = c("No", "Yes"))

# Create the boxplot
ggplot(data, aes(x = Attrition, y = Age, fill = Attrition)) +
  geom_boxplot(alpha = 0.7, color = "black") +
  scale_fill_manual(values = c("#0072B2", "#E69F00")) +
  scale_x_discrete(labels = c("No", "Yes")) +
  theme_minimal() +
  labs(title = "Age by Attrition Status", x = "Attrition Status", y = "Age") +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 14))



# Create density plot
ggplot(data, aes(x = Age, fill = Attrition, color = Attrition)) +
  geom_density(alpha = 0.7) +
  labs(title = "Age by Attrition Status",
       x = "Age",
       y = "Density",
       fill = "Attrition Status",
       color = "Attrition Status") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))


# Boxplot
ggplot(data, aes(x = Attrition, y = TotalWorkingYears, fill = Attrition)) +
  geom_boxplot(alpha = 0.7, color = "black") +
  scale_fill_manual(values = c("#0072B2", "#E69F00")) +
  labs(title = "", x = "Attrition Status", y = "Total Working Years") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 14))

# Density plot
ggplot(data, aes(x = TotalWorkingYears, fill = Attrition, color = Attrition)) +
  geom_density(alpha = 0.7) +
  labs(title = "Total Working Hours by Attrition Status", x = "Total Working Years", y = "Density",
       fill = "Attrition Status", color = "Attrition Status") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))






