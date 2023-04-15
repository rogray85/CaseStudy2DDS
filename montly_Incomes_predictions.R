library(tidyverse)
library(caret)
library(glmnet)
library(readxl)
library(olsrr)
train_data = read_csv("/Users/home/Desktop/SMU/Doing_Data_Science/Unit 14 and 15 Case Study 2/CaseStudy2-data.csv")
test_data = read_excel("/Users/home/Desktop/SMU/Doing_Data_Science/Unit 14 and 15 Case Study 2/CaseStudy2CompSet No Salary.xlsx")

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

# Perform forward selection using p-value as the criterion
forward_selected = ols_step_forward_p(model, penter = 0.05, details = TRUE)

# Get the final model formula
forward_formula = as.formula(forward_selected$model)

# Refit the model using the selected variables
forward_model = lm(forward_formula, data = train_clean)

# Summarize the final model
summary(forward_model)

# Make predictions on the test set using the forward_model
test_predictions = predict(forward_model, newdata = test_clean)

# Create a data frame with the ID numbers and the predicted Monthly Incomes
predicted_df = data.frame(ID = test_data$ID, PredictedMonthlyIncome = test_predictions)

# Write the ID numbers and predicted Monthly Incomes to a CSV file
write.csv(predicted_df, "/Users/home/Desktop/SMU/Doing_Data_Science/Unit 14 and 15 Case Study 2/forward_predictions.csv", row.names = FALSE)

#Backward Model
# Build the backward_model using ols_step_backward_p() function
backward_results = ols_step_backward_p(fit_home, premove = 0.05, details = TRUE)
backward_formula = as.formula(backward_results$model)

# Fit the backward model
backward_model = lm(backward_formula, data = train_clean)

# Make predictions on the test set using the backward_model
test_predictions_backward = predict(backward_model, newdata = test_clean)

# Create a data frame with the ID numbers and the predicted Monthly Incomes for the backward model
predicted_df_backward = data.frame(ID = test_data$ID, PredictedMonthlyIncome = test_predictions_backward)

# Write the ID numbers and predicted Monthly Incomes for the backward model to a CSV file
write.csv(predicted_df_backward, "/Users/home/Desktop/SMU/Doing_Data_Science/Unit 14 and 15 Case Study 2/backward_predictions.csv", row.names = FALSE)

##Stepwise Mode
# Build the stepwise model using ols_step_both_p() function
stepwise_results = ols_step_both_p(fit_home, pent = 0.05, prem = 0.05, details = TRUE)
stepwise_formula = as.formula(stepwise_results$model)

# Fit the stepwise model
stepwise_model = lm(stepwise_formula, data = train_clean)

# Make predictions on the test set using the stepwise_model
test_predictions_stepwise = predict(stepwise_model, newdata = test_clean)

# Create a data frame with the ID numbers and the predicted Monthly Incomes for the stepwise model
predicted_df_stepwise = data.frame(ID = test_data$ID, PredictedMonthlyIncome = test_predictions_stepwise)

# Write the ID numbers and predicted Monthly Incomes for the stepwise model to a CSV file
write.csv(predicted_df_stepwise, "/Users/home/Desktop/SMU/Doing_Data_Science/Unit 14 and 15 Case Study 2/stepwise_predictions.csv", row.names = FALSE)

# Obtain the model's fitted values and residuals
fitted_values = stepwise_model$fitted.values
residuals = stepwise_model$residuals

# Create a data frame with the fitted values and residuals
data_plot = data.frame(FittedValues = fitted_values, Residuals = residuals)

# Create the diagnostic plot using ggplot2
ggplot(data_plot, aes(x = FittedValues, y = Residuals)) +
  geom_point() +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Stepwise Model Residuals vs Fitted Values",
    x = "Fitted Values",
    y = "Residuals"
  )
#Test my models

# Set up cross-validation
k_folds = 10
cv_control = trainControl(method = "cv", number = k_folds, savePredictions = "final")

# Define the metric you want to compare (e.g., RMSE or R-squared)
metric = "RMSE"

# Cross-validation for forward model
forward_cv = train(forward_formula, data = train_clean, method = "lm", trControl = cv_control, metric = metric)

# Cross-validation for backward model
backward_cv = train(backward_formula, data = train_clean, method = "lm", trControl = cv_control, metric = metric)

# Cross-validation for stepwise model
stepwise_cv = train(stepwise_formula, data = train_clean, method = "lm", trControl = cv_control, metric = metric)

# Compare the results
cv_results = data.frame(Model = c("Forward", "Backward", "Stepwise"),
                         RMSE = c(forward_cv$results$RMSE, backward_cv$results$RMSE, stepwise_cv$results$RMSE),
                         Rsquared = c(forward_cv$results$Rsquared, backward_cv$results$Rsquared, stepwise_cv$results$Rsquared))

cv_results


