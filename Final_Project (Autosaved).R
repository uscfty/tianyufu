
# Load necessary libraries
library(ggplot2)
library(leaps)
library(car)
library(dplyr)
library(stats)
library(gridExtra)

# read CSV file
df <- read.csv("/Users/futianyu/Desktop/数理统计/STAT 8120/Final Project/Student_Performance.csv")
# Convert categorical variables to numerical variables
df$Extracurricular.Activities <- ifelse(df$Extracurricular.Activities == "Yes", 1, 0)
# View the first few rows of data
head(df)
colnames(df)
# Get summary statistics
summary(df)


# draw box plot
boxplot_hours_studied <- ggplot(df, aes(y = Hours.Studied)) + geom_boxplot(fill = "orange", color = "black", alpha = 0.7) + labs(title = 'Hours Studied', y = 'Hours Studied') + theme_minimal()

boxplot_previous_scores <- ggplot(df, aes(y = Previous.Scores)) + geom_boxplot(fill = "yellow", color = "black", alpha = 0.7) + labs(title = 'Previous Scores', y = 'Previous Scores') + theme_minimal()

boxplot_sleep_hours <- ggplot(df, aes(y = Sleep.Hours)) + geom_boxplot(fill = "red", color = "black", alpha = 0.7) + labs(title = 'Sleep Hours', y = 'Hours') + theme_minimal()

boxplot_sample_question<- ggplot(df, aes(y = Sample.Question.Papers.Practiced)) + geom_boxplot(fill = "purple", color = "black", alpha = 0.7) + labs(title = 'Sample Question Papers Practiced', y = 'Questions') + theme_minimal()

boxplot_performance_index <- ggplot(df, aes(y = Performance.Index)) + geom_boxplot(fill = "blue", color = "black", alpha = 0.7) + labs(title = 'Performance Index', y = 'scores') + theme_minimal()

# Display all box plots
grid.arrange(boxplot_hours_studied, boxplot_previous_scores, boxplot_sleep_hours, boxplot_sample_question, boxplot_performance_index, ncol = 2)

#Draw a distribution graph

hist_hours_studied <- ggplot(df, aes(x = Hours.Studied)) + geom_histogram(binwidth = 1, fill = "yellow", color = "black", alpha = 0.7) + labs(title = 'Distribution of Hours Studied', x = 'Hours.Studied', y = 'Frequency') + theme_minimal()

hist_previous_scores <- ggplot(df, aes(x = Previous.Scores)) + geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) + labs(title = 'Distribution of Previous Scores', x = 'Previous.Scores', y = 'Frequency') + theme_minimal()

hist_sleep_hours <- ggplot(df, aes(x = Sleep.Hours)) + geom_histogram(binwidth = 1, fill = "green", color = "black", alpha = 0.7) + labs(title = 'Distribution of Sleep Hours', x = 'Sleep.Hours', y = 'Frequency') + theme_minimal()

hist_sample_question <- ggplot(df, aes(x = Sample.Question.Papers.Practiced)) + geom_histogram(binwidth = 1, fill = "orange", color = "black", alpha = 0.7) + labs(title = 'Distribution of Sample Question.Papers.Practiced', x = 'Sample Question Papers Practiced', y = 'Frequency') + theme_minimal()

hist_performance_index <- ggplot(df, aes(x = Performance.Index)) + geom_histogram(binwidth = 5, fill = "purple", color = "black", alpha = 0.7) + labs(title = 'Distribution.of.Performance.Index', x = 'Performance Index', y = 'Frequency') + theme_minimal()

# Display all distribution graphs
grid.arrange(hist_hours_studied, hist_previous_scores, hist_sleep_hours,hist_sample_question, hist_performance_index, ncol = 2)


# Confirm whether to perform polynomial regression or multivariate linear regression
linear_model <- lm(Performance.Index ~ Hours.Studied + Previous.Scores + Extracurricular.Activities + Sleep.Hours + Sample.Question.Papers.Practiced, data = df)
plot(linear_model$fitted.values, rstandard(linear_model), xlab = "Fitted Values", ylab = "Standardized Residuals",main = "Residual Plot")
abline(h = 0, col = "red", lwd = 2)


# Best Subset Selection
# Use regsubsets function for Best Subset Selection
best_subset <- regsubsets(Performance.Index ~., df)
summary(best_subset)
best.summary <- summary(best_subset)
names(best.summary)
best.summary$rsq
best_features <- best.summary$which[which.max(best.summary$adjr2), ]
best_features
plot(best_subset,scale="Cp")
plot(best_subset,scale="bic")

# parameteres estimation
model_best <- lm(Performance.Index ~ Hours.Studied + Previous.Scores + Extracurricular.Activities + Sleep.Hours + Sample.Question.Papers.Practiced, data = df)
summary(model_best)
coefs <- summary(model_best)$coefficients
coefs

# anova test 
full_model <- lm(Performance.Index ~ Hours.Studied + Previous.Scores + Extracurricular.Activities + Sleep.Hours + Sample.Question.Papers.Practiced, data = df)
reduced_model <- lm(Performance.Index ~ Hours.Studied + Previous.Scores + Sample.Question.Papers.Practiced, data = df)
anova_result <- anova(reduced_model, full_model)
print(anova_result)

# model fittness by data visulization
df$predicted <- predict(linear_model, df)
ggplot(df, aes(x = Performance.Index, y = predicted)) + geom_point(color = 'blue') + geom_abline(intercept = 0, slope = 1, color = 'red', linetype = "dashed") + labs(x = 'Actual Performance Index', y = 'Predicted Performance Index', title = 'Actual vs Predicted Performance Index') + theme_minimal()


  
 









