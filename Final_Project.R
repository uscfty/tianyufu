
# 加载必要的库
library(ggplot2)
library(leaps)
library(car)
library(stats)

# 读取CSV文件
df <- read.csv("/Users/futianyu/Desktop/数理统计/STAT 8120/Final Project/Student_Performance.csv")

# 将分类变量转换为数值变量
df$Extracurricular.Activities <- ifelse(df$Extracurricular.Activities == "Yes", 1, 0)

# 查看数据的前几行
head(df)

# 确认是进行多项式回归还是多元线性回归

linear_model <- lm(Performance.Index ~ Hours.Studied + Previous.Scores + Extracurricular.Activities + Sleep.Hours + Sample.Question.Papers.Practiced, data = df)

plot(linear_model$fitted.values, rstandard(linear_model), xlab = "Fitted Values", ylab = "Standardized Residuals",main = "Residual Plot")
abline(h = 0, col = "red", lwd = 2)


# Best Subset Selection

# 使用 regsubsets 函数进行 Best Subset Selection
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

library(ggplot2)

df$predicted <- predict(linear_model, df)

ggplot(df, aes(x = Performance.Index, y = predicted)) + geom_point(color = 'blue') + geom_abline(intercept = 0, slope = 1, color = 'red', linetype = "dashed") + labs(x = 'Actual Performance Index', y = 'Predicted Performance Index', title = 'Actual vs Predicted Performance Index') + theme_minimal()


  
 









