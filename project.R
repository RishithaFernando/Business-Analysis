# Load the dataset
prestige_data <- read.csv("Prestige_New.csv")

# Income Statistics Calculation
income_stats <- list(
  min = min(prestige_data$income, na.rm = TRUE),
  max = max(prestige_data$income, na.rm = TRUE),
  mean = mean(prestige_data$income, na.rm = TRUE),
  median = median(prestige_data$income, na.rm = TRUE)
)

# Custom function for calculating the mode
calculate_mode <- function(values) {
  as.numeric(names(sort(-table(values)))[1])
}

# Applying mode function for income
income_stats$mode <- calculate_mode(prestige_data$income)

# Output the income statistics
cat("Income Statistics:\n",
    "Minimum:", income_stats$min, "\n",
    "Maximum:", income_stats$max, "\n",
    "Mean:", income_stats$mean, "\n",
    "Median:", income_stats$median, "\n",
    "Mode:", income_stats$mode, "\n")

# Summary statistics for prestige, education, and income
print(summary(prestige_data$prestige))
print(summary(prestige_data$education))
print(summary(prestige_data$income))

# Central tendency for prestige, education, income
central_tendency_data <- data.frame(
  Variable = c("Prestige", "Education", "Income"),
  mean = c(mean(prestige_data$prestige), mean(prestige_data$education), mean(prestige_data$income)),
  mode = c(
    calculate_mode(prestige_data$prestige),
    calculate_mode(prestige_data$education),
    income_stats$mode
  ),
  median = c(median(prestige_data$prestige), median(prestige_data$education), income_stats$median)
)
print(central_tendency_data)

# Plot bell curves with adjusted colors
plot_bell_curve <- function(var, mean_val, median_val, mode_val, label) {
  hist(var, prob = TRUE, main = paste(label, "Distribution with Mean, Median, and Mode"),
       xlab = label, col = "#EEDC82", border = "#8B2323")  # Adjusted color
  
  lines(density(var), col = "black", lwd = 2)
  abline(v = mean_val, col = "#FF6666", lty = 2, lwd = 2)   # Adjusted color
  abline(v = median_val, col = "#66CC66", lty = 2, lwd = 2)  # Adjusted color
  abline(v = mode_val, col = "#6699FF", lty = 2, lwd = 2)    # Adjusted color
  
  legend("topright", legend = c("Mean", "Median", "Mode"),
         col = c("#FF6666", "#66CC66", "#6699FF"), lty = 2, lwd = 2)  # Adjusted color
}

# Plots
plot_bell_curve(prestige_data$education, mean(prestige_data$education), 
                median(prestige_data$education), calculate_mode(prestige_data$education), "Education")
plot_bell_curve(prestige_data$income, income_stats$mean, income_stats$median, 
                income_stats$mode, "Income")
plot_bell_curve(prestige_data$prestige, mean(prestige_data$prestige), 
                median(prestige_data$prestige), calculate_mode(prestige_data$prestige), "Prestige")

# Shapiro-Wilk Normality Tests
cat("Shapiro-Wilk Test Results:\n")
print(shapiro.test(prestige_data$prestige))
print(shapiro.test(prestige_data$education))
print(shapiro.test(prestige_data$income))

# ANOVA for Prestige by Occupation Type
prestige_data$type <- factor(prestige_data$type)
anova_prestige <- aov(prestige ~ type, data = prestige_data)
print(summary(anova_prestige))

# Tukey's HSD post-hoc test
tukey_result <- TukeyHSD(anova_prestige)
print(tukey_result)

# Boxplot: Prestige by Occupation Type
boxplot(prestige ~ type, data = prestige_data, main = "Prestige by Occupation Type",
        xlab = "Occupation Type", ylab = "Prestige", col = "#FFFF00", border = "#B03060") 

# Simple Linear Regression: Prestige ~ Education
lm_prestige_education <- lm(prestige ~ education, data = prestige_data)
print(summary(lm_prestige_education))

# Scatterplot of Education vs. Prestige with regression line
plot(prestige_data$education, prestige_data$prestige, 
     main = "Education vs Prestige",
     xlab = "Education",
     ylab = "Prestige",
     pch = 17,   # Changed point character
     col = "#6699FF")  # Adjusted color
abline(lm_prestige_education, col = "#FF6666", lwd = 2)  # Adjusted color

# Pearson's Correlation: Income and Prestige
correlation_income_prestige <- cor.test(prestige_data$income, prestige_data$prestige)
print(correlation_income_prestige)

# Scatterplot of Income vs. Prestige with regression line
plot(prestige_data$income, prestige_data$prestige,
     main = "Prestige vs Income",
     xlab = "Income",
     ylab = "Prestige",
     pch = 18,   # Changed point character
     col = "#D02090")  # Adjusted color
abline(lm(prestige ~ income, data = prestige_data), col = "#FF6347", lwd = 2)  

# Pearson's Correlation: Prestige and Women
correlation_prestige_women <- cor.test(prestige_data$prestige, prestige_data$women)
print(correlation_prestige_women)

# Scatterplot of Women vs. Prestige with regression line
plot(prestige_data$women, prestige_data$prestige,
     main = "Prestige vs Women",
     xlab = "Women",
     ylab = "Prestige",
     pch = 15,   # Changed point character
     col = "#66CC66")  # Adjusted color
abline(lm(prestige ~ women, data = prestige_data), col = "#FF6347")

# Multiple Linear Regression of Prestige on Education, Income, and Women
multi_model <- lm(prestige ~ education + income + women, data = prestige_data)
print(summary(multi_model))
