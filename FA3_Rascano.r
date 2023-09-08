#Formative Assessment #3
#Jan Celine B. Rascano
qrt_data <- c(88, 45, 53, 86, 33, 86, 85, 30, 89, 53, 41, 96, 56, 38, 62,
              71, 51, 86, 68, 29, 28, 47, 33, 37, 25, 36, 33, 94, 73, 46,
              42, 34, 79, 72, 88, 99, 82, 62, 57, 42, 28, 55, 67, 62, 60,
              96, 61, 57, 75, 93, 34, 75, 53, 32, 28, 73, 51, 69, 91, 35)

#Solving the lower quartile or Q1 
lower_qrt <- quantile(qrt_data, 0.25)
print(lower_qrt)

#Solving the middle quartile or Q2
middle_qrt <- quantile(qrt_data, 0.5)
print(middle_qrt)

#Solving the upper quartile or Q3
upper_qrt <- quantile(qrt_data, 0.75)
print(upper_qrt)

#Solving the ninth decile or D9
ninth_dcl <- quantile(qrt_data, 0.9)
print(ninth_dcl)

#Solving the 95th percentile or P95
pct_95 <- quantile(qrt_data, 0.95)
print(pct_95)

# #Solving the Mode
tbl_data <- table(qrt_data)
mode_val <- as.numeric(names(tbl_data[which.max(tbl_data)]))
print(mode_val) 

#Solving the Median
median_val <- median(qrt_data)
print(median_val)

#Solving the Mean
mean_val <- mean(qrt_data)
print(mean_val)

#Solving the Standard Deviation
sd_val <- sd(qrt_data)
print(sd_val)

#Solving the Variance
variance_val <- var(qrt_data)
print(variance_val)

#Solving the Skewness
skewness_val <- skewness(qrt_data)
print(skewness_val)

#Solving the Standard Error of Skewness 
n <- length(qrt_data)
SESkew_val <- sqrt((6 * (n - 2)) / ((n + 1) * (n + 3)))
print(SESkew_val)

#Solving the Kurtosis
kurtosis_val <- kurtosis(qrt_data)
print(kurtosis_val)

#Solving the Standard Error of Kurtosis
n <- length(qrt_data)
SEkurt_val <- sqrt((24 * n * (n - 2) * (n - 3)) / ((n + 1) * (n + 1) * (n + 3) * (n + 5)) - (36 * (n - 1)^2) / ((n - 3) * (n - 2) * (n - 5) * (n - 6)))
print(SEkurt_val)

#Solving the Minimum
min_val <- min(qrt_data)
print(min_val)

#Solving the Maximum
max_val <- max(qrt_data)
print(max_val)

#Summary
summary_tbl <- data.frame(
  Descriptive_Statistic = c("Mode", "Median", "Mean", "Standard Deviation", "Variance", "Skewness", "Standard Error of Skewness", "Kurtosis", "Standard Error of Kurtosis", "Minimum", "Maximum", "Lower Quartile (Q1)", "Middle Quartile (Q2)", "Upper Quartile (Q3)", "Ninth Decile (D9)", "95th Percentile (P95)"),
  Value = c(mode_val, median_val, mean_val, sd_val, variance_val, skewness_val, SESkew_val, kurtosis_val, SEkurt_val, min_val, max_val, lower_qrt, middle_qrt, upper_qrt, ninth_dcl, pct_95)
)
#Rounding off the values to 4 decimal places
summary_tbl[, sapply(summary_tbl, is.numeric)] <- round(summary_tbl[, sapply(summary_tbl, is.numeric)], digits = 4)
print(summary_tbl)