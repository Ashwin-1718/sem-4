# Inference about 2 Population Mean ---------------------------------------

# Interval Estimation about 2 population mean
# Sigma Known case
# Ch 10

# Example 1
n1 = 36
n2 = 49
x1bar = 40
x2bar = 35
sigma1 = 9
sigma2 = 10
alpha = 0.05

# Point Estimation
PE = x1bar - x2bar
PE

zalpha = qnorm(alpha/2, lower.tail = FALSE)
zalpha

se = sqrt(sigma1^2/n1) + (sigma2^2/n2)
se

me = zalpha * se
me

Interval = c(PE - me, PE+me)
Interval


# Example 1 CH:10 page no 412 ---------------------------------------------
# ex : 1 ch 10 page 412 


n1 = 50
n2 = 35
x1bar = 13.6
x2bar = 11.6
sigma1 = 2.2 
sigma2 = 3.0
# 90 % and 95 % Interval
alpha = 0.10

# for 90 % 


PE = x1bar - x2bar
print(PE)

zalpha = qnorm(alpha/2, lower.tail = FALSE)
print(zalpha)

SE = sqrt((sigma1^2/n1) + (sigma2^2/n2))
print(SE)

ME = zalpha * SE 
print(ME)

interval = c(PE-ME,PE+ME)
print(interval)


# Hypothesis Testing for 2 Population mean for sigma konwn case ---------------

# Upper tail Example:-2 Ch:-10 PG NO : 413
# Example

n1 = 40
n2 = 50
x1bar = 25.2
x2bar = 22.8
sigma1 = 5.2
sigma2 = 6.0
alpha = 0.05

# Step 1: Hypothesis
# H0: mu1 - mu2 <= 0
# Ha: mu1 - mu2 > 0

# Step 2: Level of Significance
alpha = 0.05

# Step 3: Calculate Standard Error
se = sqrt((sigma1^2 / n1) + (sigma2^2 / n2))
cat("Standard Error =", se, "\n")

# Step 4: Calculate Test Statistic (z)
z = (x1bar - x2bar) / se
cat("Test Statistic (z) =", z, "\n")


# Step 5: Calculate P-value (one-tailed test)
p_value = 1 - pnorm(z)
cat("P-value =", p_value, "\n")

# Step 6: Conclusion
if (p_value < alpha) {
  cat("Reject the null hypothesis: Evidence suggests mu1 - mu2 > 0\n")
} else {
  cat("Fail to reject the null hypothesis: Insufficient evidence that mu1 - mu2 > 0\n")
}
