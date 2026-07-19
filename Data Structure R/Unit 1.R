# Normal Distribution

pnorm(2.5)

# -------------------------------------------------------------------------


 # Unit 1 

# Sigma Known case

# Q1 a simple random sample of 40 size with sample mean 25 and population stadard deviation is 5.
# 1 what is standard error 
# 2 what is margin of error 
# 3 compute 95% confidence interval 

n = 40
xbar = 25
sigma = 5
alpha = 0.05

#Q1
se = sigma / sqrt(n) ;se

#Q2
Zalpha = qnorm(alpha / 2, lower.tail = FALSE) ;Zalpha

#Q3
me = Zalpha * se ;me

CI = c(xbar - me, xbar + me) ;CI


# -------------------------------------------------------------------------

# 8.1 Population Mean: σ Known
# Q2
# Given values
n <- 50
sigma <- 6
x_bar <- 32

# Standard Error
SE <- sigma / sqrt(n)

# Confidence levels
confidence_levels <- c(0.90, 0.95, 0.99)

# Loop through each confidence level
for (conf in confidence_levels) {
  alpha <- 1 - conf
  z <- qnorm(1 - alpha / 2)  # Z-value
  margin_of_error <- z * SE
  lower_bound <- x_bar - margin_of_error
  upper_bound <- x_bar + margin_of_error
  cat(sprintf("\n%.0f%% Confidence Interval: [%.3f, %.3f]", conf*100, lower_bound, upper_bound))
}

# Q4, 5, 7 ,8


# Hypothesis Testing Sigma Known Case --------------------------------------

# We use Normal Distribution for sigma know case
# For lower tail example as an argument we use lower.tail = T
# For upper tail example as an argument we use lower.tail = False
# There will 5 step to calculate hypothesis
# Step 1 : Hypothesis 
# Step 2 : Level of Significance
# Step 3: Test statistics (z calculated)
# Step 4: P value approach (use = p norm)
# step 5 : Critical value approach (use = q norm)

# Lower Tail Example  --------------------------------------
# Consider the following hypothesis a sample of 50 provide a sample mean 19.4 and population standard is 2 using alpha = 0.05 what is the conclusion based on p value approach and critical value approach. 


n = 50 # sample size
xbar = 19.4  # sample mean
sigma = 2  # Population Standard Deviation
alpha = 0.05  # Level of Significance
mu0 = 20 # hypothesis value

# Step No:1 Hypothesis 
# H0 : mu >= 20
# Ha : mu <= 20

# Step No:2 Level Of Significance
alpha = 0.05

# Step NO:3 Test Statistics
SE = sigma/sqrt(n); SE
Zcal = (xbar - mu0) / SE
print(Zcal)

# Lowe Tail always zcal negative value

# Step 4: 
pvalue = pnorm(Zcal, lower.tail = TRUE)
print(pvalue)

# Conclusion p value = 0..01 <= alpha = 0.05
# Reject H0

# Step 5: Critical value approach
Zalpha = qnorm(alpha, lower.tail = TRUE)
print(Zalpha)

# Reject H0


# Lowe Tail Example
# Consider the following hypothesis a sample of 36 provide a sample mean 2.92 and population standard is 0.18 using alpha = 0.05 what is the conclusion based on p value approach and critical value approach. 

# hypothesis 
#H0 mu >= 3  
#H0 mu <= 3  

n = 36 # sample size
xbar = 2.92  # sample mean
sigma = 0.18  # Population Standard Deviation
alpha = 0.05  # Level of Significance
mu0 = 3 # hypothesis value

# Step No:2 Level Of Significance
alpha = 0.05

# Step NO:3 Test Statistics
SE = sigma/sqrt(n); SE
zcal = (xbar - mu0) / SE
print(Zcal)

# Step 4: 
pvalue = pnorm(zcal, lower.tail = TRUE)
print(pvalue)

# Conclusion p value = 0..01 <= alpha = 0.05
# Reject H0

# Step 5: Critical value approach
Zalpha = qnorm(alpha, lower.tail = TRUE)
print(Zalpha)
