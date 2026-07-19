# Examples pdf page 350 ----------------------------------------------------
#2) A simple random sample of 50 items from a population with σ 6 resulted in a sample
#mean of 32.
#a.
#Provide a 90% confidence interval for the population mean.
#b. Provide a 95% confidence interval for the population mean.
#c.Provide a 99% confidence interval for the population mean.

n = 50
xbar = 32
sigma = 6
alpha = 0.10

#Why α = 0.10? 
#Because 90% => α = 1 - 0.90 = 0.10

#Standard error
SE = sigma / sqrt(n); SE

#Z critical value
Zalpha = qnorm(alpha/2, lower.tail = F)
Zalpha

#Margin of error
ME = Zalpha * SE ; ME

# Confidence Interval
CI90 = c(xbar - ME , xbar + ME)
CI90

#b) 95% Confidence Interval
alpha = 0.05
SE = sigma / sqrt(n); SE
Zalpha = qnorm(alpha/2, lower.tail = F); Zalpha
ME = Zalpha * SE ; ME
CI95 = c(xbar - ME , xbar + ME)
CI95

#c) 99% Confidence Interval
alpha = 0.01
SE = sigma / sqrt(n); SE
Zalpha = qnorm(alpha/2, lower.tail = F); Zalpha
ME = Zalpha * SE ; ME
CI99 = c(xbar - ME , xbar + ME)
CI99


# -------------------------------------------------------------------------


# 31. A simple random sample of 400 individuals provides 100 Yes responses.
#a.What is the point estimate of the proportion of the population that would provide Yes responses?
#b. What is your estimate of the standard error of the proportion, ?σp¯ 
#C. Compute the 95% confidence interval for the population proportion. 

# 31. Sample of 400 individuals, 100 Yes responses.
# a) Point estimate of population proportion
# b) Standard error of proportion
# c) 95% confidence interval for population proportion

n = 400         # sample size
x = 100         # number of Yes responses

phat = x / n ; phat     # a) point estimate of population proportion

# b) Standard error of proportion
SE = sqrt(phat * (1 - phat) / n) ; SE

# c) 95% confidence interval for population proportion
alpha = 0.05
Zalpha = qnorm(alpha/2, lower.tail = FALSE) ; Zalpha   # Z for 95% CI (≈ 1.96)

ME = Zalpha * SE ; ME   # Margin of error

CI = c(phat - ME, phat + ME)
CI



# Random Data -------------------------------------------------------------

# 17. Miami International Airport ratings (max = 10)
# Develop a 95% confidence interval estimate of the population mean rating.

ratings = c(
  6, 4, 6, 8, 7, 7, 6, 3, 3, 8, 10, 4, 8,
  7, 8, 7, 5, 9, 5, 8, 4, 3, 8, 5, 5, 4,
  4, 4, 8, 4, 5, 6, 2, 5, 9, 9, 8, 4, 8,
  9, 9, 5, 9, 7, 8, 3, 10, 8, 9, 6
)

xbar = mean(ratings); xbar      # point estimate of population mean
s = sd(ratings); s              # point estimate of population SD
n = length(ratings); n          # sample size
alpha = 0.05                    # 95% confidence

SE = s / sqrt(n); SE            # standard error of the mean

talpha = qt(alpha/2, df = n-1, lower.tail = FALSE); talpha  # t critical value

ME = talpha * SE; ME            # margin of error

CI = c(xbar - ME, xbar + ME); CI   # 95% confidence interval

# -------------------------------------------------------------------------

# 35. Consumer Reports survey on future health of Social Security
# n = 2000 adults, x = 1760 say it's a major concern

n = 2000
x = 1760

# a) Point estimate of population proportion
phat = x / n ; phat    # p-hat = 1760/2000 = 0.88

# b) Margin of error at 90% confidence
alpha = 0.10
SE = sqrt(phat * (1 - phat) / n) ; SE      # standard error of p-hat

Zalpha = qnorm(alpha/2, lower.tail = FALSE) ; Zalpha   # Z for 90% CI (≈ 1.645)
ME_90 = Zalpha * SE ; ME_90     # margin of error at 90% confidence

# c) 90% confidence interval for population proportion
CI_90 = c(phat - ME_90, phat + ME_90) ; CI_90

# d) 95% confidence interval for population proportion
alpha = 0.05
Zalpha = qnorm(alpha/2, lower.tail = FALSE) ; Zalpha   # Z for 95% CI (≈ 1.96)
ME_95 = Zalpha * SE ; ME_95

CI_95 = c(phat - ME_95, phat + ME_95) ; CI_95


# 2 Sample Known Case -----------------------------------------------------------

# Two population mean (sigma known case)

n1 = 50
n2 = 35
x1bar = 13.6
x2bar = 11.6
sigma1 = 2.2
sigma2 = 3.0

# Point estimate
PE = x1bar - x2bar ; PE

# 90% CI
alpha = 0.10
Zalpha = qnorm(alpha/2, lower.tail = FALSE) ; Zalpha
SE = sqrt((sigma1^2/n1) + (sigma2^2/n2)) ; SE
ME = Zalpha * SE ; ME
CI_90 = c(PE - ME , PE + ME) ; CI_90

# 95% CI
alpha = 0.05
Zalpha = qnorm(alpha/2, lower.tail = FALSE) ; Zalpha
SE = sqrt((sigma1^2/n1) + (sigma2^2/n2)) ; SE
ME = Zalpha * SE ; ME
CI_95 = c(PE - ME , PE + ME) ; CI_95

# 2 sample unknown case ---------------------------------------------------
# Two population mean (sigma unknown case)

x1 = c(300,280,344,385,372,360,288,321,376,290,301,283)
x2 = c(274,220,308,336,198,300,315,258,318,310,332,263)

n1 = length(x1)
n2 = length(x2)
x1bar = mean(x1)
x2bar = mean(x2)
v1 = var(x1)
v2 = var(x2)
df = n1 + n2 - 2 ; df
alpha = 0.05

# Point estimate
PE = x1bar - x2bar ; PE

# Standard error
SE = sqrt((v1/n1) + (v2/n2)) ; SE

# t critical value
talpha = qt(alpha/2, df, lower.tail = FALSE) ; talpha

# Margin of error
ME = talpha * SE ; ME

# 95% Confidence interval
CI = c(PE - ME , PE + ME) ; CI

# -------------------------------------------------------------------------

# Hypothesis testing for two population mean (sigma known case)

n1 = 50
n2 = 40
x1bar = 250.2
x2bar = 22.8
sigma1 = 5.2
sigma2 = 6.0
alpha = 0.05

# Step 1 Hypothesis
# H0 : mu1 - mu2 <= 0
# Ha : mu1 - mu2 > 0    # upper tail test

# Step 2 Level of significance
alpha = 0.05

# Step 3 Test statistic
SE = sqrt((sigma1^2/n1) + (sigma2^2/n2)) ; SE
zcal = (x1bar - x2bar) / SE ; zcal       # test statistic

# Step 4 p-value approach
pvalue = pnorm(zcal, lower.tail = FALSE) ; pvalue

# Step 5 Critical value approach
zalpha = qnorm(alpha, lower.tail = FALSE) ; zalpha

# Conclusion based on p-value and critical value
# If pvalue <= alpha → Reject H0
# If zcal >= zalpha → Reject H0


