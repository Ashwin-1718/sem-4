
# Unit-1 Interval Estimation (Sigma known case) -----------------------------------------------------

#Question 1 :- A simple random sample of 40 size with 
#sample mean 25 and population standard deviation is 5
#1)What is standard error?
#2)what is margin of error?
#3)Compute 95% confidence interval
# 1-95 = 5% = 0.05
n = 40
xbar = 25
sigma = 5
alpha = 0.05
SE = sigma/sqrt(n);SE # Standard error
ME = #z alpha / 2 * (SE)
Zalpha = qnorm(alpha/2,lower.tail=F)
Zalpha
ME = Zalpha * SE ;ME
CI = c(xbar - ME , xbar + ME)
CI
