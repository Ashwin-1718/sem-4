
# Sigma unknown -------------------------------------------------------------

#we use T distribution with n - 1 degrees of freedom
#we use sample standard deviation(s) instead of sigma
talpha = qt(prob , dt= n-1 , lower.tail = FALSE)


#EXAMPLE 
# sample mean = 82
# sample standard deviation = 20
# sample size = 100
# develop 95% confidence interval

xbar = 82
n = 100
s = 20
alpha = 0.05

SE = s/sqrt(n)
talpha = qt(alpha/2,df = n-1,lower.tail = FALSE)
ME = talpha * SE ; ME
CI = c(xbar-ME,xbar + ME)
CI

# EXAMPLE 
# A simple random sample with size 54 provided a sample mean 22.5 and sample standard deviation 4.4 develop 90% , 95% and 99%

xbar = 22.5
n = 54
s = 4.4
 #1) 

alpha = 0.9
SE = s /sqrt(n)
talpha = qt(alpha/2,df=n-1,lower.tail=FALSE)
ME = talpha * SE ;ME
CI = c(xbar - ME , xbar + ME)
CI

#2)
alpha = 0.05
SE = s /sqrt(n)
talpha = qt(alpha/2,df=n-1,lower.tail=FALSE)
ME = talpha * SE ;ME
CI = c(xbar - ME , xbar + ME)
CI

#3) 
alpha = 0.01
SE = s /sqrt(n)
talpha = qt(alpha/2,df=n-1,lower.tail=FALSE)
ME = talpha * SE ;ME
CI = c(xbar - ME , xbar + ME)
CI


# The following sample data from the population
# 10 ,8 , 12,15,13,11,6,5

#1) point estimate for population mean? xbar
#2)point estimate of the population standard deviation ?sigma 
#3) develop 95% confidence Interval ?CI

data = c(10,8,12,15,13,11,6,5)
xbar = mean(data);xbar
s = sd(data);s
n = length(data);n
alpha = 0.05

SE = s /sqrt(n)
talpha = qt(alpha/2,df=n-1,lower.tail=FALSE)
ME = talpha * SE ;ME
CI = c(xbar - ME , xbar + ME)
CI



# Example 17 page 359 -----------------------------------------------------

data = c(6, 4, 6, 8, 7 ,7, 6, 3, 3, 8, 10, 4, 8,
         7, 8, 7, 5, 9, 5, 8, 4, 3, 8, 5, 5, 4,
         4, 4, 8, 4, 5, 6, 2, 5, 9, 9, 8, 4, 8,
         9, 9,5, 9, 7, 8, 3, 10, 8, 9, 6)
xbar = mean(data);xbar
s = sd(data);s
n =length(data);n
SE = s / sqrt(n) ;SE
talpha = qt(alpha/2,df=n-1 , lower.tail = FALSE);talpha
ME = talpha * SE ;ME
CI = c(xbar - ME , xbar + ME);CI



# INTERVAL ESTIMATION OF POPULATION PROPRTION -----------------------------

# Example (pdf page 367)
# a simple random sample of 400 individual provided 100 YES response 
# 1) What is the point estimate of the proportion of the population that would have provided YES responds
 # 2) Develop 95% confidence interval for the population proportion


x = 100
n = 400
pbar = x/n
alpha = 0.05
SE = sqrt((pbar * (1-pbar))/n);SE
Zalpha = qnorm(alpha/2 , lower.tail = FALSE)
ME = Zalpha * SE ;ME
CI = c(pbar - ME , pbar + ME) ;CI

