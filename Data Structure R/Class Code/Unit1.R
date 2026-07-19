# Unit 1 Interval estimation
# sigma known case

# page no 350

# Q1
#A simple random sample of 40 size with sample mean 25 and population s.d is

#1. what is s.d error 
#2. what is margin error 
#3. compute 95% confidence interval

n = 40
xbar = 25
sigma = 5
alpha = 0.05 # 1 - 95% 

SE= sigma/sqrt(n);SE #1st ans

zalpha = qnorm (alpha/2,lower.tail = F);zalpha #2nd ans

me = zalpha*SE

CI = c(xbar-me , xbar+me);CI


#--------------------------------------------------------------


#Q2

# 1 Provide a 90% confidence interval for the population mean.
# 2 Provide a 95% confidence interval for the population mean.
# 3 Provide a 99% confidence interval for the population mean.


n = 50
xbar = 32
sigma = 6 
#alpha = 0.1 # 1 - 90% 1st Q
#alpha = 0.05 # 1 - 95% 2st Q
alpha = 0.01 # 1 - 99% 3st Q


SE = sigma/sqrt(n) ; SE
zalpha = qnorm(alpha/2,lower.tail = F) ;zalpha
me = zalpha * SE 

CI = c(xbar - me , xbar + me);CI

#------------------------------------------------------------------

# Q3

# a. Compute the 95% confidence interval for the population mean.
# b. Assume that the same sample mean was obtained from a sample of 120 items. Providea 95% confidence interval for the population mean.
# c.What is the effect of a larger sample size on the interval estimate?

n = 60
xbar = 80
sigma = 15
#alpha = 0.05 # 1 - 95% (a)
alpha = 0.05 # 1 - 95% (a)


SE = sigma / sqrt(n); SE
zalpha = qnorm(alpha/2);zalpha
ME = zalpha * SE

CI = c(xbar - ME , xbar + ME);CI

#------------------------------------------------


# Q4
sigma = 15
me = 4
alpha = 0.05 # 1 - 95%

zalpha = qnorm(alpha/2,lower.tail = FALSE) ;zalpha
n = (zalpha * sigma / me) ^ 2 ;n
print(round(n))


#-------------------------------------------------

# Q5

n = 49
xbar = 24.80 
sigma = 5
alpha = 0.05 # 1 - 95%

zalpha = qnorm(alpha/2,lower.tail = FALSE) ; zalpha
SE = sigma/sqrt(n) ; SE
ME = zalpha * SE ; ME

CI = c(xbar-ME,xbar+ME) ; CI

#---------------------------------------------------------
 
# Q7
n = 50
sigma = 600
xbar = 1599
alpha = 0.05 # 95%

SE = sigma/sqrt(n) ; SE
zalpha = qnorm(alpha/2,lower.tail = FALSE) ; zalpha
ME = zalpha * SE ; ME
CI = c(xbar-ME , xbar+ME);CI

ME = 150
SE = ME / zalpha ; SE
n = (sigma/SE)^2 ; n


# Q8
#b. Using 95% confidence, what is the margin of error?
#c. What is the margin of error if 99% confidence is desired?

n = 10
xbar = 71
sigma = 5
# alpha = 0.05 # 95%
alpha = 0.01 # 99%

SE = sigma/sqrt(n);SE
zalpha = qnorm(alpha/2,lower.tail = FALSE) ; zalpha
ME = zalpha * SE ; ME


#-------------------------------------------------------------

# Q10

#a.Develop a 90% confidence interval estimate of the population mean.
#b.Develop a 95% confidence interval estimate of the population mean.
#c.Develop a 99% confidence interval estimate of the population mean.

n = 80
xbar = 119155
sigma = 30000
alpha = 0.1 #90%
#alpha = 0.05 #95%
#alpha = 0.01 #99%

SE = sigma/sqrt(n) ; SE
zalpha = qnorm(alpha/2,lower.tail = FALSE) ; zalpha
ME = zalpha * SE ; ME
CI = c(xbar-ME , xbar+ME) ; CI


#------------------------------------------------------------------

# Interval Estimation for population mean - sigma unknown case ------------

# we use T-distribution with n-1 degrees of freedom
# we use sample sd deviation (s) instead of sigma

# EXample 
# sample mean = 82 sample sd = 20 sample size = 100 develop 95% CI

xbar = 82
n = 100
s = 20
alpha = 0.05

SE = s/sqrt(n);SE
talpha = qt(alpha/2,n-1,lower.tail = F);talpha
ME = talpha * SE
CI = c(xbar-ME,xbar+ME);CI

#----------------------------------------------------------------

# a simple random sample with size 54 provided a sample mean 22.5 
# and sample s.d = 4.4 develop 90% , 95% , 99%

n = 54
xbar = 22.5
s = 4.4
#alpha = 0.1 # 90%
#alpha = 0.05 # 95%
alpha = 0.01 # 99%

SE = s/sqrt(n)
talpha = qt(alpha/2,n-1,lower.tail = F)
ME = talpha * SE 
CI = c(xbar-ME , xbar+ME);CI


#----------------------------------------------------

# The following sample data from the population-10,8,12,15,13,11,6,5
# 1 what is the point estimate of the population mean
# 2 what is the point estimate of the population s.d
# 3 develop 95% CI for population mean

data = c(10,8,12,15,13,11,6,5);data
xbar = mean(data)
s = sd(data)
n = length(data)
alpha = 0.05

SE = s/sqrt(n)
talpha = qt(alpha/2,n-1,lower.tail = F)
ME = talpha * SE 
CI = c(xbar-ME , xbar+ME);CI


#---------------------------------------------------------------------

# page 359
# Q17
data =  c(6,4,6,8,7,7,6,3,3,8,10,4,8,7,8,7,5,9,5,8,4,3,8,5,5,4,4,4,8,4,5,6,2,5,9,9,8,4,8,9,9,5,9,7,8,3,10,8,9,6)
xbar = mean(data)
s = sd(data)
n = length(data)
alpha = 0.05

SE = s/sqrt(n)
talpha = qt(alpha/2,n-1,lower.tail = F)
ME = talpha * SE 
CI = c(xbar-ME , xbar+ME);CI


# Interval estimation of population proporstion ---------------------------

#Example
# a simple random sample of 400 indivitual provided 100 yes responce
# 1 what is the point estimate of the propertion of the population that would provided yes responce
# 2 develop 95% CI fpr the population propertion

x = 100
n = 400
pbar = x/n
alpha = 0.05

SE = sqrt((pbar*(1-pbar))/n)
zalpha = qnorm(alpha/2,lower.tail = FALSE)
ME = zalpha*SE
CI = c(pbar-ME,pbar+ME);CI

#------------------------------------------------------------------------

# Example 32
#a.Provide a 90% confidence interval for the population proportion.
#b. Provide a 95% confidence interval for the population proportion.

n = 800
pbar = 70
alpha = 0.1 # 1 - 90%

SE = ((pbar*(1-pbar))/n)
talpha = 



# page no 366 ex 31 



