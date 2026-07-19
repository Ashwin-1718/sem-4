
# Hypothesis Testing Sigma-Known -----------------------------------------------------
# We use normal distribution for sigma known case
# for lower Tail example as an argument we use lower.tail = T
# for upper tail example as an argument we use lower.tail = F
# There will be 5 steps to calculate hypothesis
# Step 1 : hypothesis 
# Step 2 : Level of significance
# Step 3 : Test Statistic(z calculated)
# Step 4 : P-value Approach(Use = pnorm)
# Step 5 : Critical Value Approach(Use = qnorm)
# less than (in alternative hypothesis)lower tail 

# LOWER TAIL EXAMPLE Hypothesis testing sigma known case
# lower tail zcalculated always negative value
# Consider the following hypothesis A sample of 50 provide a sample mean 19.4
# and population standard deviation is 2 using alpha = 0.05 what is the conclusion based on 
# p value approach and critical value approach

n=50 #sample size
xbar = 19.4 # sample mean
sigma = 2 #population standard deviation
alpha = 0.05 # level of significance
mu0 = 20 #Hypothized value
# Step 1 :- Hypothesis
# H0 : mu >= 20
# Ha : mu < 20

#Step 2:- Level of Significance
alpha = 0.05 

#Step 3:- Test Statistic
SE = sigma / sqrt(n)
Zcal = (xbar - mu0)/ SE
Zcal

#Step 4:- Pvalue approach
pvalue = pnorm(Zcal,lower.tail = T)
pvalue

#Conclusion 
#pvalue = 0.016 <= alpha 0.05 
#reject H0
#Step 5:- Critical Value Approach
Zalpha = qnorm(alpha,lower.tail = T);Zalpha
# Conclusion : Zcal =-2.12 <= Zalpha =-1.64
# Reject H0

# Consider the following hypothesis A sample of 36 provide a sample mean 2.92
# and population standard deviation is 0.18 using alpha = 0.05 what is the conclusion based on 
# p value approach and critical value approach
# H0 mu >= 3
# Ha mu < 3

n= 36 # Sample Size 
xbar = 2.92 # Sample mean
sigma = 0.18 # Population Standard Deviation
alpha = 0.05 # level of significance
mu0 = 3 # Hypo Value

# Step 1: Hypothesis
# H0: mu >= 3
# Ha: mu < 3

# Step 2: level of significance
alpha = 0.05

# Step 3: Test Statistic
SE = sigma/sqrt(n)
Zcal = (xbar - mu0)/SE
Zcal

# Step 4: P value approach
pvalue = pnorm(Zcal,lower.tail = T);pvalue

#Conclusion - pvalue =0.003 < alpha=0.05
# Reject H0

#Step 5 :Critical Value Approach
Zalpha =qnorm(alpha,lower.tail = T);Zalpha

#Conclusion: Zcal = -2.67 <= Zalpha = -1.64

# UPPER TAIL EXAMPLE
# PDF PAGE 403 # Example 10 

n = 40
xbar = 26.4
sigma = 6
alpha = 0.01
mu0 = 25
# Step 1:
#H0: mu <= 25
#Ha: mu > 25

# Step 2:
alpha = 0.01

# Step 3:
SE = sigma / sqrt(n);SE
Zcal = (xbar - mu0)/SE ;Zcal

# Step 4:
pvalue = pnorm(Zcal,lower.tail = F);pvalue

# Conclusion: pvalue = 0.07 !<= alpha =0.01(not less than equal to)
# We dont reject H0
# Step 5:
Zalpha = qnorm(alpha,lower.tail = F);Zalpha

# conclusion: Zcal = 1.47 !<= zalpha=2.326
# Do not reject H0



#TWO TAIL  EXAMPLE

# negative - true 
# positive - false
# PDF PAGE 403 # EG 11

n=50
xbar=14.15
sigma = 3
alpha = 0.05
mu0 = 15

# Step 1
# H0 : mu = 15
# Ha : mu != 15

# Step 2:
alpha = 0.05

# Step 3:
SE = sigma / sqrt(n) ;SE
Zcal = (xbar - mu0)/SE ;Zcal

# Step 4:
pvalue = pnorm(Zcal, lower.tail = T)
pvalue

# pvalue = 0.022 <= alpha/2 = 0.05/2 = 0.025 
#Reject  H0

# Step 5:
Zalpha = qnorm(alpha/2,lower.tail = T);Zalpha

# Zcal = -2.00 <= Zalpha =-1.96
# So we reject H0



# CREATING HYPOTHESIS BY OURSELVES--------------

# Page 404 
# Example 15


n=400
xbar = 910
sigma = 1600
alpha =0.05
mu0 = 1056

# step 1 :
# H0 : mu >= 1056
# Ha : mu < 1056

# step 2:
alpha = 0.05 

#step 3:
SE = sigma / sqrt(n);SE
Zcal = (xbar - mu0)/SE ;Zcal

#step 4:
pvalue = pnorm(Zcal , lower.tail = T);pvalue

#con : pvalue = 0.034 <= alpha =0.05
# so we reject the null hypothesis

#step 5:
Zalpha = qnorm(alpha,lower.tail = T);Zalpha

# conc: Zcal = -1.82 <= Zalpha -1.644
# so we reject H0


# NO 15 # FOR CHECKING 
z.test(x = xbar ,mu = mu0 ,stdev = sigma ,alternative ="less", n = n , conf.level = 1-alpha)
