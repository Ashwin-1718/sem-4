
# Sigma Unknown case ------------------------------------------------------


# Important Note

# There are only 5 Steps
# Step 1 : Hypothesis 

# Step 2 : Level of Significance

# Step 3: Test statistics (z calculated)

# Step 4: P value approach (use = p norm)
# (pt = peob & dist)

# step 5 : Critical value approach (use = q norm)



# Example no: 1 (Page no: 409)

n = 36 # sample sizez
xbar = 44  # sample mean
s = 5.2
alpha = 0.01  # Level of Significance
mu0 = 45 # hypothesis value

# Step No:2 Level Of Significance
alpha = 0.01

# Step NO:3 Test Statistics
# Step 3 (fix): use t-statistic name
SE   = s/sqrt(n)
tcal = (xbar - mu0) / SE
tcal


# Step 4: P value approach
pvalue = pt(tcal, df = n-1,lower.tail = TRUE)
print(pvalue)

# Conclusion p value = 0..01 <= alpha = 0.05
# Reject H0

# Step 5: Critical value approach
Zalpha = qnorm(alpha, lower.tail = TRUE)
print(Zalpha)


# New ---------------------------------------------------------------------


n = 36
mu0 = 45
df = n - 1    # 35
alpha = 0.01

#A
xbar = 44
s = 5.2
SE = s / sqrt(n)                     # = 5.2 / 6 = 0.8666667
tcal = (xbar - mu0) / SE             # = (44 - 45) / 0.8666667 = -1.1538462
pvalue = pt(tcal, df = df, lower.tail = TRUE)   # ≈ 0.1282

#B
xbar = 43
s = 4.6
SE = s / sqrt(n)                     # = 4.6 / 6 = 0.7666667
tcal = (xbar - mu0) / SE             # = (43 - 45) / 0.7666667 ≈ -2.6086957
pvalue = pt(tcal, df = df, lower.tail = TRUE)   # ≈ 0.0066388



# Example 23 --------------------------------------------------------------

#example 23 (ch9 same page)
#uper tail
n=25 #sample size
xbar=14#sample mean
s=4.32# sample sd
alpha=0.05#lever of sigm
mu0=12#hypo. value



#step 1: hypothesis
#Ho:mu<=12
#Ha:mu>12



#step 2: level significhen
alpha=0.05


#step 3 : test stetisic
SE=s/sqrt(n) ;SE

tcal=(xbar-mu0)/SE
tcal

#step 4 : p value approch
pval=pt(tcal,df=n-1,lower.tail = T)
pval
#Conclusion
#p value=0.0066 !<= alpha=0.001
#do not reject Ho

#step 5:critical value approch
talpha=qt(alpha,df=n-1,lower.tail = T)
talpha

#Conclusion
#tcal=-2.60 !<= talpha =-2.43
#do not reject Ho  


# Example 24 --------------------------------------------------------------
#example 23 (ch9 same page)
#uper tail
n=48 #sample size
xbar=17#sample mean
s=4.5# sample sd
alpha=0.05#lever of sigm
mu0=18#hypo. value



#step 1: hypothesis
#Ho:mu<=12
#Ha:mu>12



#step 2: level significhen
alpha=0.05


#step 3 : test stetisic
SE=s/sqrt(n) ;SE

tcal=(xbar-mu0)/SE
tcal

#step 4 : p value approch
pval=pt(tcal,df=n-1,lower.tail = T)
pval
#Conclusion
#p value=0.0066 !<= alpha=0.001
#do not reject Ho

#step 5:critical value approch
talpha=qt(alpha,df=n-1,lower.tail = T)
talpha

#Conclusion
#tcal=-2.60 !<= talpha =-2.43
#do not reject Ho
