# HYPOTHESIS TESTING; sigma KNOWN CASE ------------------------------------------


#Their will be 5 step to calculate hypothesis
# step 1 Hypothesis
# step 2 Level of significance
# step 3 test statistics(Zcal)
# step 4 P value Approach (p-value Approach , use = pnorm)
# step 5 Critical Value Approach (use = qnorm)


# Lower tail 

# Example 

xbar = 2.92 #sample mean
sigma = 0.18 # population s.d
n = 36 #sample size
alpha = 0.05 #level of sig
mu0 = 3 # hypothised value

#step 1 = Hypothesis

#Ho : mu >= 3
#Ha : mu < 3

#step 2 = level of significance
alpha = 0.05

#step 3 Test Statistic

SE = sigma/sqrt(n);SE
Zcal = (xbar - mu0)/SE ; Zcal

#step 4 P-value Approach

Pvalue = pnorm(Zcal,lower.tail = TRUE);Pvalue

#concluson : 0.00383 <= 0.05
# we reject

#step 5 Critical value Approach

Zalpha = qnorm(alpha,lower.tail = TRUE);Zalpha
#concluson : -2.66 <= 1.64
# we reject


# using direct from the library

z.test(x=xbar,mu = mu0,stdev = sigma,alternative = "less",n = n ,conf.level = 1-alpha)


#----------------------------------------------------------------------

# Example 

n = 36
xbar = 2.92
sigma = 0.18
alpha = 0.05
mu0 = 3
  
#step1 Hypothesis
  
#H0 : mu >=3
#Ha : mu < 3
  
#step2 level of significance
  
alpha = 0.05

#step3 test statistics

SE = sigma/sqrt(n) ; SE
zcal = (xbar-mu0)/SE ; zcal

#step4 Pvalue Approach

pvalue = pnorm(zcal,lower.tail = TRUE);pvalue

#conclusion : pvalue = 0.003830381 <= alpha = 0.05
#we reject Ho

#step5 Critical value Approach

zvalue = qnorm(alpha,lower.tail = TRUE) ; zvalue
#conclusion : alpha =-2.666667 <= zvalue = -1.644854

#----------------------------------------------------------------------

# 9. Left tail page no : 403

n = 50
xbar = 19.4
sigma = 2
mu0 = 20

# step-1 Hypothesis

# Ho : mu0 >= 20
# Ha : mu0 < 20

#left tail

# step-2 level of significance

alpha = 0.05

# step-3 Test statistics

SE = sigma/sqrt(n) ; SE
Zcal = (xbar-mu0)/SE ; Zcal

# step-4 P-value

Pvalue = pnorm(Zcal,lower.tail = TRUE) ; Pvalue

# conclusion :Pvalue = 0.01694743 <= alpha = 0.05
# we reject Ho 

# step-5 critical value 
Zalpha = qnorm(alpha,lower.tail = TRUE); Zalpha

# conclusion : zcal = -2.12132 <= -zalpha = -1.64
# we reject Ho

#-------------------------------------------------------------------

# 10 Right tail

n = 40
xbar = 26.4
sigma = 6
mu0 = 25

# step1 Hypothesis

# Ho : mu0 <= 25
# Ha : mu0 > 25

# right tail

#step2 level of significance

alpha = 0.01

# step3 test statistics

SE = sigma/sqrt(n) ;SE
zcal = (xbar - mu0)/SE ;zcal

# step4 P value

Pvalue = pnorm(zcal,lower.tail = FALSE) ;Pvalue

# Conclusion : 0.07000825 !<= 0.01
# we failed to reject

# step5 critical value

Zalpha = qnorm(alpha,lower.tail = FALSE) ;Zalpha

# Conclusion : alpha = 1.47573 !>= zalpha = 2.326348
# we failed to reject


#---------------------------------------------------------------

# 11 Two tail

n = 50
xbar =  14.15
sigma = 3
mu0 = 15

#step 1 Hypothesis test

# H0 : μ = 15
# Ha : μ != 15
  
#step 2 Level of significant 
alpha = 0.05

#step 3 test Statistics

SE = sigma/sqrt(n) ; SE
zcal = (xbar-mu0)/SE ; zcal

#step 4 P value

Pvalue = 2*pnorm(zcal,lower.tail = TRUE);Pvalue

# conclusion : zcal = 0.04512695 <= alpha = 0.05
# we reject Ho

#step5 Critical value

Zvalue = qnorm(alpha/2,lower.tail = TRUE);Zvalue

#conclusion :  = -zcal = -2.003469  <= -Zalpha = -1.959964
#we reject Ho


#----------------------------------------------------------------

# 12  Left tail

n = 100
sigma = 12
alpha = 0.01
mu0 = 80

#xbar = 78.5 # a
#xbar = 77 # b
#xbar = 77.5 # c
xbar = 81 # d

#step 1 Hypothesis Test

# H0 : μ >= 80
# Ha : μ < 80

#step2 level of significance
alpha = 0.01

#step3 test Statistics

SE = sigma/sqrt(n) ; SE
zcal = (xbar-mu0)/SE ; zcal

#step3 P value

Pvalue = pnorm(zcal,lower.tail = TRUE) ; Pvalue

#Conclusion : 0.1056498 !< 0.01
# we failed to reject Ho

#Conclusion : 0.006209665 !< 0.01
# we reject Ho

#Conclusion : 0.01861043 !< 0.01
# we failed to reject Ho

#Conclusion : 0.7976716 !< 0.01
# we failed to reject Ho

#step4 Critical Value

Zvalue = qnorm(alpha,lower.tail = TRUE) ; Zvalue

#Conclusion : -1.25 !<= -2.326348
# we failed to reject Ho

#Conclusion : -2.5 <= -2.326348
# we failed to reject Ho

#Conclusion : -2.083333 !<= -2.326348
# we failed to reject Ho

#Conclusion : 0.8333333 !<= -2.326348
# we failed to reject Ho


#------------------------------------------------------------

# Example 15

# Left tail

n = 400
xbar =  910
sigma = 1600
mu0 = 1056

#step 1 Hypothesis test

# H0 : μ >= 1056
# Ha : μ < 1056

#step 2 Level of significant 
alpha = 0.05

#step 3 test Statistics

SE = sigma/sqrt(n) ; SE
zcal = (xbar-mu0)/SE ; zcal

#step 4 P value

Pvalue = pnorm(zcal,lower.tail = TRUE);Pvalue

# conclusion : zcal = 0.03400051 <= alpha = 0.05
# we reject Ho

#step5 Critical value

Zvalue = qnorm(alpha,lower.tail = TRUE);Zvalue

#conclusion :  = -zcal = -1.825  <= -Zalpha = -1.644854
#we reject Ho

#------------------------------------------------------------

# Example 16

#

n = 400
xbar =  910
sigma = 1600
mu0 = 1056

#step 1 Hypothesis test

# H0 : μ >= 1056
# Ha : μ < 1056

#step 2 Level of significant 
alpha = 0.05

#step 3 test Statistics

SE = sigma/sqrt(n) ; SE
zcal = (xbar-mu0)/SE ; zcal

#step 4 P value

Pvalue = pnorm(zcal,lower.tail = TRUE);Pvalue

# conclusion : zcal = 0.03400051 <= alpha = 0.05
# we reject Ho

#step5 Critical value

Zvalue = qnorm(alpha,lower.tail = TRUE);Zvalue

#conclusion :  = -zcal = -1.825  <= -Zalpha = -1.644854
#we reject Ho





#-------------------------------------------------------------------------

#HYPOTHESIS TESTING ABOUT POPULATION MEAN SIGMA UNKNOWN CASE------------

# Lower Tail
# Example (ch 9 ex:25 page no: 409)


n = 36
xbar = 44
s = 5.2
alpha = 0.01
mu0 = 45
df = n-1


#step 1 Hypothesis test

# H0 : μ >= 45
# Ha : μ < 45

#step 2 Level of significant 
alpha = 0.01

#step 3 test Statistics

SE = s/sqrt(n) ; SE
tcal = (xbar-mu0)/SE ; tcal

#step 4 P value

Pvalue = pt(tcal,df,lower.tail = TRUE);Pvalue

# conclusion : Pvalue = 0.1281909 !<= alpha = 0.01
# we failed to reject Ho

#step5 Critical value

talpha = qt(alpha,df,lower.tail = TRUE);talpha

#conclusion :  = -tcal = -1.153846  <= -talpha = -2.437723
#we failed to reject Ho


#--------------------------------------------------------------------

#Lower tail

n = 36
xbar = 43
s = 4.6
alpha = 0.01
mu0 = 45
df = n-1

#step 1 Hypothesis test

# H0 : μ >= 45
# Ha : μ < 45

#step 2 Level of significant 
alpha = 0.01

SE = s/sqrt(n) ; SE
tcal = (xbar-mu0)/SE ; tcal

#step 4 P value

Pvalue = pt(tcal,df,lower.tail = TRUE);Pvalue

# conclusion : Pvalue = 0.006638801 !<= alpha = 0.01
# we reject Ho

#step5 Critical value

talpha = qt(alpha,df,lower.tail = TRUE);talpha

#conclusion :  = -tcal = -2.608696  <= -talpha = -2.437723
#we reject Ho


#---------------------------------------------------------------------

# Uper tail
# Example (ch 9 ex:23 page no: 409)


n = 25
xbar = 14
s = 4.32
alpha = 0.05
mu0 = 12
df = n-1

#step 1 Hypothesis test

# H0 : μ >= 12
# Ha : μ > 12

#step 2 Level of significant 
alpha = 0.05

SE = s/sqrt(n) ; SE
tcal = (xbar-mu0)/SE ; tcal

#step 4 P value

Pvalue = pt(tcal,df,lower.tail = FALSE);Pvalue

# conclusion : Pvalue = 0.0147468 <= alpha = 0.05
# we reject Ho

#step5 Critical value

talpha = qt(alpha,df,lower.tail = FALSE);talpha

#conclusion :  = tcal = 2.314815  >= talpha = -1.710882
#we reject Ho


#---------------------------------------------------------------------

# Two tail
# Example (ch 9 ex:24 page no: 409)

n = 48
xbar = 17
s = 4.5
alpha = 0.05
mu0 = 18
df = n-1

#step 1 Hypothesis test

# H0 : μ >= 18
# Ha : μ > 18

#step 2 Level of significant 
alpha = 0.05

SE = s/sqrt(n) ; SE
tcal = (xbar-mu0)/SE ; tcal

#step 4 P value

Pvalue = 2*pt(tcal,df,lower.tail = TRUE);Pvalue

# conclusion : Pvalue = 0.1303639 !<= alpha = 0.05
# we failed to reject Ho

#step5 Critical value

talpha = qt(alpha/2,df,lower.tail = TRUE);talpha

#conclusion :  = tcal = -1.539601  !<= talpha = -2.011741
#we failed to reject Ho


#---------------------------------------------------------------------

# Example 27

n = 100
xbar = 231
s = 80
mu0 = 238
alpha = 0.05
df = n-1

#step 1 Hypothesis test

# H0 : μ >= 238
# Ha : μ < 238

#step 2 Level of significant 
alpha = 0.05

SE = s/sqrt(n) ; SE
tcal = (xbar-mu0)/SE ; tcal

#step 4 P value

Pvalue = pt(tcal,df,lower.tail = TRUE);Pvalue

# conclusion : Pvalue = 0.1918459 !<= alpha = 0.01
# we reject Ho

#step5 Critical value

talpha = qt(alpha,df,lower.tail = TRUE);talpha

#conclusion :  = -tcal = -0.875  !<= -talpha = -1.660391
#we reject Ho




#----------------------------------------------------------------

# HYPOTHESIS TESTING ABOUT POPULATION PERPOSION 

# Lower tail
# Example 36 (page 414)

n = 300
pbar = 0.68
alpha = 0.05
p0 = 0.75


#step 1 = Hypothesis

#Ho : p0 >= 3
#Ha : p0 < 3

#step 2 = level of significance
alpha = 0.05

#step 3 Test Statistic

SE = sqrt((p0*(1-p0))/n);SE
Zcal = (pbar - p0)/SE ; Zcal

#step 4 P-value Approach

Pvalue = pnorm(Zcal,lower.tail = TRUE);Pvalue

#concluson : 0.00255513 <= 0.05
# we reject

#step 5 Critical value Approach

Zalpha = qnorm(alpha,lower.tail = TRUE);Zalpha
#concluson : -2.8 <= -1.64
# we reject
