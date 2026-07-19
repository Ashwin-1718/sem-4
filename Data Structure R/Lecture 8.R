# INTERVAL eStimation sigma unknown case for 2 sample  ---------------------------------

# Example 
x1 = c(300,280,344,385,372,360,288,321,376,290,301,283) ;x1
x2 = c(274,220,308,336,198,300,315,258,318,310,332,263) ;x2
n1 = length(x1)
n2 = length(x2)
x1bar = mean(x1)
x2bar = mean(x2)
v1 = var(x1)
v2 = var(x2)
df = n1 + n2 -2
alpha = 0.05

PE = x1bar - x2bar;PE # Point estimator
SE = sqrt((v1/n1) + (v2/n2)) ; SE # Standard Error
talpha = qt(alpha/2,df,lower.tail = F);talpha 
ME = talpha * SE ;ME # margin of error
Interval = c(PE -ME , PE + ME); Interval



# CH 10 Eg 9
n1 = 20
n2 = 30 
x1bar = 22.5
x2bar = 20.1
s1 = 2.5
v1 = s1^2
s2 = 4.8
v2 = s2^2
# 95% and 90% confidence interval 
# 1 )
alpha = 0.05
PE = x1bar - x2bar;PE # Point estimator
SE = sqrt((v1/n1) + (v2/n2)) ; SE # Standard Error
talpha = qt(alpha/2,df,lower.tail = F);talpha 
ME = talpha * SE ;ME # margin of error
Interval = c(PE -ME , PE + ME); Interval

#2)
alpha = 0.10
PE = x1bar - x2bar;PE # Point estimator
SE = sqrt((v1/n1) + (v2/n2)) ; SE # Standard Error
talpha = qt(alpha/2,df,lower.tail = F);talpha 
ME = talpha * SE ;ME # margin of error
Interval = c(PE -ME , PE + ME); Interval


# Hypothesis testing sigma unknown case 2 sample --------------------------

# example lower tail
# CH 10 EG 14
n1 = 40
n2 = 50
s1 = 6000
v1 = s1^2
s2 = 7000 
v2 = s2^2
x1bar = 56100
x2bar = 59400
alpha = 0.05
df = n1 + n2 - 2

# 1) Hypothesis :
# H0 : mu1 - mu2 >= 0 
# ha :mu1 - mu2 < 0 

#2) level of significance 
alpha = 0.05

# 3) Test statistic 
SE = sqrt ((s1^2/n1)+s2^2/n2) ; SE
PE = x1bar - x2bar ; PE
tcal = PE / SE ;tcal

# 4) Pvalue approach 
pvalue = pt(tcal , df , lower.tail = T) ;pvalue 
# Con:- pvalue = 0.009 < alpha = 0.05 
# REJECT H0

# 5) Critical value
talpha = qt(alpha , df, lower.tail = T)
talpha
# Con :- tcal = -2.40 < talpha = -1.66 
# REJECT H0


# example upper tail

n1 = 12
n2 = 12
s1 = 40
v1 = s1^2
s2 = 44
v2 = s2^2
x1bar = 325
x2bar = 286
alpha = 0.05
df = n1 + n2 - 2

# 1) Hypothesis :
# H0 : mu1 - mu2 <= 0 
# ha :mu1 - mu2 > 0 

#2) level of significance 
alpha = 0.05

# 3) Test statistic 
SE = sqrt ((s1^2/n1)+s2^2/n2) ; SE
PE = x1bar - x2bar ; PE
tcal = PE / SE ;tcal

# 4) Pvalue approach 
pvalue = pt(tcal , df , lower.tail = F) ;pvalue 
# Con:- pvalue = 0.016 <= alpha = 0.05 
# REJECT H0

# 5) Critical value
talpha = qt(alpha , df, lower.tail = F)
talpha
# Con :- tcal = 2.27 >  talpha = - 1.71
# REJECT H0



# example two tail
# ch 10 Eg 10
n1 = 35
n2 = 40
s1 = 5.2
v1 = s1^2
s2 = 8.5
v2 = s2^2
x1bar = 13.6
x2bar = 10.1
alpha = 0.05
df = n1 + n2 - 2

# 1) Hypothesis :
# H0 : mu1 - mu2 = 0 
# Ha :mu1 - mu2 != 0 

#2) level of significance 
alpha = 0.05

# 3) Test statistic 
SE = sqrt ((s1^2/n1)+s2^2/n2) ; SE
PE = x1bar - x2bar ; PE
tcal = PE / SE ;tcal
# tcal is positive so lower.tail = False if negative lower.tail = True
# 4) Pvalue approach 
pvalue = pt(tcal , df , lower.tail = F) ;pvalue 
# Con:- pvalue = 0.016 <= alpha/2 = 0.025 
# REJECT H0

# 5) Critical value
talpha = qt(alpha/2 , df, lower.tail = F)
talpha
# Con :- tcal = 2.17 >=  talpha = 1.99
# REJECT H0

