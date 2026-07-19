
# Inference about two population mean -------------------------------------
# CH 10
# Interval Estimation of two population mean 
# Sigma Known case

#x1 bar - x2bar + - zalpha /2 
#PE +- ME
#PE + - 
  
  
# Eg 1 
n1 = 36
n2 = 49
x1bar = 40
x2bar = 35
sigma1 = 9
sigma2 = 10
alpha = 0.05

PE = x1bar - x2bar ;PE
Zalpha = qnorm(alpha/2 , lower.tail = F);Zalpha
SE = sqrt((sigma1^2/n1)+(sigma2^2/n2));SE
ME = Zalpha * SE ;ME
Interval = c(PE - ME , PE + ME)
Interval


# EG 1 , ch 10 pg 412
n1 = 50
n2 = 35
x1bar = 13.6
x2bar = 11.6
sigma1 = 2.2
sigma2 = 3.0
# 90 % and 95 % interval
alpha = 0.10

PE = x1bar - x2bar ;PE
Zalpha1 = qnorm(alpha/2 , lower.tail = F);Zalpha1
SE = sqrt((sigma1^2/n1)+(sigma2^2/n2));SE
ME1 = Zalpha1 * SE ;ME1
Interval1 = c(PE - ME1 , PE + ME1)
Interval1

Zalpha2 = qnorm(alpha/2,lower.tail=F);Zalpha2
ME2 = Zalpha2 * SE ; ME2
Interval2 = c(PE - ME2 , PE + ME2)
Interval2


# Hypothesis Testing for two pipulation mean
# Sigma Known case
# Upper Tail
# Eg 2 ch 10 page 413
n1= 40
n2= 50
x1bar=25.2
x2bar= 22.8
sigma1=5.2
sigma2=6.0
alpha=0.05

# Step 1 : Hypothesis Testing
# H0 mu1 - mu2 <= 0
# Ha mu1 - mu2 > 0

# Step 2: Level of significance
alpha = 0.05

# Step 3: test statistic
SE = sqrt((sigma1 ^2 /n1)+ (sigma2^2/n2)) ;SE
Zcal = (x1bar - x2bar) / SE ;Zcal

# Step 4 Pvalue approach
pvalue = pnorm(Zcal,lower.tail = F) ; pvalue
# con:- pvalue 0.02 < alpha = 0.05
#  reject H0

# Step 5 Critical value approach
Zalpha = qnorm(alpha , lower.tail = F);Zalpha
# Zcal = 2.03 >= Zalpha =1.64
# Reject H0

# 

# Lower Tail

