# Hypothesis testing about population mean sigma Unknown Case -------------

# 1 Hypothesis 
# Ha : 
  
# 2 alpha
# 3 t cal = xbar - mu0 / s/sqrt(n)
# 4 p val <= alpha 
# 5 t cal <= t alpha

# two tail 
# first 3 steps same 
# where alpha is used do alpha/2

# LOWER Tail
# Eg (Ch 9 Eg 25 pdf pg 409)
n=36
xbar = 44
s = 5.2
alpha = 0.01
mu0 = 45
df = n-1
# 1  Hypothesis
# H0 : mu >= 45
# Ha : mu < 45

# 2 Level of Significance
alpha = 0.01

# 3 T calculated
SE = s / sqrt(n) ; SE
tcal = (xbar - mu0)/SE ;tcal

# 4
pvalue = pt(tcal , df , lower.tail = T) # because negative so lower.tail = True
pvalue
# Con :- pvalue = 0.128 !<= 0.01
# OUR PVALUE IS NOT LESS THAN EQUAL TO ALPHA SO WE DONT REJECT THE NULL HYPOTHESIS

# 5 Critical Value approach
talpha = qt(alpha , df, lower.tail = T)
talpha
#Con:- T cal  = -1.15 !<= talpha -2.43
# "DO NOT REJECT H0"


# EG
n = 36
xbar = 43
s = 4.6
alpha = 0.01
mu0=45
df= n-1

# 1  Hypothesis
# H0 : mu >= 45
# Ha : mu < 45

# 2 Level of Significance
alpha = 0.01

# 3 T cal 
SE = s / sqrt(n)
tcal = (xbar - mu0) / SE ;tcal

# 4 
pvalue = pt(tcal , df , lower.tail = T);pvalue
# Con : p





# UPPER TAIL
# Eg (Ch 9 Eg 23 pdf pg 409)
n =25
xbar = 14
s=4.32
alpha = 0.05
mu0 = 12

# 1) Hypothesis
# H0 : mu <= 12
# ha : mu > 12

# 2 Level of Significance
alpha = 0.05

# 3 T calculated
SE = s / sqrt(n) ; SE
tcal = (xbar - mu0)/SE ;tcal

# 4 
pvalue = pt(tcal , df , lower.tail = F);pvalue
# Con pvalue = 0.014 <= alpha = 0.05

# 5 
talpha = qt(alpha , df , lower.tail = F);talpha
# Con:- tcal = 2.31 >= talpha = 2.43 
# Hence we reject H0



# TWO TAIL
# Eg (Ch 9 Eg 23 pdf pg 409)
n = 48
xbar = 17
s = 4.5
alpha = 0.05
mu0 = 18

# 1 

# 2 Level of Significance
alpha = 0.05

# 3 T calculated
SE = s / sqrt(n) ; SE
tcal = (xbar - mu0)/SE ;tcal

# 4 
pvalue = pt(tcal , df , lower.tail = T)
pvalue
# Con :- pvalue = 0.07 !<= alpha /2 = 0.025
# So we fail to reject
# 5
talpha = qt(alpha/2 , df,lower.tail =  T);talpha
# COn 


# EG (27 , 
# keyword = below so lower tail eg
n = 100 
xbar = 231
s= 80
alpha = 0.05
mu0 = 238

# 1  Hypothesis
# H0 : mu >= 238
# Ha : mu < 45

# 2 Level of Significance
alpha = 0.05

# 3 T calculated
SE = s / sqrt(n) ; SE
tcal = (xbar - mu0)/SE ;tcal

# 4
pvalue = pt(tcal , df , lower.tail = T) # because negative so lower.tail = True
pvalue
# Con :- pvalue = 0.19 !<= 0.05
# OUR PVALUE IS NOT LESS THAN EQUAL TO ALPHA SO WE DONT REJECT THE NULL HYPOTHESIS

# 5 Critical Value approach
talpha = qt(alpha , df, lower.tail = T)
talpha
#Con:- T cal  = -1.68 !<= talpha -2.43
# "DO NOT REJECT H0"

# Eg 28 Differ (two tail)



# Hypothesis testing about proportion -------------------------------------

# LOWER TAIL 
# eg 36 book pg 379
n = 300
pbar = 0.68
alpha = 0.05
p0 = 0.75

# 1  Hypothesis
# H0 : pbar >= 0.75
# Ha : pbar < 0.75

# 2 Level of Significance
alpha = 0.05

# 3 Test Statistic (p cal) 
SE = sqrt((p0*(1-p0))/n);SE
pcal = (pbar - p0)/SE;pcal

# 4 
pvalue = pnorm(pcal , lower.tail = T);pvalue
# Con pvalue =  <= alpha = 
# So we reject H0

# 5 
zalpha = qnorm(alpha,lower.tail = F);zalpha
# Con:- pcal = -2.8 <= zalpha = -1.64 
# Hence we reject H0
