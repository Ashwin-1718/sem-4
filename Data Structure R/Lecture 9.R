
# Inference about population proportion 2 sample  -------------------------


# interval estimation of p1 - p2
# PG 468 pdf eg 28
n1 = 400
n2= 300
p1bar = 0.48
p2bar = 0.36
alpha= 0.10

# 1) Point estimator PE = p1bar - p2bar
pe = p1bar - p2bar
pe
Zalpha = qnorm(alpha/2 ,lower.tail = FALSE)
Zalpha
a = p(1-p)
b = (1/n1) +(1/n2)
SE = sqrt(a * b)
SE = sqrt((p1bar*(1-p1bar)/n1)+(p2bar*(1-p2bar)/n2))
SE          
ME =Zalpha * SE ; ME
Interval = c(pe - ME , pe + ME) ; Interval



# Hypothesis
# Eg 29 UPPER TAIL Pdf page 468

n1 = 200
n2= 300
p1bar = 0.22
p2bar = 0.16
alpha= 0.05

# 1) Hypothesis :
# H0 :  p1 - p2 <= 0
# Ha : p1 - p2 >0 

# 2)Level of significance 
alpha = 0.05

#  3) Test Statistic
PE= p1bar - p2bar 
SE = sqrt((p1bar*(1-p1bar)/n1)+(p2bar*(1-p2bar)/n2))
Zcal= PE / SE
Zcal

# 4) Pvalue approach
pvalue = pnorm (Zcal ,lower.tail = FALSE)
pvalue
# our p value = 0.04 , alpha = 0.05 
# Hence we reject the null hypothesis

# Critical Value approach 


# Inference about population variance (chi - square distribution) ---------
# page Formula -489

# Pdf page 492
# 2.1)
n =20
s = 5
v = s^2
alpha = 0.10

df = n- 1
d1 = qchisq (alpha /2 ,df , lower.tail =F)
d2 = qchisq (1 - alpha/2 , df  , lower.tail = F)
N = (n-1) * v
Interval = c(N/d1 , N/d2) ; Interval

#2.2)
alpha = 0.05
df = n- 1
d1 = qchisq (alpha /2 ,df , lower.tail =F)
d2 = qchisq (1 - alpha/2 , df  , lower.tail = F)
N = (n-1) * v
Interval = c(N/d1 , N/d2) ; Interval

# 2.3)
s = 5
d1 = qchisq (alpha /2 ,df , lower.tail =F)
d2 = qchisq (1 - alpha/2 , df  , lower.tail = F)
N = (n-1) * s
Interval = c(N/d1 , N/d2) ; Interval
