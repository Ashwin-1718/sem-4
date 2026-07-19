#inforance about Two population mean---------------------------------

#interval estimation of two population mean (sigma known case)-------

#Example 1

n1 = 36
n2 = 49
x1bar = 40
x2bar = 35
sigma1 = 9 
sigma2 = 10
alpha = 0.05

PE  = x1bar - x2bar ; PE
Zalpha = qnorm(alpha/2,lower.tail = FALSE);Zalpha
SE = sqrt((sigma1^2/n1)+(sigma2^2/n2));SE
ME = Zalpha * SE ; ME
Interval = c(PE-ME,PE + ME) ; Interval

#----------------------------------------------------------------------

# Ex 1 page 412 ch 10

n1 = 50
n2 = 35
x1bar = 13.6
x2bar = 11.6
sigma1 = 2.2
sigma2 = 3.0

# 90% interval
alpha = 0.10

PE  = x1bar - x2bar ; PE
Zalpha = qnorm(alpha/2,lower.tail = FALSE);Zalpha
SE = sqrt((sigma1^2/n1)+(sigma2^2/n2));SE
ME = Zalpha * SE ; ME
Interval = c(PE-ME,PE + ME) ; Interval

# 95% interval
alpha = 0.05

PE  = x1bar - x2bar ; PE
Zalpha = qnorm(alpha/2,lower.tail = FALSE);Zalpha
SE = sqrt((sigma1^2/n1)+(sigma2^2/n2));SE
ME = Zalpha * SE ; ME
Interval = c(PE-ME,PE + ME) ; Interval



#----------------------------------------------------------------------
#Hypothesis testing for two population mean for sigma knwon case

#Upertail
# Example 2 ch 10 page= 413


n1 = 40
n2 = 50
x1bar = 25.2 
x2bar = 22.8
sigma1 = 5.2
sigma2 = 6.0
alpha = 0.05

#step 1 Hypothesis test

# Ho = mu1-mu2 <= 0
# Ha = mu1-mu2 > 0

#step 2 Level of significance
alpha = 0.05

#step 3 Test statistics
SE = sqrt((sigma1^2/n1)+(sigma2^2/n2));SE
zcal = (x1bar-x2bar)/SE ; zcal

#step 4 pvalue Approach
pvalue = pnorm(zcal,lower.tail = FALSE);pvalue

# conclusion : Pvalue = 0.0211136 <= alpha = 0.05
# we reject Ho
  
#step 5 critical value Approach
zalpha = qnorm(alpha,lower.tail = FALSE);zalpha

# conclusion : zcal = 2.031274 >= zalpha = 1.644854
# we reject Ho


#------------------------------------------------------------------

#interval estimation sigma unknown case-------------------------------

x1 = c(300,280,344,385,372,360,288,321,376,290,301,283) ;x1
x2 = c(274,220,308,336,198,300,315,258,318,310,332,263) ;x2

n1 = length(x1)
n2 = length(x2)
x1bar=mean(x1)
x2bar=mean(x2)
v1=var(x1)
v2=var(x2)
df = n1+n2-2;df
alpha = 0.05

PE = x1bar-x2bar

SE = sqrt((v1/n1)+(v2/n2));SE
talpha = qt(alpha/2,df,lower.tail = F);talpha

ME = talpha * SE ; ME
CI = c(PE-ME,PE+ME);CI
  
  
  
#---------------

#example = 9 ch 10

n1 = 90
n2 = 30
x1bar = 22.5
x2bar = 20.1
s1 = 2.5
s2 = 4.8
v1 = s1^2
v2 = s2^2
# 95% and 90% confidence interval

SE = sqrt((v1/n1)+(v2/n2));SE
talpha = qt(alpha/2,df,lower.tail = F) 





  
# Hypothesis testing sigma unknown case Two sample----------------------

#example Lowertail--------------------------------------------------

n1 = 40
n2 = 50
s1 = 6000
s2 = 7000
x1bar = 56100
x2bar = 59400
alpha = 0.05
df = n1+n2-2

# Step 1 Hypothesis
#H0 : mu1 - mu2 >= 0 
#Ha : mu1 - mu2 < 0 

#step 2 level of significance
alpha = 0.05

# step 3 test statistics

PE = x1bar - x2bar ;PE
SE = sqrt((s1^2/n1)+(s2^2/n2));SE
tcal = PE/SE ; tcal

#step 4 p value Approach

pvalue = pt(tcal,df,lower.tail = T);pvalue
#conclusion : pvalue = 0.009092354 <= alpha = 0.05
# we reject H0

# step 5 critical value Approach

talpha = qt(alpha,df,lower.tail = T);talpha
#conclusion : tcal = -2.406772 <= talpha = -1.662354
# we reject H0

#-----------------------------------------------------------------------

#Upertail--------------------------------------------------

n1 = 12
n2 = 12
s1 = 40
s2 = 44
x1bar = 325
x2bar = 286
alpha = 0.05
df = n1+n2-2

# Step 1 Hypothesis
#H0 : mu1 - mu2 <= 0 
#Ha : mu1 - mu2 > 0 

#step 2 level of significance
alpha = 0.05

# step 3 test statistics

PE = x1bar - x2bar ;PE
SE = sqrt((s1^2/n1)+(s2^2/n2));SE
tcal = PE/SE ; tcal

#step 4 p value Approach

pvalue = pt(tcal,df,lower.tail = F);pvalue
#conclusion : pvalue = 0.01660831 <= alpha = 0.05
# we reject H0

# step 5 critical value Approach

talpha = qt(alpha,df,lower.tail = F);talpha
#conclusion : tcal = 2.271952 >= talpha = 1.717144
# we reject H0

# Twotail--------------------------------------------------
# Example 10

n1 = 35
n2 = 40
s1 = 5.2
s2 = 8.5
x1bar = 13.6
x2bar = 10.1
alpha = 0.05
df = n1+n2-2

# Step 1 Hypothesis
#H0 : mu1 - mu2 = 0 
#Ha : mu1 - mu2 != 0 

#step 2 level of significance
alpha = 0.05

# step 3 test statistics

PE = x1bar - x2bar ;PE
SE = sqrt((s1^2/n1)+(s2^2/n2));SE
tcal = PE/SE ; tcal

#step 4 p value Approach

pvalue =2*pt(tcal,df,lower.tail = F);pvalue
#conclusion : pvalue = 0.03252088 <= alpha = 0.05
# we reject H0

# step 5 critical value Approach

talpha = qt(alpha/2,df,lower.tail = F);talpha
#conclusion : tcal = 2.179503 >= talpha = 1.992997
# we reject H0


# Inferance about population propersion two sample --------------------
# Interval estimation of p1 - p2 --------------------------------------



