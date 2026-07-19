# Inference about population variance CHI square distribution

#Example 2

n = 20
s = 5
v = s^2
df = n-1
alpha = 0.10 # 90%

d1 = qchisq(alpha/2,df,lower.tail = F)
d2 = qchisq(1-alpha/2,df,lower.tail = F)

N = (n-1)*v

CI = c(N/d1,N/d2);CI

alpha = 0.05 # 90%

d1 = qchisq(alpha/2,df,lower.tail = F)
d2 = qchisq(1-alpha/2,df,lower.tail = F)

N = (n-1)*v

CI = c(N/d1,N/d2);CI

alpha = 0.10 # 90%

d1 = qchisq(alpha/2,df,lower.tail = F)
d2 = qchisq(1-alpha/2,df,lower.tail = F)

N = (n-1)*s

CI = c(N/d1,N/d2);CI


# Hypothesis testing about population Variance

#Upper Tail EX = 3 page = 492

n = 16 
alpha = 0.05 
s = 9.5
v = 9.5 ^ 2 ; v
v0 = 50
df =n-1

#step 1 Hypothisis

# H0 : sigma0 <= 50
# Ha : sigma0 > 50

# step 2 Level of significance
alpha = 0.05

# step 3 test stestics

chical = ((n-1)*v)/v0;chical


# step 4 pvalue Approach

pvalue = pchisq(chical,df,lower.tail = F);pvalue

# conclusion : pvalue = 0.02813119 <= alpha = 0.05
# we reject H0

#step 5 critical value Approach

chialpha = qchisq(alpha,df,lower.tail = F);chialpha
# conclusion : chical = 27.075 >= chialpha = 24.99579
# we reject H0

#--------------------------------------------------------------------
# Ex = 9 Upper tail

n = 30
df = n-1
v = 0.0005
v0 = 0.0004
alpha = 0.05

#step 1 Hypothisis

# H0 : sigma0 <= 0.0004
# Ha : sigma0 > 0.0004

# step 2 Level of significance
alpha = 0.05

# step 3 test stestics

chical = ((n-1)*v)/v0;chical


# step 4 pvalue Approach

pvalue = pchisq(chical,df,lower.tail = F);pvalue

# conclusion : pvalue = 0.1663945 <= alpha = 0.05
# we failed to reject H0

#step 5 critical value Approach

chialpha = qchisq(alpha,df,lower.tail = F);chialpha
# conclusion : chical = 36.25 >= chialpha = 42.55697
# we failed to reject H0

#-----------------------------------------------------------------------

# EX = 11 Two tail

data = c(3.98,3.78,2.95,3.55,3.44,3.08,4.51,1.32,3.53,3.90,2.48,3.76)
n = length(data);n
df = n-1
v = var(data);v
v0 = 0.70
alpha = 0.05

#step 1 Hypothisis

# H0 : sigma0 = 0.70
# Ha : sigma0 != 0.70

# step 2 Level of significance
alpha = 0.05

# step 3 test stestics

chical = ((n-1)*v)/v0;chical


# step 4 pvalue Approach

pvalue = 2*pchisq(chical,df,lower.tail = F);pvalue

# conclusion : pvalue = 0.913365 <= alpha = 0.05
# we reject H0

#step 5 critical value Approach

chialpha = qchisq(alpha/2,df,lower.tail = F);chialpha
# conclusion : chical = 10.84095 >= chialpha = 21.92005
# we reject H0

#if in two tail ex if we are continue witrh alpha/2 lowertail.tail = F
#1-alpha alpha = T

#----------------------------------------------------------

# Inference about two population varience (F test)
# important higher variance v1 and lower varience v2

# Two tail

n1 = 26
n2 = 16
v1 = 48
v2 = 40
alpha = 0.10


#step 1 Hypothisis

# H0 : sigma = 0.70
# Ha : sigma0 != 0.70

# step 2 Level of significance
alpha = 0.05

# step 3 test stestics

fcal = (v1/v2);fcal


# step 4 pvalue Approach

pvalue = pf(fcal,n1-1,n2-1,lower.tail = F);pvalue

# conclusion : pvalue = 0.913365 <= alpha = 0.025
# we reject H0

#step 5 critical value Approach

falpha = qf(alpha/2,df = n1-1, df2 = n2-1,lower.tail = F);falpha
# conclusion : fcal = 10.84095 >= chialpha = 21.92005
# we reject H0




#------------------------------------------------------------------
#ch 12 goodness of fit

#exa

ov = c(48,98,54);ov
ep = c(0.30,0.50,0.20);ep
chisq.test(ov,p=ep) 

#------------------------------------------------------------------------

#Example 1

ov = c(60,120,20)
ep = c(0.40,0.40,0.20)
chisq.test(ov,p=ep)
# conclusion : pvalue < 0.01 < = alpha = 0.01
# we reject h0

#-----------------------------------------------------------------------

#Example 3

ov = c(95,70,89,46)
ep = c(0.29,0.28,0.25,0.18)
chisq.test(ov,p=ep)


#-----------------------------------------------------------------------

#page = 515

data = matrix(c(20,30,40,30,20,10),nrow = 2,ncol = 3,byrow = FALSE);data
chisq.test(data)
#conclusion : pvalue = 0.04683 <= alpha = 0.05
# we reject ho

#---------------------------------------------------------------------

#Example 9 

data = matrix(c(20,30,44,26,50,30), nrow = 2, ncol = 3 , byrow = FALSE);data
chisq.test(data)
#conclusion : pvalue = 0.01962 <= alpha = 0.05
# we reject ho

#---------------------------------------------------------------------

#Example 10

data = matrix(c(20,30,10,30,60,15,20,25,30), nrow = 3, ncol = 3 , byrow = FALSE);data
chisq.test(data)
#conclusion : pvalue = 0.0005554 <= alpha = 0.05
# we reject ho


#---------------------------------------------------------------------


# Analysis of variance ANNOVA  -----------------------------------------------------------------

# ch-13
# their aer mainly two type
# One-way annova = Complitlly randomize design(CRD)
# Two-way annova = randomize block design(RBD)

# NO 1 : CRD Method--------------------------------------------------

#no of column = treatment = no levels = fectors 
# n = no of observation in 1 column
#CRD = gives only column name

# Method 1

# using 'vector' & 'gl' command
# column wise entry

v1 = c(58,64,55,66,67)
v2 = c(58,69,71,64,68)
v3 = c(48,57,59,47,49)

data = c(v1,v2,v3);data
r = gl(3,5,length = 15,labels = c('A','B','C'));r
model = aov(data~r);model
x = summary(model);x

#--------------------------------------------------------------------

#Row-wise entry

data1 = c(58,58,48,64,69,57,55,71,59,66,64,47,67,68,49)
r1 = gl(3,1,length = 15,labels = c('A','B','C'));r1
model = aov(data~r);model
x = summary(model);x

#-----------------------------------------------------------------

# gl command only use when data is square or rectangular form (which means length of the each treatment should be same)
# using 'vector' & 'rep' command
# rep command is used when length of the each treatment is an unequal

# EX 6 page no : 557 ------------------------------------------------------

v1 = c(136,120,113,107,131,114,129,102)
v2 = c(107,114,125,104,107,109,97,114,104,89)
v3 = c(92,82,85,101,89,117,110,120,98,106)

data = c(v1,v2,v3);data
r1= rep('A',times=length(v1))
r2= rep('B',times=10)
r3= rep('C',times=length(v3))

r = c(r1,r2,r3);r
model = aov(data~r);model
x = summary(model);x


#-----------------------------------------------------------------

#using csv file
# pg 545

x = read.csv(file.choose(),header = T);
x

#column wise entry
data = c(as.matrix(x));data

r = gl(3,5,15,labels=c('A','B','C'));r #column

model = aov(data~r);model
x = summary(model);x

#row wise entry
data1 = c(t(as.matrix(x)));data1
r1 = gl(3,1,15,labels=c('A','B','C'));r1 #row

model = aov(data1~r1);model
x = summary(model);x

#-----------------------------------------------------------------------

#Example 1 pg no 556

v1 = c(162,142, 165,145,148,174)
v2 = c(142,156,124,142,136,152)
v3 = c(126,122,138,140,150,128)

data = c(v1,v2,v3);data
r = gl(3,6,length = 18,labels = c('A','B','C'));r
model = aov(data~r);model
x = summary(model);x


#-------------------------------------------------------------------------

# Example 11
# baki


# Randomize Block Design --------------------------------------------------

x = read.csv(file.choose(),header = T);
x

#column wise entry
data = c(as.matrix(x));data

tm = gl(3,6,18,labels=c('A','B','C'));tm #column
block = gl(6,1,18,labels = c('r1','r2','r3','r4','r5','r6'));block

model = aov(data~tm+block);model
x = summary(model);x

#------------------------------------------------------------------

#Example 26 page 571

x = read.csv(file.choose(),header = T);
x

#column wise entry
data = c(as.matrix(x));data

tm = gl(3,6,18,labels=c('A','B','C'));tm #column
block = gl(6,1,18,labels = c('1','2','3','4','5','6'));block

model = aov(data~tm+block;model
x = summary(model);x
