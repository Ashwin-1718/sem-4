
# Ch 12:- Goodness of feet ------------------------------------------------

# Example 
ov=c(48,98,54) ;ov
ep = c(0.30,0.50,0.20);ep
pvalue =chisq.test(ov,p=ep)
pvalue

# Pdf page 512 eg1
ov = c(60,120,20);ov
ep=c(0.40,0.40,0.20);ep
pvalue = chisq.test(ov,p=ep);pvalue
# e -08 means less than 0.000....
# Con- pvalue =2.511e-08 < alpha =0.01
# SO we reject H0

# Pdf page 512 eg 3 
ov=c(95,70,89,46);ov
ep = c(.29,.28,.25,.18);ep
pvalue = chisq.test(ov , p=ep) ;pvalue

# Con- pvalue = 0.07624 !< alpha =0.05 
# So we fail to reject Ho


# Test of Independence ----------------------------------------------------
# Ch 12
# pdf Page 515 Eg table
## REMEMVER TO CONVERT INTO MATRIX
data = matrix(c(20,40,20,30,30,10),nrow=2,ncol=3,byrow=TRUE)
data
chisq.test(data)
# Con- pvalue = 0.04 < alpha 0.05 # Reject

# Pdf page 519 Eg 9
data = matrix(c(20,44,50,30,26,30),nrow=2,ncol=3,byrow=TRUE)
data
chisq.test(data)
# Con- pvalue = 0.019 < alpha 0.05 # Reject

# Pdf page 519 Eg 10
data = matrix(c(20,30,20,30,60,25,10,15,30),3,3,TRUE)
data
chisq.test(data)
# Con- pvalue = 0.00054 < alpha 0.05 # Reject

# Analysis of Variance  ANOVA ---------------------------------------------

# Ch 13
# There are mainly 2 type
# One-way ANOVA = completely randomized design (CRD)
# Two-way ANOVA = randomized block design (RBD)
# aov (analysis of variance)

#1) CRD  METHOD
# k= No of coloumns = treatment = no of levels = factors 
# n = No of observation in one column 
# CRD = gives only column names 

# METHOD 1:-
# using 'vector' and 'gl' command

#1) coloumnwise entry

v1=c(58,64,55,66,67) ;v1
v2=c(58,69,71,64,68) ;v2
v3=c(48,57,59,47,49) ;v3

data = c(v1,v2,v3);data
r = gl(3,5,15,labels=c('A','B','C')) ;r# n= no of levels ,  k =  , length 
Model = aov(data ~ r); Model
# For annova Table
x= summary(Model) ;x

#2) rowwise entry

data1 = c(58,58,48,64,69,57,55,71,59,66,64,47,67,68,49);data1
r1 = gl(3,1,15,labels=c('A','B','C')) ; r1
Model1 = aov(data1~r1);Model1
x= summary(Model1) ; x

# Method 1
# Using Vector and 'rep' command

# gl Command Only used when data in square or rectangular form
#(Means - length of each treatment should be same )
# rep command used when length in each treatment is unequal

# Example 6 Pg 557
# length diff so use rep
# Coloumnwise
v1 =c(136,120,113,107,131,114,129,102)
v2 =c(107,114,125,104,107,109,97,114,104,89)
v3 =c(92,82,85,101,89,117,110,120,98,106)
data = c(v1,v2,v3)
data
r1 = rep('A',times=length(v1))
r2 = rep('B',times=length(v2))
r3 = rep('C',times=length(v3))

r = c(r1,r2,r3);r
model =aov(data ~ r)
model
x= summary (model) ; x


# Using CSV file ----------------------------------------------------------


