# Goodness of feet --------------------------------------------------------

# chapter 12 pg509 --------------------------------------------------------
#book eg

of=c(48,98,54)#observed frequency
Ep=c(0.30,0.50,0.20) #expected probablity
chisq.test(of,p=Ep)
# p-value=0.025 < alpha=0.05
#conclusion:Reject H0
total=sum(of);total
ef=total*Ep
ef
data=matrix(c(of,ef),nr=3)#nr=number of row
data
chisq.test(data)

# pg512 -------------------------------------------------------------------

#Q1
of=c(60,120,20)
Ep=c(0.40,0.40,0.20) 
chisq.test(of,p=Ep)
# p-value=2.511 < alpha=0.01 if e is there reject
#conclusion: Reject H0
total=sum(of);total
ef=total*Ep
ef


#Q2
of=c(85,95,50,70)
Ep=c(0.25,0.25,0.25,0.25) 
chisq.test(of,p=Ep)
# p-value=0.001 < alpha=0.05
#conclusion:Reject H0
total=sum(of);total
ef=total*Ep
ef



# test of independence ----------------------------------------------------
#pg515
data=matrix(c(20, 40, 20, 30, 30, 10),nrow=2 ,ncol = 3, byrow = T)
data
chisq.test(data)
#conclusion pvalue< alpha 0.04<0.05
#reject H0 



# Annova ------------------------------------------------------------------
#1 Completely randomize design(CRD)/ one way annova
#pg545
#using vector
#n=no of levels( treatment = no of col)
#k =  no of observation per treatment
#method 1 using gl command
c1=c(22,42,44,52,45,37)
c2=c(52,33,8,47,43,32)
c3=c(16,24,19,18,34,39)
v=c(c1,c2,c3)
v
f=c('A','B','C')
k=6
n=3
x=gl(n,k,length=n*k, labels = f)
x
av=aov(v~x)
av
summary(av)
# Conclusion :0.122 < 0.05
# Fail to reject




# method 2 ----------------------------------------------------------------
#using repetition code
c1=c(22,42,44,52,45,37)
c2=c(52,33,8,47,43,32)
c3=c(16,24,19,18,34,39)
v=c(c1,c2,c3)
v
r1=rep('A',times=length(c1))
r2=rep('B',times=length(c2))
r3=rep('C',times=length(c3))
r=c(r1,r2,r3)
r
av1=aov(v~r)
av1
summary(av)
#or
r=rep(c('A','B','C'),times=c(length(c1),length(c2),length(c3)))
av1=aov(v~r)
av1
summary(av)

#using factor
fac=factor(c(c1,c2,c3))
fac
#fac=factor(c('A','B','C')) nai chale
av=aov(v~fac)
av
summary(av)

#csv
v=read.csv(file.choose(), header = T)
v
data=c(data=as.matrix(v)) #column vise
data
data1=c(t(as.matrix(v))) #row wise
data1

#column wise
n=3
k=6
r=gl(n=3,k=6,length= n*k,labels=c("A","B","C"))
r
aov(data~r)

#row wise entry
data1=c(t(as.matrix(v)))
data1
n=3
k=1
r1=gl(n=3,k=1, length = 18, labels = c("A","B","C"))
r1
aov(data1~r1)




# pg 556 ------------------------------------------------------------------

#Q1
#column wise
v=c(162,142,165,145,148,174,142,156,124,142,
    136,152,126,122,138,140,150,128)
k=6
n=3
r=gl(n,k,length=n*k, labels = c("A","B","C"))
r
x=aov(v~r)
av
summary(x)

#row wise
data1=c(162, 142, 126,
        142, 156, 122,
        165, 124, 138,
        145, 142, 140,
        148, 136,150,
        174, 152, 128)
data1
n=3
k=1
r1=gl(n=3,k=1, length = 18, labels = c("A","B","C"))
r1
x1=aov(data1~r1)
x1
summary(x1)



# eg6 ch13 pg557 --------------------------------------------------------------
#using vector and repeat command
v1=c(136,120,113,107,131,114,129,102)
v2=c(107,114,125,104,107,109,97,114,104,89)
v3=c(92,82,85,101,89,117,110,120,98,106)
data=c(v1,v2,v3)
data
r1=rep("A",times=length(v1))
r2=rep("B",times=length(v2))
r3=rep("C",times=length(v3))
r=c(r1,r2,r3)
r
x=aov(data~r)
x
summary(av)


# EG 11 -------------------------------------------------------------------

data1=c(128, 144, 133, 150,
        137, 133, 143, 142,
        135, 142, 137, 135,
        124, 146, 136, 140,
        141, 130, 131, 153)
data1
n=4
k=1
r1=gl(n=3,k=1, length = 20, labels = c("A","B","C"))
r1
x1=aov(data1~r1)
x1
summary(x1)



# Randomize block design(RBD) ---------------------------------------------
#two way annova
#There will be row and column name entry at the same time
#treatment=no of column
#block=no of row
data=read.csv(file.choose(), header = T)
data
v=c(data$A,data$B,data$C)
v
tm=gl(n=3,k=6,length = 18, labels = c("A","B","C"))
tm
block=gl(n=6,k=1,length = 18, labels = c("R1","R2","R3","R4","R5","R6"))
block
modal=aov(v~tm+block)
summary(modal)



# pg 570 q21 --------------------------------------------------------------

A=c(10,12,18,20,8)
B=c(9,6,15,18,7)
C=c(8,5,14,18,8)
v=c(A,B,C)
v
tm=gl(n=3,k=5,length = 15, labels = c("A","B","C"))
tm
block=gl(n=5,k=1,length = 15, labels = c("R1","R2","R3","R4","R5","R6"))
block
modal=aov(v~tm+block)
summary(modal)


# factorial design --------------------------------------------------------
#pg573
#13.10
data=read.csv(file.choose(), header = T)
data
#row-wise entry
data1=c(t(as.matrix(data)))
data1
factorA=c('Three-hour review','One-day program','10-week course')
factorB=c('Business','Engineering','Arts and Sciences')
r=2 #replications
a=length(factorA)
b=length(factorB)
tm1=gl(3,1,length = a*b*r, labels = factorB)
tm1
tm2=gl(3,r*b,length = a*b*r, labels = factorA)
tm2
model=aov(data1~tm1*tm1)
model
x=summary(model)
x
model1=aov(data1~tm2*tm1)
model1
summary(model1)

#column-wise entry
data2=c(as.matrix(data))
data2
factorA=c('Three-hour review','One-day program','10-week course')
factorB=c('Business','Engineering','Arts and Sciences')
r=2 #replications
a=length(factorA)
b=length(factorB)
tm3=gl(3,6,length = a*b*r, labels = factorB)
tm3
tm4=gl(3,2,length = a*b*r, labels = factorA) #d
tm4
model=aov(data1~tm3*tm4)
model

# pg577 -------------------------------------------------------------------
#Q28
data=read.csv(file.choose(), header = T)



# Non paramatric test -----------------------------------------------------
# sign test for sample sample size ---------------------------------------------------------------
#unit4
#A sign test is used to decide whether a bionomial distribution 
#has the equal chance of success and failure
#pg897
n=14 #total sample size
x=2 #no of success
binom.test(x,n)
#for small sample binom.test and for large sample z.test
binom.test(2,14,p=0.5,alternative = "two.sided",conf.level = 0.95)


# pg 893 ------------------------------------------------------------------
#eg 19.1
#hypothesis
#H0: M<=450
#Ha: M>450

weeklysales=c(485,562,415,860,426,474,662,380,515,721)

suc=length(weeklysales[weeklysales<450])
suc

n=length(weeklysales)
n

binom.test(x=suc,n=n)
binom.test(x=n-suc, n=n)


# pg 898 eg 4 -------------------------------------------------------------

netasset=c(6.1,11.7,22.4,9.6,4.9,6.9,15,6.6,14.5,9.6)
suc=length(netasset[netasset<15])
suc

n=length(weeklysales)
n

binom.test(x=suc,n=n)
binom.test(x=n-suc, n=n)



# sign test for large sample ----------------------------------------------
#n>20
n=200
x=72 #success
n=200-25
mu=0.5*n
sd=sqrt(0.25*n)
z.test(x,mu,sd) #method 1

#method2
z=(x-mu)/sd
2*pnorm(z,lower.tail = T)

#With a two-sided test, the p-value 0.111 which is less than 0.05 alpha
# significant level. So, we can reject the Null Hypothesis.

#The study indicates that the candidates are perceived to differ in terms
# of public opinion about their foreign policy

#question1
n=30-3
x=22
mu=0.5*n
sd=sqrt(0.25*n)
z.test(x,mu,sd) #method 1


# Wilcoxon signed - rank test ---------------------------------------------
#when we want to check the significant difference between matched sample
#NOTE :- used for matched sample


# PG900 eg19.6-------------------------------------------------------------------
a=c(10.2,9.6,9.2,10.6,9.9,10.2,10.6,10,11.2,10.7,10.6)
b=c(9.5,9.8,8.8,10.1,10.3,9.3,10.5,10,10.6,10.2,9.8)
diff=c(a-b)
diff
which(a-b==0)
A=a[-c(which(a-b==0))];A
B=b[-c(which(a-b==0))];B
wilcox.test(A,B,paired = T) #error because exact is not written
wilcox.test(A,B,paired = T, exact=F) 


# pg 904 ------------------------------------------------------------------
#eg12
car1=c(20.12,23.56,22.03,19.15,21.23,24.77,16.16,18.55,21.87,24.23,23.21,25.02)
car2=c(18.05,21.77,22.57,17.06,21.22,23.80,17.20,14.98,20.03,21.15,22.78,23.7)
which(car1-car2==0)

#if data does not have difference 0 don't try to remove
wilcox.test(car1,car2, paired = TRUE, exact = FALSE)



# Mann-Whitney-Wilcoxon Test ----------------------------------------------

#to check the significant difference between independent sample

# pg 911 eg19.11 ----------------------------------------------------------
B1=c(1095,955,1200,1195,925,950,805,945,875,1055,1025,975)
B2=c(885,850,915,950,800,750,865,1000,1050,935)
wilcox.test(B1~B2,,exact=F) #pad = F default entry

data=c(B1,B2)
data
v1=rep(1,times=length(B1))
v1
v2=rep(2,times=length(B2))
v2
v=c(v1,v2)
v
wilcox.test(data~v,exact=F)



# pg 915 eg 21 ------------------------------------------------------------
a=c(125,110,65,135,80,150,150,65,80,105,145,60)
b=c(105,75,65,90,70,110,130,95,65,120,75,70)
data=c(a,b)
v1=rep(1,times=length(a))
v1
v2=rep(2,times=length(b))
v2
v=c(v1,v2)
v
wilcox.test(data~v,exact=F)



# Krushal Wallis Test -----------------------------------------------------

#similar to annova more than two column available

#26
a=c(50,62,75,48,65)
b=c(80,95,98,87,90)
c=c(60,45,30,58,57)
data=c(a,b,c)
r=gl(3,5,15,labels = c('A','B','C'))
r
kruskal.test(data~r)

#27
a=c(540,400,490,530,490,610)
b=c(450,540,400,410,480,370,550)
c=c(600,630,580,490,590,620,570)
data=c(a,b,c)
v1=rep(1,times=length(a))
v1
v2=rep(2,times=length(b))
v2
v3=rep(3,times=length(c))
v3
v=c(v1,v2,v3)
v
kruskal.test(data~v)