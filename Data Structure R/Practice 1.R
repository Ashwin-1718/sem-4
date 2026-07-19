# Chi-Square Goodness of Fit Test

of = c(40, 120, 40)         # observed frequency
Ep = c(0.25, 0.50, 0.25)    # expected probability

chisq.test(of, p = Ep)
# Check p-value with alpha = 0.05

total = sum(of); total
ef = total * Ep; ef         # expected frequency


of = c(18,16,20,22,24,20)         # observed frequency
Ep = c(1/6,1/6,1/6,1/6,1/6,1/6)   # expected probability for fair die

chisq.test(of, p = Ep)

total = sum(of); total
ef = total * Ep; ef               # expected frequencies



# Test of Independence ----------------------------------------------------

# Chi-Square Test of Independence

data = matrix(c(12, 8,
                10, 20),
              nrow = 2, ncol = 2, byrow = TRUE)
data

chisq.test(data)

# H0: Smoking habit and exercise level are independent
# Ha: They are not independent

# Decision: if p-value < 0.05 → Reject H0 (dependent)
#           if p-value > 0.05 → Fail to reject H0 (independent)


# -------------------------------------------------------------------------


data = matrix(c(55, 45,
                65, 35),
              nrow = 2, ncol = 2, byrow = TRUE)
data

chisq.test(data)

# H0: Meal preference and gender are independent
# Ha: They are not independent
# If p-value < 0.05 → Reject H0


# One Way Anova -----------------------------------------------------------

# One-way ANOVA (CRD)

c1 = c(22,42,44,52,45,37)
c2 = c(52,33,8,47,43,32)
c3 = c(16,24,19,18,34,39)

v = c(c1, c2, c3)   # all observations
k = 6               # observations per treatment
n = 3               # number of treatments

r = gl(n, k, length = n*k, labels = c("A","B","C"))
r

av = aov(v ~ r)
summary(av)

# If p-value < 0.05 → Reject H0 (means are significantly different)

# -------------------------------------------------------------------------

A = c(12,15,14,16,13,11)
B = c(10,11,13,12,14,15)
C = c(14,17,16,15,18,17)

v = c(A, B, C)
k = 6
n = 3

group = gl(n, k, length = n*k, labels = c("A","B","C"))
group

model = aov(v ~ group)
summary(model)

# If p-value < 0.05 → Reject H0
#0.00693 < 0.05


# RBD – Randomized Block Design (Two-Way ANOVA) ---------------------------
# RBD – Two-way ANOVA

A = c(20,21,19,22)
B = c(18,19,17,20)
C = c(22,23,20,24)

v = c(A,B,C)
v

tm = gl(n=3, k=4, length=12, labels=c("A","B","C"))
tm

block = gl(n=4, k=1, length=12, labels=c("F1","F2","F3","F4"))
block

model = aov(v ~ tm + block)
summary(model)


# -------------------------------------------------------------------------

# Factorial Design – Two-way ANOVA with interaction

# Data row-wise: A1B1, A1B2, A2B1, A2B2, A3B1, A3B2 (each with 2 reps)

data1 = c(
  70,75,   # A1 B1
  72,78,   # A1 B2
  65,68,   # A2 B1
  70,73,   # A2 B2
  80,82,   # A3 B1
  78,81    # A3 B2
)
data1

factorA = c('A1','A2','A3')
factorB = c('B1','B2')

r = 2
a = length(factorA)
b = length(factorB)

tmA = gl(a, r*b, length = a*b*r, labels = factorA)  # Method
tmB = gl(b, r,   length = a*b*r, labels = factorB)  # Stream

tmA
tmB

model = aov(data1 ~ tmA * tmB)
summary(model)


# Unit 4 ------------------------------------------------------------------
# H0: Median <= 450
# Ha: Median > 450   (or you can also test median = 450 using two-sided)

weeklysales = c(485,562,415,860,426,474,662,380,515,721)

suc = length(weeklysales[weeklysales > 450]); suc  # count > 450 (or <, depends on Ha)
n = length(weeklysales); n

# Two-sided sign test:
binom.test(x = suc, n = n, p = 0.5, alternative = "two.sided")



# Wilcoxon Signed-Rank Test -----------------------------------------------
old = c(10.2, 9.6, 9.2, 10.6, 9.9, 10.2, 10.6, 10, 11.2, 10.7, 10.6)
new = c(9.5, 9.8, 8.8, 10.1,10.3, 9.3, 10.5, 10, 10.6, 10.2, 9.8)

wilcox.test(old, new, paired = TRUE, exact = FALSE)



# Mann–Whitney ------------------------------------------------------------

B1 = c(1095,955,1200,1195,925,950,805,945,875,1055,1025,975)
B2 = c(885,850,915,950,800,750,865,1000,1050,935)

data = c(B1, B2)
v1 = rep(1, times = length(B1))
v2 = rep(2, times = length(B2))
v = c(v1, v2)

wilcox.test(data ~ v, exact = FALSE)


# Kruskal–Wallis Test -----------------------------------------------------

A = c(50,62,75,48,65)
B = c(80,95,98,87,90)
C = c(60,45,30,58,57)

data = c(A,B,C)
r = gl(3, 5, 15, labels = c('A','B','C'))
r

kruskal.test(data ~ r)


# -------------------------------------------------------------------------

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
