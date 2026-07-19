# Q1 ----------------------------------------------------------------------

n = 50
sigma = 600
alpha = 0.05

Zalpha = qnorm(alpha/2, lower.tail = FALSE) ; Zalpha
SE = sigma / sqrt(n) ; SE
ME = Zalpha * SE ; ME 

ME_req = 150
n_req = ( (Zalpha * sigma) / ME_req )^2 ; n_req
ceiling(n_req)   


# Q2 ----------------------------------------------------------------------

n1 = 80
n2 = 70
x1bar = 104
x2bar = 106
sg1 = 8.4
sg2 = 7.6
alpha = 0.05

PE = x1bar - x2bar ; PE
SE = sqrt((sg1^2 / n1) + (sg2^2 / n2)) ; SE
zcal = PE / SE ; zcal 

pvalue = 2 * pnorm(abs(zcal), lower.tail = FALSE) ; pvalue

zalpha = qnorm(alpha/2, lower.tail = FALSE) ; zalpha


# Q3 ----------------------------------------------------------------------

data = matrix(c(29,22,95,121,518,135),nrow = 3, ncol = 2, byrow = TRUE)
data

chisq.test(data)


# Q4 ---------------------------------------------------------------------

Easetowne = c(99, 64, 101, 85, 79, 88, 97, 95, 90, 100)
Craborchard = c(83, 102, 125, 61, 91, 96, 94, 89, 93, 75)
Fairforest = c(89, 98, 56, 105, 87, 90, 87, 101, 76, 89)

data = c(Easetowne, Craborchard, Fairforest)
group = gl(3, 10, 30, labels = c("Easetowne","Craborchard","Fairforest"))
group

kruskal.test(data ~ group)

