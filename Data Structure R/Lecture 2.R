# Examples pdf page 350 ----------------------------------------------------
#2)A simple random sample of 50 items from a population with σ 6 resulted in a sample
#mean of 32.
#a.
#Provide a 90% confidence interval for the population mean.
#b. Provide a 95% confidence interval for the population mean.
#c.Provide a 99% confidence interval for the population mean.

n = 50
sigma = 6
xbar = 32
alpha = 0.10 #10% to 0.10 

#1
SE = sigma / sqrt(n);SE
#for the value of z alpha
Zalpha = qnorm(alpha/2 ,lower.tail=F )
ME = Zalpha * SE;ME
CI_1 = c(xbar - ME, xbar + ME );CI_1

#2
alpha = 0.05
SE = sigma / sqrt(n);SE
Zalpha = qnorm(alpha/2,lower.tail = F)
ME = Zalpha * SE ;ME
CI_2 = c(xbar - ME , xbar + ME)
CI_2

#3
alpha = 0.01
SE = sigma / sqrt(n);SE
Zalpha = qnorm(alpha/2,lower.tail = F);Zalpha
ME = Zalpha * SE;ME
CI_3 = c(xbar - ME ,xbar + ME)
CI_3

cat("90% CI:", CI_1, "\n")
cat("95% CI:", CI_2, "\n")
cat("99% CI:", CI_3, "\n")



#3)A simple random sample of 60 items resulted in a sample mean of 80. The population
#standard deviation is σ 15.
#a.Compute the 95% confidence interval for the population mean.
#b. Assume that the same sample mean was obtained from a sample of 120 items. Provide
#a 95% confidence interval for the population mean.
#c.What is the effect of a larger sample size on the interval estimate?


n=60
xbar = 80
sigma = 15
#1)
alpha = 0.05
SE = sigma / sqrt(n); SE
Zalpha = qnorm(alpha/2, lower.tail = F)
Zalpha
ME = Zalpha * SE ; ME
CI95 = c(xbar - ME , xbar + ME)
CI95
