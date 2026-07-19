
# Q1 ----------------------------------------------------------------------

x = 1760
n = 2000
p_hat = x / n

SE = sqrt((p_hat * (1 - p_hat)) / n)

# 90% CI
z90 = 1.645
CI_90 = c(p_hat - z90 * SE, p_hat + z90 * SE)

# 95% CI
z95 = 1.96
CI_95 = c(p_hat - z95 * SE, p_hat + z95 * SE)

# 99% CI
z99 = 2.576
CI_99 = c(p_hat - z99 * SE, p_hat + z99 * SE)

p_hat
CI_90
CI_95
CI_99


# Q2 ----------------------------------------------------------------------

n1 = 40
xbar1 = 56100
s1 = 6000

n2 = 50
xbar2 = 59400
s2 = 7000

alpha = 0.05

SE = sqrt((s1^2 / n1) + (s2^2 / n2))
t_stat = (xbar1 - xbar2) / SE

df = ((s1^2 / n1 + s2^2 / n2)^2) /
  (((s1^2 / n1)^2 / (n1 - 1)) + ((s2^2 / n2)^2 / (n2 - 1)))

p_value = pt(t_stat, df)

t_crit = qt(alpha, df)

t_stat
p_value
t_crit


# Q3 ----------------------------------------------------------------------
# Observed frequencies
observed = matrix(
  c(40, 55, 45, 50,
    60, 45, 55, 50),
  nrow = 2,
  byrow = TRUE
)

colnames(observed) = c("NE", "NW", "SE", "SW")
rownames(observed) = c("Purchase", "Do_Not_Purchase")

# Expected frequencies
expected = chisq.test(observed)$expected

# Chi-square test at alpha = 0.05
chi_result = chisq.test(observed, correct = FALSE)

observed
expected
chi_result



# Q4 ----------------------------------------------------------------------

# Data for each group
group1 = c(8, 5, 7, 11, 9, 6)
group2 = c(10, 12, 11, 9, 13, 12)
group3 = c(11, 14, 10, 16, 17, 12)
group4 = c(18, 20, 16, 15, 14, 22)

# Combine data
growth = c(group1, group2, group3, group4)
group = factor(rep(c("Natural", "Water", "Fertilizer", "Fert_Water"), each = 6))

alpha = 0.01

# Kruskal-Wallis test
kruskal_result = kruskal.test(growth ~ group)

kruskal_result

