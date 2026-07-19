
# Using CSV File ----------------------------------------------------------

x = read.csv(file.choose(),header =T)
x
# columnwise entry
data = c(as.matrix(x)) # convert dataframe to 
data
# rowwise entry
data1 = c(t(as.matrix(x)))
data1

r = gl(3,5,15,labels=c("A","B","C"))
r
model = aov(data ~ r)
model
r1 = gl(3,1,15,labels=c("A","B","C"))
r1
model1=aov(data1 ~ r1)
model1


# PDF PAGE 556 eg 1 ch 13
v1 = c(162,142,165,145,148,174)
v2=c(142,156,124,142,136,152)
v3 = c(126,122,138,140,150,128)
data = c(v1,v2,v3)
r = gl(3,6,18,labels=c("A","B","C"))
model = aov(data ~ r)
model
x = summary(model);x
# pvalue 0.016<= alpha reject H0

# PDF PAGE 559 eg 11 ch 13
v1 = c(128,137,135,124,141)
v2=c(144,133,142,146,130)
v3 = c(133,143,137,136,131)
v4 = c(150,142,135,140,153)
data = c(v1,v2,v3,v4)
r = gl(4,5,20,labels=c("Paint1","Paint2","Paint3","Paint4"))
model = aov(data ~ r)
x = summary(model) ;x


# Randomized Block Design -------------------------------------------------
# row and column there in ques use randomized block design

# Page 566 sum

x = read.csv(file.choose(),header =T)
x
data = c(as.matrix(x))
data

tm1 = gl(3,6,18,labels=c("System A","System B","System C"));tm1 # treatment Coloumn
block = gl(6,1,18,labels=c("R1","R2","R3","R4","R5","R6")) ;block  # blockwise Rowwise
model = aov(data ~ tm1+block);model
x=summary(model) ;x

# Eg 26 Pdf page 571
data = c(526,594,465,561,436,430,534,590,464,566,478,458,530,586,445,553,430,420)
data # column wise entry
tm1 = gl(3,6,18,labels=c('Critical Reading','Mathematics','Writing'))
block = gl(6,1,18,labels=c(1,2,3,4,5,6))
model = aov(data ~ tm1+block)
x = summary(model)
x 


# pg 573 12