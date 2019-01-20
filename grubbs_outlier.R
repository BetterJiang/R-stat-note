
x = c(199.31, 199.53,200.19, 200.82, 201.92, 201.95, 202.18, 245.57)
n = length(x)

x1 = min(x)
x2 = max(x)
u = mean(x)
s0 = sd(x)
s = sqrt(sum((x-u)^2)/ (n-1)) ## the same as sd(x)



g1 = (u-x1)/s
g2 = (x2-u)/s
gx = abs(x- u)/s

n = length(x)
a = 0.05
ta = qt(p=0.05/(2*n), df=n-2,lower.tail = FALSE, log.p = FALSE)
ga = (n-1)/sqrt(n)*sqrt(ta^2/(n-2+ta^2))


pt((ta - u)/s, df=n-2, lower.tail = T) *2



# Rcommands and output: 
## Input data from the Tietjen and Moore paper. 
y = c(199.31,199.53,200.19,200.82,201.92,201.95,202.18,245.57) 
## Generate normal probability plot. 
qqnorm(y, main="Normal Probability Plot of Mass Spectrometer Measurements") 
## Attach "outliers" library and perform Grubbs test for one outlier.
library(outliers)
grubbs.test(y, type=10) 


# ## Grubbs test for one outlier
# data: y
# G = 2.4688, U = 0.0049, 
# p-value = 1.501e-07
# alternative hypothesis: highest value 245.57 is an outlier




