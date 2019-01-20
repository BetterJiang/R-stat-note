

# hypothesis testing 


n = 100
Xbar = 0.5
sigma  = 0.8
1 - pnorm(sqrt(n)*Xbar/sigma)


sigma_hat = 1.4
1 - pt(sqrt(n)*Xbar/sigma_hat, n-1)


## Column 1 is the p-value, column 2 is the proportion of null test
## statistics exceeding the actual test statistic.
PQ = array(0, c(1000,2))
for (r in (1:1000)) {
    ## Generate an alternative data set. n, number of samples.
    n = 20
    X = rnorm(n, 0, 1) + runif(n, 0, 1)
    ## Get the p-value, assuming that the variance is known to be 1.
    M1 = mean(X)
    p = 1-pnorm(sqrt(20)*M1/1)
    ## Check the accuracy of the p-value using simulated null data.
    Z = array(rnorm(1000*n), c(1000, n))
    M2 = apply(Z, 1, mean)   # 1, is for each row, calculate the mean,
    PQ[r,1] = p
    PQ[r,2] = mean(M2 > M1)
}

hist(PQ[,1])
hist(PQ[,2])
hist(PQ[,1] - PQ[,2])



## Sample size.
n = 20
## Storage for null p-values.
Q = array(0, 1e4)
## Simulation loop.
for (r in 1:1e4) {
    ## Generate a null data set.
    X = rnorm(n, 0, 1)
    ## Get the p-value, assuming that the variance is known to be 1.
    M1 = mean(X)
    Q[r] = 1-pnorm(sqrt(n)*M1)
}

hist(Q)



