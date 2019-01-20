

set.seed(1)
n = 100
N = 1 + rpois(n, 5)
X1 = runif(n)
X2 = rexp(n)
s = X2 - X1 - 2
p = exp(s)/(1+exp(s))
vY = NULL
for (i in 1:n){
    Y = rbinom(1,prob = p[i],size = N[i])
    vY = c(vY,Y)
}


db = data.frame(Y = vY,N = N,X1,X2)
head(db,4)


# ##########################################
# ##########################################

set.seed(666)
x1 = rnorm(1000)           # some continuous variables 
x2 = rnorm(1000)
z = 1 + 2*x1 + 3*x2        # linear combination with a bias
pr = 1/(1+exp(-z))         # pass through an inv-logit function
set.seed(333)
y = rbinom(1000,1,pr)      # bernoulli response variable
plogis(z[1])
1/(1+exp(-z[1]))

#now feed it to glm:
df = data.frame(y=y,x1=x1,x2=x2)
res = glm(y~x1+x2,data=df, family="binomial")
res$coefficients


res2 <- glm(y ~ x1 + x2, data=data.frame(y, x1, x2), family = binomial(link = "logit"),
               control=list(maxit=100))

res2$coefficients
summary(res2)

m = 1000
d <- 2
Sigma <- diag(1, d, d)
set.seed(666)
x <- mvrnorm(m, mu=rep(0, 2), Sigma)
X <- cbind(rep(1, m), x)  # X[1,]
z <- X %*% c(1, 2, 3)

# sum(c(1, 2, 3) * X[2,])
# z[2]
# pr <- plogis(xb)  # 1 / (1 + exp(-qtl[1]))
pr <- 1 / (1 + exp(-z[,1]))
set.seed(333)
y <- rbinom(1000, size=1, prob=pr)

glm_res <- glm(y ~., data=data.frame(y, x), family = binomial(link = "logit"),
               control=list(maxit=100))

glm_res$coefficients

summary(glm_res)
