

# iid sample data ~ from Bernoulli(p)
# I want to how the Bernoulli log likelihood changes with p
# 

n = 100
p = 0.2
x = rbinom(n, 1, prob = p)
s = (1/p + 1/(1-p))*sum(x) - n/(1-p)

loglikeli <- function(n, p){
  x = rbinom(n, 1, prob = p)
  loglike = log(p)*sum(x) + (n - sum(x))*log(1-p)
  return(loglike)
}


score <- function(n, p){
  x = rbinom(n, 1, prob = p)
  score = (1/p + 1/(1-p))*sum(x) - n/(1-p)
  return(score)
}

pVec = seq(0, 1, 0.01)[-c(1,101)]
num = length(pVec)
logL = rep(0, num)
S = rep(0, num)
n = 100
for(i in 1:num){
  logL[i] = loglikeli(n, pVec[i])
  S[i] = score(n, pVec[i])
}

scaled_logL =  logL/max(logL)
plot(pVec, scaled_logL)



