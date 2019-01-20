
n = 1000; p = 2
x = matrix(rnorm(n*p, 0, 1), n, p)

beta = c(1, -1)
fx = x %*% beta

########### 这样生成数据是错误的 ############
# eps = rnorm(n, 0, 2)
# y = drop(fx + eps)
# # y = drop(fx)
# ly <- drop(y)
# ly[y > 0] <- 1
# ly[y < 0] <- 0
########### 这样生成数据是错误的 ############

y = plogis(fx, location = 0, scale = 1)
ly = rbinom(n, size=1, prob = y)

gl <- glm(ly ~ x+0, family = "binomial") # x+0 表示没有截距项
summary(gl)


par(mfrow = c(1, 2))
plot(x)

plot(x)
for (i in 1:length(ly)){
    if (ly[i] == 1){
        points(x[i,1], x[i,2], pch = 21, bg = "green")
    }
    else{
        points(x[i,1], x[i,2], pch = 21, bg = "red")
    }
}
abline(a = 0, b = 1, lty = 2)
abline(a = 0, b = -coef(gl)[2]/coef(gl)[1], col = 'blue')

coef(gl)
beta

rbinom(4, size=1, prob = 0.5)




