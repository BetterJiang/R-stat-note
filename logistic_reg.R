

n = 1000; p = 2
x = matrix(rnorm(n*p, 0, 1), n, p)

x = matrix(runif(990*p, 0, 10), 990, p)
z1 = matrix(c(seq(0.3, 2, length.out =5), seq(8, 9, length.out = 5)), 5, 2)
z2 = matrix(c(seq(8, 9, length.out = 5), seq(0.3, 2, length.out =5)), 5, 2)
# x = rbind(x,matrix(c(seq(12,14,length.out = 5),seq(8,10,length.out = 5)),5,2))
x = rbind(x, z1)
x = rbind(x, z2)

beta = c(1, -1)
fx = x %*% beta

eps = rnorm(n, 0, 2)

# # y = drop(fx + eps)
# y = drop(fx)
# ly <- drop(y)
# ly[y > 0] <- 1
# ly[y < 0] <- 0
# #ly[986:990] <- 1
# ly[991:995] <- 1
# ly[996:1000] <- 0


y = plogis(fx, location = 0, scale = 1)
y = drop(fx)
ly <- drop(y)
ly[y >= 0.5] <- 1
ly[y < 0.5] <- 0


#xm <- matrix(0,(n-sum(ly))*2,p)
#sly <- rep(0,(n-sum(ly))*2)

#xm[1:(n - sum(ly)),] <- x[ly == 0,]
#sly[1:(n - sum(ly))] <- 0
#xm[(n - sum(ly) + 1):((n - sum(ly))*2),] <- x[ly == 1,][1:(n-sum(ly)),]
#sly[(n - sum(ly) + 1):((n - sum(ly))*2)] <- 1

#plot(xm)
#for (i in 1:length(sly)){
#  if (sly[i] == 1){
#    points(xm[i,1],xm[i,2],col = "blue")
#  }
#  else{
#    points(xm[i,1],xm[i,2],col = "red")
#  }
#}

#gl <- glm(sly~xm[,1] + xm[,2]+0,family = "binomial")
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






