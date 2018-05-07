
a <- 0.5
x <- seq(-100, 100, 1)

y <- 450 + a*(x-10)^3
plot(x, y, type='l',col='navy',main='Nonlinear relationship',lwd=3)

# 怎样拟合一个多项式回归
# 首先，当我们要创建一串虚拟随机数的时候，我们必须总要记得写set.seed(n)。这样做，随机数生成器总能产生同等数目的数据。

set.seed(20)
# 预测变量x：使用seq来快速产生等间距的序列：
x <- seq(from=0, to=20, by=0.1)
y <- 500 + 0.4 * (x-10)^3
y2 <- 0.4 * x^3 - 12 * x^2 + 120 * x + 100
# 我们现在产生一些噪音并把它添加到模型中：
noise <- rnorm(length(x), mean=10, sd=80)
noisy.y <- y + noise
# 对噪声数据进行画图：
plot(x, noisy.y, col='deepskyblue4', xlab='q', main='Observed data')
lines(x, y, col='firebrick1', lwd=3)
lines(x, y2, col='blue', lwd=3)



# 我们得出的模型应当是 y = a x + b x^2 + c* x3 + epsilon
# 现在，我们用R对此进行模拟。要拟合一个多项式模型，你也可以这样用：
model <- lm(noisy.y ~ poly(x, 3))
model <- lm(noisy.y ~ poly(x, degree=2, raw=TRUE))
summary(model)

# 或者：
model_2 <- lm(noisy.y ~ x + I(x^2) + I(x^3))
summary(model_2)

# 然而，我们要知道x，I(x^2)，I(x^3)存在相关的关系，而这些相关变量很有可能引起某些问题的产生。
# 这时，使用poly()可以避免这个问题，因为它是创建一个垂直的多项式。
# 因此，我喜欢第一种方法：

# 我们可以使用confint()来获得一个模型的参数的置信区间。
# 模型参数的置信区间：
confint(model, level=0.95)


# 现在，我们要作一个拟合VS残差图。
# 如果这是一个拟合效果比较不错的模型，我们应该看不到任何一种模型的模式特征：

plot(fitted(model), residuals(model))

# 整体来说，这个模型的拟合效果还是不错的，毕竟Multiple R-squared:  0.8047。
# 第一和第三个订单序列的系数，在统计学当中is significant，这样在我们的意料之中。
# 现在，我们可以使用predict()函数来获得拟合数据以及置信区间，这样，我们可以不按照数据来作图。
# 下面是预测值和预测置信区间：


predicted.intervals <- predict(model, data.frame(x=x), interval='confidence', level=0.99)

# 对噪声数据进行画图：
plot(x, noisy.y, col='deepskyblue4', xlab='q', main='Observed data')
lines(x, y, col='firebrick1', lwd=3)
# 在已有的图像中添加拟合线：
lines(x, predicted.intervals[,1], col='green', lwd=3)
lines(x, predicted.intervals[,2], col='black', lwd=1)
lines(x, predicted.intervals[,3], col='black', lwd=1)
# 添加图例：
legend("bottomright", c("Observ.","Signal","Predicted"), 
       col=c("deepskyblue4","red","green"), lwd=3)


model$coefficients


