# 函数的增长速度
# log2(n) < n^(1/2) < n < n * log2(n)
# n^2 < 2^n < factorial(n) < n^n

f1 <- function(n) log2(n)
f2 <- function(n) n^(1/2)
f3 <- function(n) n
f4 <- function(n) n * log2(n)
f5 <- function(n) n^2
f6 <- function(n) n^3
f7 <- function(n) 2^n
f8 <- function(n) factorial(n)
f9 <- function(n) n^n


x <- 1:100
plot(x, c(lapply(x, f2), recursive=TRUE), 'l', col='red')
points(c(lapply(x, f1), recursive=TRUE), type ='l', col='green')


x <- seq(0, 50, length.out = 100)
plot(x, c(lapply(x, f2), recursive=TRUE), 'l', col='red')
points(c(lapply(x, f3), recursive=TRUE), type ='l', col = "dark red")


x <- 1:100
plot(x, c(lapply(x, f4), recursive=TRUE), 'l', col='red')
points(c(lapply(x, f3), recursive=TRUE), type ='l', col = "green")

x <- 1:100
plot(x, c(lapply(x, f5), recursive=TRUE), 'l', col='red')
points(c(lapply(x, f4), recursive=TRUE), type ='l', col = "blue")
points(c(lapply(x, f3), recursive=TRUE), type ='l', col = "green")


x <- 0:5
plot(x, c(lapply(x, f8), recursive=TRUE), 'l', col='red')
points(c(lapply(x, f7), recursive=TRUE), type ='l', col = "dark red")


x <- 0:20
plot(x, c(lapply(x, f9), recursive=TRUE), 'l', col='red')
points(c(lapply(x, f8), recursive=TRUE), type ='l', col = "dark red")


