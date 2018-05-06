

# this function answers the question:
# why on average does each boostrap sample contains roughly 2/3 observations?


# rm(list = ls())
# getwd()
# setwd('D:/gitnote/R_stats_note')
# rm(list = ls())

# n <- 100

fboot <- function(n){
    b <- sample(1:n, n, replace = TRUE)
    rt <- length(unique(b)) / n
    return(rt)
}

lapply(1:3, fboot)

rt <- c(lapply(1:300, fboot), recursive=TRUE)
plot(rt)


1 - 1/exp(1)


fb <- function(n) 1 - (1 - 1/n)^n
rt <- c(lapply(1:1000, fb), recursive=TRUE)
plot(rt[1:10])


# Suppose you roll four times the identical six-sided dice. 
# The probability that you will see six unique numbers is very small: only 6! / 6^6 ≈ 0.015.

prob_dup_dice <- function(k){
    n <- 6
    choose(n, k) * factorial(k) / (n^k)
}

prob_dup_dice(6)

1 - prob_dup_dice(6)

prob_dup_dice(4)



# Among 23 people, the probability of a duplicate birthday is more than 50%
prob_dup_birth <- function(k){
    n <- 365
    choose(n, k) * factorial(k) / (n^k)
    # A_n^k/ n**k
}

k = 23
1 - prob_dup_birth(23)


