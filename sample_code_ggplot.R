# rm(list = ls())
getwd()

setwd("E:/hyjiang")
load(file = "Daily-2016select-2016-12-07.RData")

library(ggplot2)
mtcars
qplot(mtcars$wt,mtcars$mpg)
qplot(wt, mpg, data = mtcars)
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()

ggplot(mtcars)



# Generate some sample data, then compute mean and standard deviation
# in each group
df <- data.frame(
  gp = factor(rep(letters[1:3], each = 10)),
  y = rnorm(30)
)

ds <- plyr::ddply(df, "gp", plyr::summarise, mean = mean(y), sd = sd(y), med=median(y)) # help plyr;

# The summary data frame ds is used to plot larger red points on top
# of the raw data. Note that we don't need to supply `data` or `mapping`
# in each layer because the defaults from ggplot() are used.
ggplot(df, aes(gp, y)) +
  geom_point() +
  geom_point(data = ds, aes(y = mean), colour = 'red', size = 3) + 
  geom_point(data = ds, aes(y = med), colour = 'green', size = 3)





