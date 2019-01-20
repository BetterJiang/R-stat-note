

load("xxx.RData")
load("ac1.RData")
x2 <- rbind(ac1, xxx)

AC=x2



Z <- yearIxDaily$DIF


plot(pressure$temperature, pressure$pressure, type = "l")
points(pressure$temperature, pressure$pressure)
lines(pressure$temperature, pressure$pressure, col = "green")

lines(pressure$temperature, pressure$pressure/2, col = "red")
points(pressure$temperature, pressure$pressure/2, col = "red")



library(ggplot2) # install.packages("ggplot2")
qplot(pressure$temperature, pressure$pressure, geom = "line")
ggplot(data = pressure, aes(x=temperature, y=pressure)) + geom_line()

qplot(pressure$temperature, pressure$pressure, geom = c("line", "point"))
# the same as the follows
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line() + geom_point()

BOD
class(BOD)

barplot(BOD$demand, names.arg = BOD$Time)

mtcars
class(mtcars)
mtcars$cyl
table(mtcars$cyl)
barplot(table(mtcars$cyl))


library(ggplot2)

ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth = 4)

install.packages("gcookbook")
library(gcookbook)
x <- climate
names(climate)
csub <- subset(climate, Source=="Berkeley" & Year >=1900)
csub$pos <- csub$Anomaly10y >= 0


ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) + 
  geom_bar(stat="identity", position = "identity", colour = "black", size= 0.25) +
  scale_fill_manual(values = c("#CCEEFF", "#FFDDDD"), guide = FALSE)

