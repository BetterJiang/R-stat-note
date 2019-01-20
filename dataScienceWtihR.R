
# LEARNING PATH: INTERMEDIATE DATA SCIENCE WITH R

install.packages("devtools")
devtools::install_github("rstudio/EDAWR")

install.packages(c("dplyr", "tidyr", "ggvis", "nycflights13"))


library(dplyr)
library(nycflights13)

help(package=nycflights13)
#  tbl 

flights
View(flights)
View(iris)
View(mtcars)
?mtcars

glimpse(flights)
glimpse(iris)
glimpse(mtcars)

dd <- flights$dep_delay
mean(dd, na.rm = TRUE)
dd %>% mean(na.rm = TRUE)

library(EDAWR)
?tb
?storms

storms

