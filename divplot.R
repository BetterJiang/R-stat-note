

rm(list = ls())
setwd("E:/hyjiang/keepInMindR")

library(timeDate)

load(file="E:/hyjiang/keepInMindR/allBottomDiv_Min60A_2016.RData")

x1 <- yearBottomDiv$CreateTime # number of levels, not the number of length as yearBottomDiv;

x <- timeDate(yearBottomDiv$CreateTime, format = "%H:%m:%d %H:%M")
## table(timeDate()) # returns wrong;

Xdate <- as.Date(yearBottomDiv$CreateTime, format= "%Y-%m-%d")
y <- table(Xdate)

barplot(y)


z <- data.frame(y)
z1 <- as.Date(z$Xdate, format= "%Y-%m-%d")
Xf <- unique(format(z1,"%Y-%m"))

zz <- data.frame(y, Xf)


# Add title, narrower bars, fill color, and change axis labels
ggplot(data=zz, aes(x=Xdate, y=Freq, fill=Xf)) + 
  geom_bar(colour="black", fill="#DD8888", width=.8, stat="identity") + 
  # guides(fill=FALSE) +
  xlab("Time of day") + ylab("Total bill") +
  ggtitle("Average bill for 2 people")




format("2016-12-30","%m")


Xdate <- as.Date(yearBottomDiv$CreateTime, format= "%Y-%m-%d")
y <- table(Xdate)
Xdate1 <- format(Xdate,"%Y-%m")

M <- as.integer(format( unique(Xdate),"%m")) # M=as.factor(format(unique(Xdate),"%m"));
numM <- tabulate(bin=M) # M are different months;
n1 <- length(numM) #length(numM) are length of different months;
require("RColorBrewer")
# c1 <- c( brewer.pal(n1, "Set3"))
cc <- c(brewer.pal(4, "Set1"), brewer.pal(n1-4,"Set3"))
color1 <- rep(cc, numM)

tl <- paste("bottom divergence distribution", sep=" ")
# # sink("myoutput.pdf",append = TRUE)
# jpeg( paste0(tl,'.jpeg'), width = 480*2, height = 480*2)
barplot(y, col=color1, main=tl, ylab="Divergence Number", xlab="Day")
# legend()
# dev.off()

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


p <- ggplot(zz, aes(x = Xf, y = Freq, fill=Xf)) + 
  geom_bar(stat="identity", aes(fill=Xf),colour="black", position = position_jitterdodge()) + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


n <- length(unique(Xf))
cols = gg_color_hue(n)
p1 <- p + scale_fill_manual(values=cols)

length()require(RColorBrewer)
brewer.pal(9, "Set1")

p + scale_fill_brewer(palette="Set3")

ggsave(filename="myPlot.jpg", plot=p1)

# Black outline for all