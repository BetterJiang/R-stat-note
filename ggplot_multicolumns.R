



require(ggplot2)
require(grid)
#####现将图画好，并且赋值变量，储存#####
a <- ggplot(mtcars, aes(mpg, wt, colour = factor(cyl))) + geom_point()
b <- ggplot(diamonds, aes(carat, depth, colour = color)) + geom_point()
c <- ggplot(diamonds, aes(carat, depth, colour = color)) + geom_point() + 
  facet_grid(.~color,scale = "free") 

########新建画图页面###########
grid.newpage()  ##新建页面
pushViewport(viewport(layout = grid.layout(2,2))) ####将页面分成2*2矩阵
vplayout <- function(x,y){
  viewport(layout.pos.row = x, layout.pos.col = y)
}
print(c, vp = vplayout(1,1:2))   ###将（1,1)和(1,2)的位置画图c
print(b, vp = vplayout(2,1))   ###将(2,1)的位置画图b
print(a, vp = vplayout(2,2))  ###将（2,2)的位置画图a
# dev.off() ##画下一幅图，记得关闭窗口




# 读入数据和加载包
data <-read.csv('d:\\data.csv',T)
library(ggplot2)
library(ellipse)
library(gridExtra)
library(plyr)
library(ReadImages)

#建立一个函数以生成置信椭圆
generatfun <- function(x) {
  as.data.frame(with(data,ellipse(cor(a,b),scale=c(sd(a),sd(b)),
                                  level=x,centre=c(mean(a),mean(b)))))}
# 根据不同的置信度来生成多个数据框并整合
i <- seq(0.1,0.9,by=0.1)
data2 <- adply(i,1,generatfun)
names(data2) <- c('level','x','y')

# 绘制主图散点图，并将图例去除，这里point层和path层使用了不同的数据集
scatter <- ggplot() + 
  geom_point(data=data,aes(a,b,shape=type))+
  geom_path(data=data2,aes(x,y,group=level))+
  opts(legend.position = "none")+
  geom_vline(xintercept = mean(data$a),linetype=2)+
  geom_hline(yintercept = mean(data$b),linetype=2)
# 绘制上边的直方图，并将各种标注去除
hist_top <- ggplot()+geom_histogram(aes(data$a),colour='black',fill='gray',binwidth = 0.3)+
  opts(panel.background=theme_blank(),
       axis.title.x=theme_blank(), 
       axis.title.y=theme_blank(),
       axis.text.x=theme_blank(),
       axis.text.y=theme_blank(),
       axis.ticks=theme_blank())
# 同样绘制右边的直方图
hist_right <- ggplot()+geom_histogram(aes(data$b),colour='black',fill='gray',binwidth = 0.1)+
  opts(panel.background=theme_blank(),
       axis.title.x=theme_blank(),
       axis.title.y=theme_blank(),
       axis.text.x=theme_blank(),
       axis.text.y=theme_blank(),
       axis.ticks=theme_blank())+
  coord_flip()
# 要由四个图形组合而成，可以用空白图作为右上角的图形也可以，但为了好玩加上了R的logo，这是一种在ggplot中增加jpeg位图的方法
logo <- read.jpeg("d:\\Rlogo.jpg")
empty <- ggplot(data.frame(x=1:10,y=1:10),aes(x,y))+
  annotation_raster(logo,-Inf, Inf, -Inf, Inf)+
  opts(axis.title.x=theme_blank(),
       axis.title.y=theme_blank(),
       axis.text.x=theme_blank(),
       axis.text.y=theme_blank(),
       axis.ticks=theme_blank())
# 最终的组合
grid.arrange(hist_top, empty, scatter, hist_right, ncol=2, nrow=2, widths=c(4,1), heights=c(1,4))







