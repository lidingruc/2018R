# 利用RScripts演示数据分析
# 数据科学与社会研究:基于R和Python的实践 2017年
# 李丁(liding@ruc.edu.cn)
# 中国人民大学社会与人口学院

# 加载后面的分析中需要用到的包
library(dplyr)
library(readr)
library(ggplot2)
library(gganimate)

#读入数据
setwd("/Users/liding/E/Bdata/liding17/2017R/lesson3/")
gapminder <- read.csv("gapminder.csv")

#初步了解数据
names(gapminder)
head(gapminder)
View(gapminder)
dim(gapminder)
table(gapminder$year)

#选择2007年数据进行分析
gap07 <- gapminder %>%
  filter(year == 2007)

# 散点图呈现GDP和预期寿命的关系
qplot(x = gdpPercap, y = lifeExp, data = gap07)

# 分大陆的关系
qplot(x = gdpPercap, y = lifeExp, color = continent, data = gap07)

# 将人口规模信息也放进去
qplot(x = gdpPercap, y = lifeExp, color = continent, size = pop,data = gap07)

# 制作动态图形的方法
getwd()
gapminder_plot <- ggplot(gapminder) +
  aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop,
      frame = year) +
  geom_point(alpha = 0.4) +
  scale_x_log10()
# gapminder_plot
gganimate(gapminder_plot, convert='gm convert', filename = "gapminder-gganimate.gif")

