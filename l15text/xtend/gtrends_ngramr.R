
# 用R绘制google趋势
# google 预料库（2012版）词频趋势分析
# 参考了 https://gist.github.com/erzk/3a24c10be67c640d2d6a/
# ngrams 数据http://storage.googleapis.com/books/ngrams/books/datasetsv2.html
# 需要用北大vpn

# install necessary packages
devtools::install_github("hubte1g/GTrendsR")
devtools::install_github("trinker/gtrend")
devtools::install_github("seancarmody/ngramr")

# load the packages
library(curl)
library(dplyr)
library(gtrend)
library(GTrendsR)
library(ggplot2)
library(ngramr)

# query Google Ngrams and create a plot
# "Sigmund Freud", "Carl Jung"
ngramData <- ngrami(c("Karl Marx","Max Weber","Emile Durkheim"), year_start = 1850)

ggplot(ngramData, 
       aes(Year, Frequency, colour = Phrase)) + 
  geom_line(lwd = 1) +
  stat_smooth()

ng  <- ngram("python", year_start = 1800)
ggplot(ng, aes(x=Year, y=Frequency)) +
  geom_line()

# exmaple1
ng  <- ngram(c("hacker", "programmer"), year_start = 1950)
ggplot(ng, aes(x=Year, y=Frequency, colour=Phrase)) +
  geom_line()

# exmaple2
ggram(c("monarchy", "democracy"), year_start = 1500, year_end = 2000, 
      corpus = "eng_gb_2012", ignore_case = TRUE, 
      geom = "area", geom_options = list(position = "stack")) + 
  labs(y = NULL)

# 
hacker
ggram(hacker) + facet_wrap(~ Corpus)


#ngram 主题风格
ng <- c("((The United States is + The United States has) / The United States)",
        "((The United States are + The United States have) / The United States)")
ggram(ng, year_start = 1800, google_theme = TRUE) +
  theme(legend.direction = "vertical")

# 相对频次
ng <- c("(Karl Marx / sociology)",
        "(Max Weber / sociology)","Emile Durkheim / sociology")
ggram(ng, year_start = 1800, google_theme = TRUE) +
  theme(legend.direction = "vertical")




# 因为防火墙可能上不了
# 需要穿墙
# query Google Trends
gtrendQueries <- c("Sigmund Freud", "Carl Jung")
# add your Google account details
outputGT <- gtrend_scraper("你的gmail账号", "gmail密码", gtrendQueries)

# create a plot
outputGT %>%
  trend2long() %>%
  plot() +
  stat_smooth()

