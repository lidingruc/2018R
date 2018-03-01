# 2018R
这里我创建的是2017年秋季学期的《数据科学与社会研究》的课程资料，2018年在北京大学《互联网与社会研究：技术基础》的课程上做了一些修改。

互联网特别是移动互联网对社会生活产生了巨大的影响，非常值得社会学去关注和研究。采用传统的研究方法当然也能得到很好的研究成果，但如果能够深入理解构建起互联网的技术与信息基础，并能利用这些技术和信息对互联网基础上发生的人类活动及组织运作进行研究，有可能会更深刻一步。即便你做技术专家，也应该懂点技术。

课程针对没有R编程经验、统计基础的本科生、研究生；难度不大，但需要高强度练习。重点在于让学生们对数据科学产生强烈兴趣，了解数据科学技能累积的学习路径，掌握基本的数据获取、整理、分析和结果呈现方法。在能用R替代SAS、SPSS、stata的基础上，并对新数据及大数据的采集和使用有所了解。

# 课程初步结构如下

## 第1讲、望远：备战大数据时代

课程介绍：大数据时代对青年人的要求，如何达到这些要求！
PPT文件：https://github.com/lidingruc/2018R/blob/master/%E7%AC%AC%E4%B8%80%E8%AE%B2.pdf

课程指引：http://note.youdao.com/noteshare?id=351a5e712274bd552b70aeb557a9cae5

课后作业（不用提交）：尝试安装软件，不懂则问；速读一些与大数据或互联网相关的通俗读物，加深对于大数据的理解。
可以开始阅读《R语言实战》或者下面的章节，了解两本主要的教材风格和结构。

http://moderndive.com/index.html

http://moderndive.com/2-getting-started.html

http://r4ds.had.co.nz/introduction.html

http://r4ds.had.co.nz/workflow-scripts.html

http://r4ds.had.co.nz/workflow-basics.html

http://r4ds.had.co.nz/workflow-projects.html


## 第2讲、登高：数据、信息、知识与理论

课前预习：找一本社会研究方法教材速读一遍或者看看邱泽奇老师的社会调查研究方法的在线课程，对社会研究有些概念。

课堂演示：社会研究方法体系串讲（PPT ）。

PPT文件：https://github.com/lidingruc/2018R/blob/master/%E7%AC%AC%E4%BA%8C%E8%AE%B2.pdf

课后作业（不用提交）：针对大数据时代和社会研究方法中的某个问题，你如何学习、积累并将之解决？简单规划一下自己的学习路径。

继续阅读此前没有看完的内容：
http://moderndive.com/index.html

http://moderndive.com/2-getting-started.html

http://r4ds.had.co.nz/introduction.html

http://r4ds.had.co.nz/workflow-scripts.html

http://r4ds.had.co.nz/workflow-basics.html

http://r4ds.had.co.nz/workflow-projects.html


## 第3讲、立靶：R数据汇总和可复制性研究

课前预习：阅读r4ds教材中数据可视化和数据分析流程部分

http://r4ds.had.co.nz/introduction.html

http://r4ds.had.co.nz/workflow-scripts.html

http://r4ds.had.co.nz/workflow-basics.html

http://r4ds.had.co.nz/workflow-projects.html


http://r4ds.had.co.nz/data-visualisation.html


http://r4ds.had.co.nz/r-markdown.html

http://r4ds.had.co.nz/r-markdown-formats.html

http://r4ds.had.co.nz/graphics-for-communication.html


课堂演示：R界面和操作的简单介绍，tidyverse数据分析过程展示，github的使用展示，时间允许详细展示data-visualisation并介绍作业的做法。

演示概要：

http://note.youdao.com/noteshare?id=94c815919f88613d071d2254934ca53e

提交作业1：将http://r4ds.had.co.nz/data-visualisation.html一章中的命令复制到R scripts中，并运行，如果有错误，找出，并尝试解决。
自己要强化学习。

## 第4讲、夯基：关于R的基础

课前预习：肖凯90分钟（推荐,优酷搜索 R语言快速入门 SupStat 分5集 ）：http://v.youku.com/v_show/id_XNjYyNzczMTgw.html?spm=a2h0j.11185381.listitem_page1.5!5~A&f=23488136&from=y1.2-3.4.5

http://r4ds.had.co.nz/wrangle-intro.html

http://r4ds.had.co.nz/tibbles.html

http://r4ds.had.co.nz/data-import.html

http://r4ds.had.co.nz/transform.html


课堂演示：R基础入门：R中的对象、函数、控制语句与数据框操作

提交作业2：将http://r4ds.had.co.nz/transform.html 中的命令转移到R scripts文件中，并完成其中的练习题。


## 第5讲、备砖：数据管理（变量层次的管理）
课前预习：

http://moderndive.com/5-wrangling.html

http://r4ds.had.co.nz/factors.html

http://r4ds.had.co.nz/strings.html

http://r4ds.had.co.nz/dates-and-times.html


提交作业3：将http://moderndive.com/5-wrangling.html中的命令转移到R scripts文件中，并完成其中的练习题。


## 第6讲、备砖：数据管理（数据层次的管理）
课前预习：

http://r4ds.had.co.nz/tidy-data.html

http://moderndive.com/4-tidy.html

http://r4ds.had.co.nz/relational-data.html


提交作业4：将http://r4ds.had.co.nz/tidy-data.html中的命令转移到R scripts文件中，并完成其中的练习题。


## 第7讲、探索：统计描述与ggplot作图
课前预习：

http://moderndive.com/5-wrangling.html（分类汇总部分）

http://r4ds.had.co.nz/exploratory-data-analysis.html

http://moderndive.com/3-viz.html

http://r4ds.had.co.nz/data-visualisation.html


提交作业5：将用http://r4ds.had.co.nz/exploratory-data-analysis.html中的命令转移到R scripts文件中，并完成其中的作业题。


## 第8讲、可视化：R基础作图

课前预习：

课堂演示：利用基础命令作图

提交作业6：将http://moderndive.com/3-viz.html中的命令转移到R scripts文件中，并完成其中的learning check


## 第9讲、推论：统计检验
课前预习:第二讲讲义中关于抽样和统计推论的部分

http://moderndive.com/7-sim.html

http://moderndive.com/B-appendixB.html

http://moderndive.com/8-sampling.html

http://moderndive.com/9-ci.html

http://moderndive.com/10-hypo.html

https://github.com/andrewpbray/infer


课堂演示:如何用R来进行卡方检验、T检验、方差检验、分析检验，模拟抽样分布

提交作业7：运行http://moderndive.com/10-hypo.html中的命令，并完成其中的learning check



## 第10讲、建模：一般线性回归

课前预习：回归模型相关的内容

http://r4ds.had.co.nz/model-basics.html

http://r4ds.had.co.nz/model-building.html

http://moderndive.com/6-regression.html

http://moderndive.com/7-multiple-regression.html

http://moderndive.com/11-inference-for-regression.html

提交作业8：将http://r4ds.had.co.nz/model-basics.html对应的rmd文件中的命令誊到R script上 运作一遍。

## 第11讲、建模：回归诊断与拓展
课前预习：预习回归诊断、模型选择、logit回归内容

交互效应：http://faculty.smu.edu/kyler/courses/7312/interact.pdf
回归诊断：https://socialsciences.mcmaster.ca/jfox/Courses/Brazil-2009/index.html
GLM模型：https://socialsciences.mcmaster.ca/jfox/Courses/SPIDA/index.html
SEM模型：https://socialsciences.mcmaster.ca/jfox/Courses/R/IQSBarcelona/index.html
高级模型与编程： https://socialsciences.mcmaster.ca/jfox/Courses/R/Peking/index.html

课堂演示

课后作业（不提交）：将课堂示例操练一下。


## 第12讲、综合：实例和练习

了解实例的数据和问题，分解出任务步骤，尝试寻找方案。

分成几个小组：复现几个实例。你们小组的结论与原作者的结论相同吗？有改进吗？
https://ismayc.github.io/soc301_s2017/group-projects/index.html

提交作业9（课堂小组合作，单独提交）


## 第13讲、连通：网络分析

课前预习：预习社会网络分析的基本概念和历史

自学材料：http://note.youdao.com/share/?id=28c7b0a4e947ae29462fb424cf11dd21&type=note#/

陈华珊：http://www.istata.cn/wp-content/uploads/2013/11/huashan_sna_visualization_2017.pdf

课堂演示：网络数据的基本概念与描述

提交作业10：选取相关演示命令的一部分或者全部（不少于100行），转换成为rmd文件。


## 第14讲、邻里：空间分析

课前预习：空间分析的基本原理

入门材料：http://note.youdao.com/noteshare?id=92cbe89d3e03cc530ac28c4a0eb6449e
朱可夫：http://www.people.fas.harvard.edu/~zhukov/spatial.html
孙秀林空间建模：http://note.youdao.com/noteshare?id=4f4180ea28db7fdd238b882b681c5cd2

课堂演示：空间数据的基本介绍与作图 

提交作业11：选取相关演示命令的一部分或者全部（不少于300行），转换成为rmd文件。


## 第15讲、异型：文本分析

课前预习：

入门材料：http://note.youdao.com/noteshare?id=f0d94703ba72b57c54ad9318bdf0f274

课堂演示：文本分词、词云、主题、情感分析

提交作业12：选取相关演示命令的一部分或者全部（不少于300行），转换成为rmd文件。


## 第16-17讲、采集：爬虫与数据获取

课前预习：

http://note.youdao.com/noteshare?id=57cc2a7d0f893b58d4fbb217f65f167d

http://note.youdao.com/noteshare?id=6d0aab0f55880292730ff9535488b356

课堂演示：用R爬取数据的原理与实战


提交作业13（课堂小组合作，单独提交）：尝试爬取北大未名BBS所有学生社团版面开版时间

https://bbs.pku.edu.cn/v2/board.php?bid=682


## 第18讲、新招：python爬虫

课前预习：

安装说明：http://note.youdao.com/noteshare?id=8b5797ca96ee80737a6a9048c0423b6f

http://note.youdao.com/noteshare?id=ab8f1f4e84519a6eb3881c8d7ca37841

http://note.youdao.com/noteshare?id=aa3b31703ff6468eb2c884494e11b939

http://note.youdao.com/noteshare?id=8d72a2741f381b292d40c7583047c891

课后作业（不提交）：安装配置好python后，尝试运行示例命令。
 


