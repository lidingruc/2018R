#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Dec 21 09:29:23 2017

@author: liding
"""
# 安装说明1：http://www.cnblogs.com/sunada2005/p/selenium.html
# 安装说明2：http://blog.csdn.net/qq_33348497/article/details/77851623
# 使用示例1：http://www.zhidaow.com/post/selenium-phantomjs-xpath


from selenium import webdriver

browser = webdriver.PhantomJS()  #浏览器初始化；Win下需要设置phantomjs路径，linux下置空即可
url = 'http://www.zhidaow.com'  # 设置访问路径
browser.get(url)  # 打开网页
title = browser.find_elements_by_xpath('//h2')  # 用xpath获取元素

for t in title:  # 遍历输出
    print (t.text) # 输出其中文本
    print (t.get_attribute('class'))  # 输出属性值

browser.quit() 


browser = webdriver.PhantomJS()
url = 'http://www.aizhan.com/siteall/tuniu.com/'
browser.get(url)
table = browser.find_elements_by_xpath('//*[@id="index_history"]')  # 用Xpath获取table元素


for t in table:
    print (t.text)

browser.quit()