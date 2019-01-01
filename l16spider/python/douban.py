__author__ = 'liding'
#coding=utf8
import requests
import json
import os
import time
from bs4 import BeautifulSoup
import csv
import random
#参考了知乎网页https://zhuanlan.zhihu.com/p/25658564 
#进行了修改，random、csv包需要加载
#这示例命令包括如何使用代理池，如何进行json数据读入
#num获取num页 国内高匿ip的网页中代理数据
def fetch_proxy(num):
    #修改当前工作文件夹
    os.chdir(r'/Users/liding/E/Bdata/ptemp')
    api = 'http://www.xicidaili.com/nn/{}'
    header = {
    'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_3) AppleWebKit/'
                  '537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36'}
    fp = open('host.txt', 'a+', encoding=('utf-8'))
    for i in range(num+1):
        api = api.format(1)
        respones = requests.get(url=api, headers=header)
        soup = BeautifulSoup(respones.text, 'lxml')
        container = soup.find_all(name='tr',attrs={'class':'odd'})
        for tag in container:
            try:
                con_soup = BeautifulSoup(str(tag),'lxml')
                td_list = con_soup.find_all('td')
                ip = str(td_list[1])[4:-5]
                port = str(td_list[2])[4:-5]
                IPport = ip + '\t' + port + '\n'
                fp.write(IPport)
            except Exception as e:
                print('No IP！')
        time.sleep(1)
    fp.close()


#百度网进行检验代理ip是否可用（大公司不怕咱们短时间内高频率访问）
def test_proxy():
    N = 1
    os.chdir(r'/Users/liding/E/Bdata/ptemp')
    url = 'https://www.baidu.com'
    fp = open('host.txt', 'r')
    ips = fp.readlines()
    proxys = list()
    for p in ips:
        ip = p.strip('\n').split('\t')
        proxy = 'https:\\' + ip[0] + ':' + ip[1]
        proxies = {'proxy': proxy}
        proxys.append(proxies)
    for pro in proxys:
        try:
            s = requests.get(url, proxies=pro)
            print('第{}个ip：{} 状态{}'.format(N,pro,s.status_code))
        except Exception as e:
            print(e)
        N+=1


# 生产代理池,可以结合上面的检验函数，自动选出合适的代理，
def proxypool(num):
    n = 1
    os.chdir(r'/Users/liding/E/Bdata/ptemp')
    fp = open('host.txt', 'r')
    proxys = list()
    ips = fp.readlines()
    while n<num:
        for p in ips:
            ip = p.strip('\n').split('\t')
            proxy = 'https:\\' + ip[0] + ':' + ip[1]
            proxies = {'proxy': proxy}
            proxys.append(proxies)
            n+=1
    return proxys

#爬取电影
def fetch_movies(tag, pages, proxys):
    os.chdir(r'/Users/liding/E/Bdata/ptemp')
    USER_AGENTS = [
    "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; AcooBrowser; .NET CLR 1.1.4322; .NET CLR 2.0.50727)",
    "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0; Acoo Browser; SLCC1; .NET CLR 2.0.50727; Media Center PC 5.0; .NET CLR 3.0.04506)",
    "Mozilla/4.0 (compatible; MSIE 7.0; AOL 9.5; AOLBuild 4337.35; Windows NT 5.1; .NET CLR 1.1.4322; .NET CLR 2.0.50727)",
    "Mozilla/5.0 (Windows; U; MSIE 9.0; Windows NT 9.0; en-US)",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Win64; x64; Trident/5.0; .NET CLR 3.5.30729; .NET CLR 3.0.30729; .NET CLR 2.0.50727; Media Center PC 6.0)",
    "Mozilla/5.0 (compatible; MSIE 8.0; Windows NT 6.0; Trident/4.0; WOW64; Trident/4.0; SLCC2; .NET CLR 2.0.50727; .NET CLR 3.5.30729; .NET CLR 3.0.30729; .NET CLR 1.0.3705; .NET CLR 1.1.4322)",
    "Mozilla/4.0 (compatible; MSIE 7.0b; Windows NT 5.2; .NET CLR 1.1.4322; .NET CLR 2.0.50727; InfoPath.2; .NET CLR 3.0.04506.30)",
    "Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN) AppleWebKit/523.15 (KHTML, like Gecko, Safari/419.3) Arora/0.3 (Change: 287 c9dfb30)",
    "Mozilla/5.0 (X11; U; Linux; en-US) AppleWebKit/527+ (KHTML, like Gecko, Safari/419.3) Arora/0.6",
    "Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.8.1.2pre) Gecko/20070215 K-Ninja/2.1.1",
    "Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9) Gecko/20080705 Firefox/3.0 Kapiko/3.0",
    "Mozilla/5.0 (X11; Linux i686; U;) Gecko/20070322 Kazehakase/0.4.5",
    "Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.8) Gecko Fedora/1.9.0.8-1.fc10 Kazehakase/0.5.6",
    "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.56 Safari/535.11",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_7_3) AppleWebKit/535.20 (KHTML, like Gecko) Chrome/19.0.1036.7 Safari/535.20",
    "Opera/9.80 (Macintosh; Intel Mac OS X 10.6.8; U; fr) Presto/2.9.168 Version/11.52",
    "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/536.11 (KHTML, like Gecko) Chrome/20.0.1132.11 TaoBrowser/2.0 Safari/536.11",
    "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.1 (KHTML, like Gecko) Chrome/21.0.1180.71 Safari/537.1 LBBROWSER",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; WOW64; Trident/5.0; SLCC2; .NET CLR 2.0.50727; .NET CLR 3.5.30729; .NET CLR 3.0.30729; Media Center PC 6.0; .NET4.0C; .NET4.0E; LBBROWSER)",
    "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; QQDownload 732; .NET4.0C; .NET4.0E; LBBROWSER)",
    "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.84 Safari/535.11 LBBROWSER",
    "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.1; WOW64; Trident/5.0; SLCC2; .NET CLR 2.0.50727; .NET CLR 3.5.30729; .NET CLR 3.0.30729; Media Center PC 6.0; .NET4.0C; .NET4.0E)",
    "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; WOW64; Trident/5.0; SLCC2; .NET CLR 2.0.50727; .NET CLR 3.5.30729; .NET CLR 3.0.30729; Media Center PC 6.0; .NET4.0C; .NET4.0E; QQBrowser/7.0.3698.400)",
    "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; QQDownload 732; .NET4.0C; .NET4.0E)",
    "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; Trident/4.0; SV1; QQDownload 732; .NET4.0C; .NET4.0E; 360SE)",
    "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; QQDownload 732; .NET4.0C; .NET4.0E)",
    "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.1; WOW64; Trident/5.0; SLCC2; .NET CLR 2.0.50727; .NET CLR 3.5.30729; .NET CLR 3.0.30729; Media Center PC 6.0; .NET4.0C; .NET4.0E)",
    "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/537.1 (KHTML, like Gecko) Chrome/21.0.1180.89 Safari/537.1",
    "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.1 (KHTML, like Gecko) Chrome/21.0.1180.89 Safari/537.1",
    "Mozilla/5.0 (iPad; U; CPU OS 4_2_1 like Mac OS X; zh-cn) AppleWebKit/533.17.9 (KHTML, like Gecko) Version/5.0.2 Mobile/8C148 Safari/6533.18.5",
    "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:2.0b13pre) Gecko/20110307 Firefox/4.0b13pre",
    "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:16.0) Gecko/20100101 Firefox/16.0",
    "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.11 (KHTML, like Gecko) Chrome/23.0.1271.64 Safari/537.11",
    "Mozilla/5.0 (X11; U; Linux x86_64; zh-CN; rv:1.9.2.10) Gecko/20100922 Ubuntu/10.10 (maverick) Firefox/3.6.10"
                 ]
 
    headers = {
    'User-Agent': random.choice(USER_AGENTS),
    'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
    'Accept-Language': 'en-US,en;q=0.5',
    'Connection': 'keep-alive',
    'Accept-Encoding': 'gzip, deflate'}
#    headers = {
#    'User-Agent': 'Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/'
#                  '537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Mobile Safari/537.36'}
#    cookies = dict()
    #用csv文件保存数据
    csvFile = open("{}.csv".format(tag), 'a+', newline='', encoding='utf-8')
    writer = csv.writer(csvFile)
    writer.writerow(('name', 'score', 'peoples', 'date', 'nation', 'actor'))
    for page in range(0, (pages*20+1), 20):
        url = 'https://movie.douban.com/tag/{}?start={}'
        url = url.format(tag, page)
        print(url)
        try:
            respones = requests.get(url, headers=headers,proxies=random.choice(proxys))
            while respones.status_code!=200:
                respones = requests.get(url, headers=headers,proxies=random.choice(proxys))
                print('再试')
            soup = BeautifulSoup(respones.text, 'lxml')
            movies = soup.find_all(name='div', attrs={'class': 'pl2'})
            for movie in movies:
                movie = BeautifulSoup(str(movie), 'lxml')
                movname = movie.find(name='a')
                # 影片名
                movname = movname.contents[0].replace(' ', '').strip('\n').strip('/').strip('\n')
                movInfo = movie.find(name='p').contents[0].split('/')
                # 上映日期
                date = movInfo[0][0:10]
                # 国家
                nation = movInfo[0][11:-2]
                actor_list = [act.strip(' ').replace('...', '') for act in movInfo[1:-1]]
                # 演员
                actors = '\t'.join(actor_list)
                # 评分
                score = movie.find('span', {'class': 'rating_nums'}).string
                # 评论人数
                peopleNum = movie.find('span', {'class': 'pl'}).string[1:-4]
                writer.writerow((movname, score, peopleNum, date, nation, actors))
            print('共有{}页，已爬{}页'.format(pages, int((page/20))))
        except Exception as e:
            print(e)
        #time.sleep(random.randint(0,3))
        

# 简单json数据获取示例
def douban_tv():
    tv_list=[]
    url='https://movie.douban.com/explore#!type=movie&tag=%E7%83%AD%E9%97%A8&sort=recommend&page_limit=20&page_start=0'
    r = requests.get(url,verify=False)
    content=r.text
    result=json.loads(content) # https://jsonformatter.curiousconcept.com/

    tvs=result['subjects']

    for i in range (0,len(tvs)):
        tv={}
        tv['rate']=tvs[i]['rate']
        tv['cover']=tvs[i]['cover']
        tv['url']=tvs[i]['url']
        tv['title']=tvs[i]['title']
        tv_list.append(tv)
    return tv_list


def douban_moviehot():
    url='https://movie.douban.com/j/search_subjects?type=movie&tag=%E7%83%AD%E9%97%A8&sort=recommend&page_limit=1000&page_start=0'
    r = requests.get(url,verify=False)
    content=r.content
    result=json.loads(content) # https://jsonformatter.curiousconcept.com/

    tvs=result['subjects']

    for i in range (0,len(tvs)):
        rate=tvs[i]['rate']
        cover=tvs[i]['cover']
        url=tvs[i]['url']
        title=tvs[i]['title']
        with open("豆瓣热门电影.txt",'a',encoding='utf-8') as f:
                f.write(title + ',' + rate + ',' + url + ',' +cover +'\n')


def douban_movie():
    urlb='https://movie.douban.com/j/new_search_subjects?sort=T&range=0,10&tags=&start='
    urle='&countries=%E4%B8%AD%E5%9B%BD%E5%A4%A7%E9%99%86'
    for i in range(0,1):
        j=i*20
        url = urlb+str(j)+urle
        print(url)
        r = requests.get(url,verify=False)
        content=r.content
        result=json.loads(content) # https://jsonformatter.curiousconcept.com/
        tvs=result['data']

        for i in range (0,len(tvs)):
            rate=tvs[i]['rate']
            cover=tvs[i]['cover']
            url=tvs[i]['url']
            title=tvs[i]['title']
            with open("豆瓣中国电影1.txt",'a',encoding='utf-8') as f:
                    f.write(title + ',' + rate + ',' + url + ',' +cover +'\n')

def main():
    start = time.time()
    #test_proxy()
    proxyPool= proxypool(2000)
    fetch_movies('中国', 3, proxyPool)
    end = time.time()
    lastT = int(end-start)
    print('耗时{}s'.format(lastT))

if __name__=="__main__":
     main()

#    result=douban_movie()
#    for i in result:
#        print (i)