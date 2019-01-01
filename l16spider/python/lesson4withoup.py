# -*- coding: utf-8 -*-
# python 进阶内容

# 登录网页+动态代码问题
# Scrapy框架
# 其他复杂问题：动态加载+分布式+数据库
##1. 针对一些反扒取的数据或者比较难爬取的数据应该怎么办呢？
##2. 针对需要动态加载的页面该如何处理呢？类似于淘宝、知乎等
##3. 爬取大规模数据中断了怎么办？¶
##4. 爬取大规模数据怎么更快？
##5. 数据库使用和数据导出


###############################################
# 手工复制cookies和agent登录
from bs4 import BeautifulSoup
import requests
##一个登陆知乎的示例
#请各位同学自行登入自己的账户后使用检查后，获得相应的数据
url = 'https://www.zhihu.com/'
headers = {
    'User-Agent':"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.81 Safari/537.36",
    'Cookie':'aliyungf_tc=AQAAAMBm5GWfhQIA6GFkdT/lc4PnZVLw; acw_tc=AQAAAHRCRnKnzgIA6GFkdSO7zbpgmIM/; q_c1=3dac8e87d94d4986ad07bdb1d575e35c|1493955867000|1493955867000; _xsrf=8c0780d645e5cd79aa7c3cb8eff4bad7; d_c0="AEACozmovQuPTizSJ6tkaBSooaUbjyFrywo=|1494486235"; _zap=2589f203-4e9c-4a52-9a12-3188ef02b6bb; r_cap_id="MjdhZGRmMzEzZjliNGQyZmExYzZjYjE0YmQwZmI1ODk=|1494493029|6a061922c246cbd2410e79b18645abd09f6c7e86"; cap_id="MTZjZTc3NGRjZTJjNGRjNWE2OTAyNmY5MWRlMjVjNjk=|1494493156|6e46420bc6846c5ceedececa6c1b6995389c1f43"; __utma=51854390.1148585147.1494486236.1494486236.1494491973.2; __utmc=51854390; __utmz=51854390.1494491973.2.2.utmcsr=zhihu.com|utmccn=(referral)|utmcmd=referral|utmcct=/; __utmv=51854390.000--|3=entry_date=20170505=1; l_n_c=1; z_c0=Mi4wQUREQ19OREZkUXNBUUFLak9haTlDeGNBQUFCaEFsVk5Hclk3V1FDTVc0ZjhONjhOVU45Tnd5WWZSQ2pXYmZ0ZTd3|1494504216|4a92887d4d370c0c66eaa6f4d8128b461ef995be'
    }
wb_data = requests.get(url,headers = headers)
soup = BeautifulSoup(wb_data.text,'lxml')
print(soup.title)

labels = soup.select('h2["class"="ContentItem-title"] > a')#只是个小示例，把你所打开页面的前10个标题找到
for label,num in zip(labels,range(1,100)):
    print(str(num)+":"+label.text.strip())
    

###############################################
# 模拟登陆知乎
# 基于李佳龙的代码（原代码不能运行），修改后还是半成品
# 没有解决二维码识别问题
import urllib.request
import urllib
import re
import http.cookiejar

#登陆地址 使用的是手机登陆
posturl = 'https://www.zhihu.com/login/phone_num'
#伪造的头信息  user-agent and  referer
headers={
    'User-Agent':'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_4) '
                 'AppleWebKit/537.36 (KHTML, like Gecko) '
                 'Chrome/56.0.2924.87 Safari/537.36',
    'Referer':'https://www.zhihu.com/'
}
#初始化一个CookieJar来处理Cookie
cookieJar=http.cookiejar.CookieJar()
cookie_support = urllib.request.HTTPCookieProcessor(cookieJar)
#实例化一个全局opener
opener=urllib.request.build_opener(cookie_support)
#获取参数_xsrf

#post 传递的值 form-data
#post地址在phone_num
#form data
#自动获取xsrf
def get_xsrf():
    req = opener.open(posturl)
    html = req.read().decode('utf-8')
    get_xsrf_pattern = re.compile(r'<input type="hidden" name="_xsrf" value="(.*?)"')
    #这里会返回多个值，不过都是一样的内容，取第一个
    _xsrf = re.findall(get_xsrf_pattern, html)[0]
    #print _xsrf
    return _xsrf

value = {
    'password':'yourpassword',
    'captcha_type':'cn',
    'phone_num':'yourphone',
    '_xsrf':get_xsrf()
}

data=urllib.parse.urlencode(value)
# 注意编码格式
binary_data = data.encode("utf-8") #http://stackoverflow.com/questions/5440485/typeerror-post-data-should-be-bytes-or-an-iterable-of-bytes-it-cannot-be-str
request =urllib.request.Request(posturl, binary_data, headers)

result=opener.open(request)
print(result.read())

# 返回的内容，验证码无效
# 使用前端工具查看返回代码的意思
# 如何解决动态码的问题呢？教程：http://www.cnblogs.com/beer/p/5672678.html

###############################################
#将动态码下载本地，手工输入
#https://segmentfault.com/a/1190000005778518
import requests,time
from bs4 import BeautifulSoup
def get_captcha(data):
    with open('captcha.gif','wb') as fp:
        fp.write(data)
        fp.close()
        im = Image.open('captcha.gif')
        im.show()
        im.close()
    return input('输入验证码：')

def login(username,password,oncaptcha):
    sessiona = requests.Session()
    headers = {'User-Agent':'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:47.0) Gecko/20100101 Firefox/47.0'}
    _xsrf = BeautifulSoup(sessiona.get('https://www.zhihu.com/#signin',headers=headers).content,'html.parser').find('input',attrs={'name':'_xsrf'}).get('value')
    captcha_content = sessiona.get('https://www.zhihu.com/captcha.gif?r=%d&type=login'%(time.time()*1000),headers=headers).content
    data = {
        "_xsrf":_xsrf,
        "phone_num":username,
        "password":password,
        "captcha":oncaptcha(captcha_content)
    }
    resp = sessiona.post('https://www.zhihu.com/login/phone_num',data,headers=headers).content
    print(resp)
    return resp 

if __name__ == "__main__":
    login("yournumber","yourpassword",get_captcha)
    
    
#########################################
#成型的解决方案
#各种可能的情况都要考虑到
#https://github.com/xchaoinfo/fuck-login
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
'''
Required
- requests (必须)
- pillow (可选)
Info
- author : "xchaoinfo"
- email  : "xchaoinfo@qq.com"
- date   : "2016.2.4"
Update
- name   : "wangmengcn"
- email  : "eclipse_sv@163.com"
- date   : "2016.4.21"
'''
import requests
try:
    import cookielib
except:
    import http.cookiejar as cookielib
import re
import time
import os.path
try:
    from PIL import Image
except:
    pass


# 构造 Request headers
agent = 'Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Mobile Safari/537.36'
headers = {
    "Host": "www.zhihu.com",
    "Referer": "https://www.zhihu.com/",
    'User-Agent': agent
}

# 使用登录cookie信息
session = requests.session()
session.cookies = cookielib.LWPCookieJar(filename='cookies')
try:
    session.cookies.load(ignore_discard=True)
except:
    print("Cookie 未能加载")


def get_xsrf():
    '''_xsrf 是一个动态变化的参数'''
    index_url = 'https://www.zhihu.com'
    # 获取登录时需要用到的_xsrf
    index_page = session.get(index_url, headers=headers)
    html = index_page.text
    pattern = r'name="_xsrf" value="(.*?)"'
    # 这里的_xsrf 返回的是一个list
    _xsrf = re.findall(pattern, html)
    return _xsrf[0]


# 获取验证码
def get_captcha():
    t = str(int(time.time() * 1000))
    captcha_url = 'https://www.zhihu.com/captcha.gif?r=' + t + "&type=login"
    r = session.get(captcha_url, headers=headers)
    with open('captcha.jpg', 'wb') as f:
        f.write(r.content)
        f.close()
    # 用pillow 的 Image 显示验证码
    # 如果没有安装 pillow 到源代码所在的目录去找到验证码然后手动输入
    try:
        im = Image.open('captcha.jpg')
        im.show()
        im.close()
    except:
        print(u'请到 %s 目录找到captcha.jpg 手动输入' % os.path.abspath('captcha.jpg'))
    captcha = input("please input the captcha\n>")
    return captcha


def isLogin():
    # 通过查看用户个人信息来判断是否已经登录
    url = "https://www.zhihu.com/settings/profile"
    login_code = session.get(url, headers=headers, allow_redirects=False).status_code
    if login_code == 200:
        return True
    else:
        return False


def login(secret, account):
    _xsrf = get_xsrf()
    headers["X-Xsrftoken"] = _xsrf
    headers["X-Requested-With"] = "XMLHttpRequest"
    # 通过输入的用户名判断是否是手机号
    if re.match(r"^1\d{10}$", account):
        print("手机号登录 \n")
        post_url = 'https://www.zhihu.com/login/phone_num'
        postdata = {
            '_xsrf': _xsrf,
            'password': secret,
            'phone_num': account
        }
    else:
        if "@" in account:
            print("邮箱登录 \n")
        else:
            print("你的账号输入有问题，请重新登录")
            return 0
        post_url = 'https://www.zhihu.com/login/email'
        postdata = {
            '_xsrf': _xsrf,
            'password': secret,
            'email': account
        }
    # 不需要验证码直接登录成功
    login_page = session.post(post_url, data=postdata, headers=headers)
    login_code = login_page.json()
    if login_code['r'] == 1:
        # 不输入验证码登录失败
        # 使用需要输入验证码的方式登录
        postdata["captcha"] = get_captcha()
        login_page = session.post(post_url, data=postdata, headers=headers)
        login_code = login_page.json()
        print(login_code['msg'])
    # 保存 cookies 到文件，
    # 下次可以使用 cookie 直接登录，不需要输入账号和密码
    session.cookies.save()

try:
    input = raw_input
except:
    pass

# 登录之后抓取内容
## 將main的問題列表輸出在shell上面
def  getPageQuestion(url2):  
  mainpage = session.get(url2, headers=headers)  
  # 打印整个网页
  #with open('/Users/liding/temp/a.html','w',encoding='utf-8') as f:
  #    f.write(mainpage.text)        
  #print("snatch successfully.") 
  soup=BeautifulSoup(mainpage.text,'html.parser') 
  tags=soup.find_all("a",class_="question_link")
  #print tags
  for tag in tags:
    print (tag.string)

# 將main頁面上面的問題的回答的摘要輸出在shell上面
def getPageAnswerAbstract(url2):
    mainpage=session.get(url2,headers=headers)
    soup=BeautifulSoup(mainpage.text,'html.parser')
    tags=soup.find_all('div',class_='zh-summary summary clearfix')
    for tag in tags:
       # print tag
        print (tag.get_text())
        print ('詳細內容的鏈接 ： ',tag.find('a').get('href'))

#登录
if __name__ == '__main__':
    if isLogin():
        print('您已经登录')

    else:
        account = input('请输入你的用户名\n>  ')
        secret = input("请输入你的密码\n>  ")
        login(secret, account)

#登陆后获取内容  
#参考:http://www.jianshu.com/p/2577e5bcbf05   
url2='https://www.zhihu.com'
getPageQuestion(url2)
getPageAnswerAbstract(url2)



#利用Scrapy框架 示范
#说明http://www.jianshu.com/p/dfd797a9a329
#第一步：创建工程
#第二步：设定Item
#第三部：编写爬取代码
#进入工程文件夹、启动工程


