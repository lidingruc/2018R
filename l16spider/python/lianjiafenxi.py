
import pandas as pd
from pyecharts import Pie, Bar,Line,Overlap

# 租房数据分析

f = open(r'/Users/liding/E/Bdata/ptemp/链家北京租房1.txt')
df = pd.read_csv(f,sep=',',header=None,encoding='utf-8',
	names=['area','title','room_type','square','position','detail_place','floor','total_floor','price','house_year'])
print(df.describe)


#各个区域房屋数量均价

area = df.groupby(['area'])

house_com = area['price'].agg(['mean','count'])

house_com.reset_index(inplace=True)

area_main = house_com.sort_values('count',ascending=False)[0:20]


attr = area_main['area']

v1 = area_main['count']

v2 = area_main['mean']



line = Line("北京各区房租均价")

line.add("城区",attr,v2,is_stack=True,xaxis_rotate=30,yaxix_min=4.2,

    mark_point=['min','max'],xaxis_interval=0,line_color='lightblue',

    line_width=4,mark_point_textcolor='black',mark_point_color='lightblue',

    is_splitline_show=False)



bar = Bar("北京各区房屋数量")

bar.add("城区",attr,v1,is_stack=True,xaxis_rotate=30,yaxix_min=4.2,

    xaxis_interval=0,is_splitline_show=False)



overlap = Overlap()

overlap.add(bar)

overlap.add(line,yaxis_index=1,is_add_yaxis=True)
overlap.render('北京各区房屋数量_租金价格.html')


#北京路段_房屋均价分布图

detail_place = df.groupby(['detail_place'])

house_com = detail_place['price'].agg(['mean','count'])

house_com.reset_index(inplace=True)

detail_place_main = house_com.sort_values('count',ascending=False)[0:20]



attr = detail_place_main['detail_place']

v1 = detail_place_main['count']

v2 = detail_place_main['mean']



line = Line("北京主要路段房租均价")

line.add("路段",attr,v2,is_stack=True,xaxis_rotate=30,yaxix_min=4.2,

    mark_point=['min','max'],xaxis_interval=0,line_color='lightblue',

    line_width=4,mark_point_textcolor='black',mark_point_color='lightblue',

    is_splitline_show=False)



bar = Bar("北京主要路段房屋数量")

bar.add("路段",attr,v1,is_stack=True,xaxis_rotate=30,yaxix_min=4.2,

    xaxis_interval=0,is_splitline_show=False)



overlap = Overlap()

overlap.add(bar)

overlap.add(line,yaxis_index=1,is_add_yaxis=True)
overlap.render('主要路段房屋数量_价格.html')


#房源价格区间分布图

price_info = df[['area', 'price']]


#对价格分区

bins = [0,1000,1500,2000,2500,3000,4000,5000,6000,8000,10000]

level = ['0-1000','1000-1500', '1500-2000', '2000-3000', '3000-4000', '4000-5000', '5000-6000', '6000-8000', '8000-1000','10000以上']

price_stage = pd.cut(price_info['price'], bins = bins,labels = level).value_counts().sort_index()



attr = price_stage.index

v1 = price_stage.values



bar = Bar("价格区间&房源数量分布")

bar.add("",attr,v1,is_stack=True,xaxis_rotate=30,yaxix_min=4.2,

    xaxis_interval=0,is_splitline_show=False)



overlap = Overlap()

overlap.add(bar)



overlap.render('价格区间&房源数量分布.html')

#房屋面积分布

bins =[0,30,60,90,120,150,200,300,400,700]

level = ['0-30', '30-60', '60-90', '90-120', '120-150', '150-200', '200-300','300-400','400+']

df['square_level'] = pd.cut(df['square'],bins = bins,labels = level)



df_digit= df[['area', 'room_type', 'square', 'position', 'total_floor', 'floor', 'house_year', 'price', 'square_level']]

s = df_digit['square_level'].value_counts()



attr = s.index

v1 = s.values



pie = Pie("房屋面积分布",title_pos='center')



pie.add(

    "",

    attr,

    v1,

    radius=[40, 75],

    label_text_color=None,

    is_label_show=True,

    legend_orient="vertical",

    legend_pos="left",

)



overlap = Overlap()

overlap.add(pie)

overlap.render('房屋面积分布.html')



#房屋面积&价位分布

bins =[0,30,60,90,120,150,200,300,400,700]

level = ['0-30', '30-60', '60-90', '90-120', '120-150', '150-200', '200-300','300-400','400+']

df['square_level'] = pd.cut(df['square'],bins = bins,labels = level)



df_digit= df[['area', 'room_type', 'square', 'position', 'total_floor', 'floor', 'house_year', 'price', 'square_level']]



square = df_digit[['square_level','price']]

prices = square.groupby('square_level').mean().reset_index()

amount = square.groupby('square_level').count().reset_index()



attr = prices['square_level']

v1 = prices['price']



pie = Bar("房屋面积&价位分布")

pie.add("", attr, v1, is_label_show=True)

pie.render()

bar = Bar("房屋面积&价位分布")

bar.add("",attr,v1,is_stack=True,xaxis_rotate=30,yaxix_min=4.2,

    xaxis_interval=0,is_splitline_show=False)



overlap = Overlap()

overlap.add(bar)

overlap.render('房屋面积&价位分布.html')





overlap.render('北京路段_房屋均价分布图.html')