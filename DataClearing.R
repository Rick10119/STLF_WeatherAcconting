# 从Excel读取数据 -------------------------

rm(list=ls())# claering
library(rJava)
library(xlsx)
library(lattice)
library(grid)
library(DMwR)
library(nnet)
#加载时间序列程序包
library(tseries)
library(forecast)
library(e1071)
library(ggplot2)
library(AMORE)

filein = 'STLF_DATA_IN_1.xls'
# 读取sheet1,负荷数据到data1
data1 <- read.xlsx2(filein ,1, header=FALSE)
ndays = length(data1[,1]) # 记录总的日数(包括带预测日)


# 读取sheet2,气象数据
data2 <- read.xlsx2(filein ,2, header=FALSE, encoding="UTF-8")#
# 读取气象信息到data2
data2$encoding <- NULL #去掉最后的encoding



# 清洗负荷的坏数据----------------------------------------------------------

# 原始数据 load0
load0 = data1[(1:ndays-1),]
# 拷贝原始数据中的负荷，建立load1
load1 = matrix(0 ,ndays-1,96)
# 如果是新数据这里要改

# 检测空值,先取成NA，表示缺省
for(i in c(1: (ndays-1))){
  for(j in c(1:96)){
    if(load0[i,(j+1)]==''){
      load1[i,j] = NA 
    }
    else{
      load1[i,j] = as.numeric(as.character(load0[i,(j+1)]))
    }#涉及数据类型的转换
  }
}
# 缺省的值取周围8个数的平均数，填充后建立load2
load2 <- knnImputation(load1,k=8,meth='mean') 
# 把数据矩阵转换为序列 load3
load3 = matrix(matrix(load2,96,byrow = T))
npoint = length(load3)#点的个数

#作这个时间序列的图,通过图作一个直观判断
linshi = 14*96*2

load_temp = tail(load3,linshi)
fanwei = c(1:linshi)
p1=plot( load_temp,
        main="负荷曲线（清洗前）",
        xlab="时间",
        ylab="负荷",
        type="l",
        col="red")
# 可以发现存在坏数据
anders = load_temp[c(2:linshi)] - load_temp[c(1:linshi-1)]
hist(abs(anders),
     main="负荷变化量（清洗前）",
     xlab="负荷变化绝对值",
     ylab="出现频率")

# 把爬升过快的数据清洗,形成 load4

load4 = load3
anders2 = load4[c(2:npoint)] - load4[c(1:(npoint-1))]
for(i in c(2:(npoint-1))){
  if (abs(anders2[i])>500){
    load4[i] = NA
  }
}

load4_1 = matrix(load4,96)
load4_1 <- knnImputation(load4_1,k=2,meth='mean') 
load4 = matrix(load4_1)

# 再观察数据
load_temp = tail(load4,linshi)
anders = load_temp[c(2:linshi)] - load_temp[c(1:linshi-1)]
hist(abs(anders),
     main="负荷变化量（清洗后）",
     xlab="负荷变化绝对值",
     ylab="出现频率")



# 最后，把不需要的数据放空
# load0 = NULL
# load1= NULL
# load2= NULL
# load3= NULL
# load4_1= NULL

# 天气表格的整理 --------------------------------------------------------------------

name_weather =  c('风速', '风向', '降雨量', '平均温度',
                  '湿度', '天气类型', '最低温度', '最高温度')
# 创建表来记录天气,读入原excel中的日期信息
date = as.numeric(as.character(data1$X1))#日期
weather_table = data.frame('日期'=date  ,'风速'=0, '风向'=0,
                           '降雨量'=0, '平均温度'=0,
                           '湿度'=0, '天气类型'=0, 
                           '最低温度'=0, '最高温度'=0)
# 修改列名字
names(weather_table) = c('日期',name_weather)

# 读入excel中的天气信息到weather（已经读到data2)
weather = data2
# 修改weather的列名字
names(weather) = c('日期','指标名称','指标数值')
# 转化格式 为char和num
weather$日期   = as.numeric(as.character(data2$X1))
weather$指标名称   = as.character(data2$X2)
weather$指标数值   = as.numeric(as.character(data2$X3))

# 整理表格信息，建立新表weather_table
for(zhibiao in c(1:8)){#对于每个气象指标
  #找到指标对应的行数
  nrow_zhibiao = c(which(weather$指标名称==name_weather[zhibiao]))
  for(nrow in nrow_zhibiao){
    # 读取该行该指标数值和日期值
    shuzhi =  weather[nrow,3]
    riqi_new = weather[nrow,1]
    # 在新表中找到日期对应的行数
    nrow_new = which(weather_table$日期 == riqi_new )
    # 在新表中存入数值
    weather_table[nrow_new,zhibiao+1] = shuzhi
  }
}


# 天气信息清洗 -----------------------------------------------------------
# 拷贝，创建新表格tweather_table2,处理坏数据
weather_table2 = weather_table
# 遇到缺省或数据为0则取前一天的数据
for(i in c(2: 1982)){
  for(j in c(2:9)){
    if(weather_table2[i,j]==0||is.na(weather_table2[i,j])){
      weather_table2[i,j] = weather_table2[i-1,j]
    }
  }
}
hist(weather_table2$最低温度,
     main="最低温度分布（清洗前）",
     xlab="最低温度",
     ylab="出现频率")
# 把极端的数据（首尾3%）取为前一天的数据

for(i in c(2:ndays)){
  for(j in c(2:9)){
    if (weather_table2[i,j]>quantile(weather_table2[,j],0.97)||
        weather_table2[i,j]<quantile(weather_table2[,j],0.03)){
      weather_table2[i,j] = weather_table2[i-1,j]
    }
  }
}
hist(weather_table2$最低温度,
     main="最低温度分布（清洗后）",
     xlab="最低温度",
     ylab="出现频率")
# 舍去风向风速
weather_factors = matrix(c(weather_table2$平均温度,
                           weather_table2$湿度,
                           weather_table2$天气类型,
                           weather_table2$最低温度,
                           weather_table2$最高温度),1983)
# 标准化化处理
norm.data <- scale(weather_factors) 

# 最后，把不需要的表格放空
# weather_factors = NULL
# weather_table = NULL
# weather_table2 =NULL
# weather =NULL