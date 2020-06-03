# 主程序
# 数据预处理，形成符合时间序列load4
source('DataClearing.R', encoding = 'UTF-8', echo=TRUE)
# 气象影响 --------------------------------------------------------------------
# 生成天气模型训练集
source('WeatherTrainingData.R', encoding = 'UTF-8', echo=TRUE)
# 训练天气模型
source('WeatherModel.R', encoding = 'UTF-8', echo=TRUE)
# 生成气象影响修正fore2
fore2 = - bias_weather3

# 生成AMIMA预测fore1 -------------------------------------------------------------
days = 13
ntotal = 96*days
#训练集load5
load5 <- ts(as.vector(tail(load4,ntotal)),frequency=96,start=c(0,1))

# 形成arima模型
fit1 = auto.arima(load5)

#预测 96个点
f.p1<-forecast(fit1,h=96,level=c(99.5))

# 预测值 fore1
fore1 = as.numeric(f.p1$mean)

fore = fore1 + fore2

# writing excel -----------------------------------------------------------
source('Writing.R', encoding = 'UTF-8', echo=TRUE)
