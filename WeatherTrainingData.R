
# # 测试选择训练集长度 ---------------------------------------------------------------
# # acr1 = 0*c(1:50)
# for (days in c(12:15)){
#   ntotal = 96*days
#   ntest = 96 #这里正式预测时改为0
#   ntrain = ntotal - ntest
#   #可以拆掉最后一天来做样本的训练集load5，总共的是load6
#   load6 <- ts(as.vector(tail(load4,ntotal)),frequency=96,start=c(0,1))
#   load5 <- ts(load6[1:ntrain],frequency=96,start=c(0,1))
#   # 形成arima模型
#   fit1 = auto.arima(load5)
#   
#   #预测 96个点
#   f.p1<-forecast(fit1,h=96,level=c(99.5))
#   # 计算现在的准确度
#   fore1 = as.numeric(f.p1$mean)
#   real0 = as.numeric(load6)
#   real1 = c(real0[(ntrain+1):ntotal])
#   dz0 = fore1-real1
#   acr1[days] = 100 - 100*(sum((dz0^2/real1^2))/96)^0.5
#   
# }
# days =13
# a0 = matrix(acr1,10)
# plot(acr1[c(4:50)],
#      main="ARIMA模型预测精度随训练集天数变化",
#      xlab="训练集包含天数",
#      ylab="预测精度",
#      type = 'l')

#选取最近n个星期的数据
# training - ARIMA----------------------------------------------------------------
# 选择用作arima训练集的周数nweeks,先预测6月4号
nweeks = 2
days = 7*nweeks 

# 滚动训练，形成误差集合 -------------------------------------------------------------

ntotal = 96*days
ntest = 96 
ntrain = ntotal - ntest
dz = 0*matrix(NA,30,96)
for(nroll in c(1:30)){# 滚动次数,从预测6月4号开始滚
  # 训练集有关点个数
  load7 = load4[1:(npoint-nroll*96)]
  #可以拆掉最后一天来做样本的训练集load5，总共的是load6
  load6 <- ts(as.vector(tail(load7,ntotal)),frequency=96,start=c(0,1))
  load5 <- ts(load6[1:ntrain],frequency=96,start=c(0,1))
  # 形成arima模型
  fit1 = auto.arima(load5)
  #预测 96个点
  f.p1<-forecast(fit1,h=96,level=c(99.5))
  # 计算现在的准确度
  fore1 = as.numeric(f.p1$mean)#预测值
  real1 = as.numeric(load6)#真实值
  #测试集真实值
  dm = c(real1[(ntrain+1):ntotal])
  #预测值减真实值
  dz[31-nroll,] = fore1-dm
  
}


