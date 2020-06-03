
# 天气因素 --------------------------------------------------------------------

# 认为ARIMA的误差是天气带来的，研究与天气因素的关系
bias = dz# 误差集
# 各气象因素
x = norm.data[(ndays - 31):(ndays-2),]
new_data = norm.data[ndays-1,]
x1 = norm.data[(ndays - 31):(ndays-2),1]
x2 = norm.data[(ndays - 31):(ndays-2),2]
x3 = norm.data[(ndays - 31):(ndays-2),3]
x4 = norm.data[(ndays - 31):(ndays-2),4]
x5 = norm.data[(ndays - 31):(ndays-2),5]



# nnet预测 ------------------------------------------------------------------
# x = norm.data[(ndays - 31):(ndays-2),c(2,3,5)]
# bias_weather2 = 0*c(1:96)
# 
# for(i in c(1:96)){
#   #使用nnet命令
#   nn <- nnet(x, bias[,i], size = 10, decay = 0.01, 
#              maxit = 1000, linout = T, trace = T) 
#   #利用模型进行预测 
#   bias_weather2[i]<- predict(nn, norm.data[ndays,c(2,3,5)]) 
# }
# 
# fore2 = fore1 + bias_weather2
# dz2 = fore2 - real1
# acr2 = 100 - 100*(sum((dz2^2/real1^2))/96)^0.5
# acr2
# plot(dz0,type = 'l',col ='red',
#      main="气象因素影响预测情况(6月4日）",
#      xlab="时刻点",
#      ylab="偏差")
# lines(-bias_weather2,col='green')
# # lines(dz3,col="red")
# legend("topright",                                    #图例位置为右上角
#        legend=c("实际偏差","预测偏差(nnt)"),        #图例内容
#        col=c("red","green"),                 #图例颜色
#        lty=1,lwd=2)            
# 线性回归 --------------------------------------------------------------------

bias_weather3 = 0*c(1:96)
for(i in c(1:96)){
  y = bias[,i]
  f =  y ~  x3 + x5 + x3^3 + x5^2 + x2 +x2^2  + x3*x5
  fit2 <- lm( f )
  bias_weather3[i]<- predict(fit2,data.frame(X2=norm.data[ndays,c(2,3,5)]))
}

# fore3 = fore1 + bias_weather3
# dz3 = fore3 - real1
# acr3 = 100 - 100*(sum((dz3^2/real1^2))/96)^0.5
# acr3

# plot(dz0,type = 'l',col ='red',
#      main="气象因素影响预测情况(6月4日）",
#      xlab="时刻点",
#      ylab="偏差")
# lines(-bias_weather3,col='green')
# # lines(dz3,col="red")
# legend("topright",                                    #图例位置为右上角
#         legend=c("实际偏差","预测偏差(lm)"),        #图例内容
#         col=c("red","green"),                 #图例颜色
#         lty=1,lwd=2)                                          #图例大小
# svm ---------------------------------------------------------------------
# x = norm.data[(ndays - 31):(ndays-2),c(2,3,5)]
# bias_weather4 = 0*c(1:96)
# 
# for(i in c(1:96)){
#   
#   y = bias[,i]
#   fit4 <- svm(x,y)
#   pred <- predict(fit4,norm.data[(ndays - days +1):(ndays-1),c(2,3,5)])
#   bias_weather4[i] = pred[days-1]
#   # points(pred, col = 4)
# }

# fore4 = fore1 + bias_weather4
# dz4 = fore4 - real1
# acr4 = 100 - 100*(sum((dz4^2/real1^2))/96)^0.5
# acr4

# plot(dz0,type = 'l',col ='red',
#      main="气象因素影响预测情况(6月4日）",
#      xlab="时刻点",
#      ylab="偏差")
# lines(-bias_weather4,col='green')
# # lines(dz3,col="red")
# legend("topright",                                    #图例位置为右上角
#        legend=c("实际偏差","预测偏差(svm)"),        #图例内容
#        col=c("red","green"),                 #图例颜色
#        lty=1,lwd=2)               
# fenxi  ------------------------------------------------------------------
# bb = data.frame('最初偏差'= dz0,
#                 'NNET预测偏差'=-bias_weather2,
#                 '线性预测偏差'=-bias_weather3,
#                 'svm预测偏差'=-bias_weather4)
# par(mfrow = c(1, 3))
# p1=plot(t, pc[2, ]+pc[1,],
#         main="??????????",
#         xlab="ʱ??/h",
#         ylab="???繦??/??kW",
#         type="l",
#         col="red")
# p1 = plot(dz0,-bias_weather2, 
#           xlab="实际偏差",
#           ylab="NNT回归偏差预测")
# p2 = plot(dz0,-bias_weather3,
#           main="气象因素影响(6月4日）",
#           xlab="实际偏差",
#           ylab="多元线性回归偏差预测")
# p3 = plot(dz0,-bias_weather4,
#           xlab="实际偏差",
#           ylab="SXM回归偏差预测")
# 
# #

#  综合模型 -------------------------------------------------------------------
# 
# x = bb[,c(2:4)]
# norm.data <- scale(weather_factors) 
# for(i in c(1:96)){
#   #使用nnet命令
#   nn <- nnet(norm.data[1:(days-1),], bias[,i], size = 10, decay = 0.01, 
#              maxit = 1000, linout = T, trace = T) 
#   #利用模型进行预测 
#   bias_weather2[i]<- predict(nn, norm.data[days,]) 
# }
# 
# fore2 = fore1 + bias_weather2
# dz2 = fore2 - real1
# acr2 = 100 - 100*(sum((dz2^2/real1^2))/16)^0.5