# 主程序和脚本功能说明（R语言）

# 主程序 
Main.R

# 数据预处理，形成负荷时间序列load4
DataClearing.R

# 气象影响 --------------------------------------------------------------------
# 生成天气模型训练集
WeatherTrainingData.R

# 训练天气模型
WeatherModel.R

# 生成气象影响修正fore2


# 生成AMIMA预测fore1 -------------------------------------------------------------
days = 13
#训练集load5(前13天的数据）
#预测 96个点

# ARIMA预测+气象修正
fore = fore1 + fore2

# 写到excel中
Writing.R

---------------------------------------------------------------------------------------------

使用方法：
将
Main.R
DataClearing.R
WeatherTrainingData.R
WeatherModel.R
Writing.R
拷贝到文件所在地址，运行Main.R即可，
亲测有效...

P.S. 天气训练模型需要跑大概15分钟，用了滚动训练


吕睿可
2020年6月3日



